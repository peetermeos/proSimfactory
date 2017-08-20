source("ServerScripts/utils.R")

metadata <- list(
  title = "CycleTimes",
  version = "v0.0.3",
  description =  "Shows cycle times for operations and production batches for past 24 hrs.",
  inputs = list(),
  outputs = list(result = "character")
)

#' Shows cycle times for operations and production batches for past 24 hrs..
#' @version v0.0.3
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
serviceCode <- function(){
  library("RODBC")
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  
  printLog <- function(s){print(paste(Sys.time(), s, sep = ": "))}
  
  printLog("Init")
  printLog("Loading started, getting last 24 hrs")

  t2 <- as.POSIXct(Sys.time())
  t1 <- t2 - 3600 * 24
  
  t1 <- format(t1, "%Y-%m-%d %H:%M:%S")
  t2 <- format(t2, "%Y-%m-%d %H:%M:%S")
  
  sql <- list()
  sql$db.name <- "SAPMEODS"
  sql$host.name <- "eeel164.encnet.ead.ems"
  sql$driver.name <- "SQL Server"
  sql$port <- ""
  sql$user.name <- "proekspert"
  sql$pwd <- "proekspert1!"
  
  s.odbc <- paste("DRIVER=", sql$driver.name,
                  ";Database=", sql$db.name,
                  ";Server=", sql$host.name,
                  ";Port=", sql$port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", sql$user.name,
                  ";PWD=", sql$pwd,
                  ";TDS_Version=8.0", sep = "")
  
  db <- odbcDriverConnect(s.odbc)
  
  
  # Get the production log for past 24 hrs
  df <- sqlQuery(db, paste("SELECT a.SFC, a.OPERATION, a.RESRCE, a.TIMES_PROCESSED, a.PASS1_ELAPSED_TIME,
                                  a.PASS1_ELAPSED_QUEUE_TIME, a.ODS_DATE_TIME,
                                  a.SHOP_ORDER, a.ITEM, a.ITEM_REVISION
                                  FROM dbo.ODS_PRODUCTION_LOG AS a
                                  WHERE a.DATE_TIME >='", t1, "' AND a.DATE_TIME   <= '", t2, "'", sep = ""))

  
  odbcClose(db)
  if (nrow(df) > 2)
    printLog("Loading successful")
  else{
    printLog("Loading failed")
    stop()
  }

  # Now we need to calculate median cycle times for every shop_order and operation
  df$cycle.time <- df$PASS1_ELAPSED_TIME / 1000
  
  df.mean <- aggregate(data = df, cycle.time ~ OPERATION  + SHOP_ORDER + ITEM + ITEM_REVISION, FUN = "mean", na.rm = TRUE)
  names(df.mean)[names(df.mean) %in% "cycle.time"] <- "mean.(sec)"

  df.sd <- aggregate(data = df, cycle.time ~ OPERATION  + SHOP_ORDER + ITEM + ITEM_REVISION, FUN = "sd", na.rm = TRUE)
  names(df.sd)[names(df.sd) %in% "cycle.time"] <- "sd.(sec)"

  # Merge the two descriptives
  df <- merge(df.mean, df.sd, by = c("OPERATION", "SHOP_ORDER", "ITEM", "ITEM_REVISION"))

  # # Order levels alphabetically
  # df$OPERATION <- factor(df$OPERATION, levels = levels(df$OPERATION)[order(levels(df$OPERATION))])
  df$SHOP_ORDER <- factor(df$SHOP_ORDER)
  
  # Now we need for these items historical cycle times.
  
  items <- paste(df$ITEM, collapse = "' OR ITEM='")
  
  printLog("Loading started, historic cycle times")
  db <- odbcDriverConnect(s.odbc)
  df.hist <- sqlQuery(db, paste("SELECT OPERATION, PASS1_ELAPSED_TIME,
                                  ODS_DATE_TIME, ITEM
                                  FROM dbo.ODS_PRODUCTION_LOG 
                                  WHERE DATE_TIME >='2015-01-01' AND (ITEM='", items, "')", sep = "")) 
  odbcClose(db)
  if (nrow(df.hist) > 2)
    printLog("Loading successful")
  else{
    printLog("Loading failed")
    stop()
  }
  
  
  df.hist$cycle.time <- df.hist$PASS1_ELAPSED_TIME / 1000
  df.hist.mean <- aggregate(data = df.hist, cycle.time ~ OPERATION  + ITEM, FUN = "mean", na.rm = TRUE)
  names(df.hist.mean)[names(df.hist.mean) %in% "cycle.time"] <- "mean.(sec)"
  
  df.hist.sd <- aggregate(data = df.hist, cycle.time ~ OPERATION  + ITEM, FUN = "sd", na.rm = TRUE)
  names(df.hist.sd)[names(df.hist.sd) %in% "cycle.time"] <- "sd.(sec)"
  # Merge the two descriptives
  df.hist <- merge(df.hist.mean, df.hist.sd, by = c("OPERATION", "ITEM"))

  ##### Creating a plot ######
  printLog("Creating the plot")
  plotStr <- ""
  plotStr <- "var data = [{";

  plotStr <- paste(plotStr, "x: ['", paste(df$OPERATION, collapse = "','"),"'],", sep = "")
  plotStr <- paste(plotStr, "y: ['item ", paste(df$ITEM, collapse = "','item "),"'],", sep = "")
  plotStr <- paste(plotStr, "mode: 'markers+text', type: 'scatter', name: 'Mean cycle time', 
                   marker: {size: 12}, textposition: 'top center',", sep = "")
  plotStr <- paste(plotStr, "text: [", paste(round(df$`mean.(sec)`, 2), collapse = ","), "]", sep = "")

  plotStr <- paste(plotStr, "}];",
                  "var layout = {title: 'Item mean cycle times over past 24hrs as of  ", t2,
                  "', xaxis: {title: 'Operation'}, yaxis:{title: 'Item'}};",
                  "myChart = document.getElementById('myChart');
                   Plotly.newPlot(myChart, data, layout);", sep = "");   
   
  ##### Returning dataset ##### 
  # df <- dcast(data = df, ITEM~STATUS_DESCRIPTION, fun.aggregate = sum, value.var = "QTY")
  
  printLog("Returning dataset")
  s <- toJSON(list(previous.24h = df, historics = df.hist, plot = plotStr), na = "string", null = "list")
  #return(s)
  }




