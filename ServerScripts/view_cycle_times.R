source("ServerScripts/utils.R")

metadata <- list(
  title = "CycleTimes",
  version = "v0.1.0",
  description =  "Shows cycle times series for operations and production batches for past 24 hrs overlaid by historic data.",
  inputs = list(),
  outputs = list(result = "character")
)

#' Shows cycle times series for operations and production batches for past 24 hrs overlaid by historic data.
#' @version v0.1.0
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
  
  start.date <- "2015-01-01"
  
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
  names(df.sd)[names(df.sd) %in% "cycle.time"] <- "std.dev.(sec)"

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
                                  WHERE DATE_TIME >='", start.date, "' AND (ITEM='", items, "')", sep = "")) 
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
  names(df.hist.sd)[names(df.hist.sd) %in% "cycle.time"] <- "std.dev.(sec)"
  # Merge the two descriptives
  df.hist.rep <- merge(df.hist.mean, df.hist.sd, by = c("OPERATION", "ITEM"))
  
  df.hist$item.op <- paste(df.hist$ITEM, df.hist$OPERATION, sep = "-")
  df.hist$year.mon <- format(as.POSIXct(df.hist$ODS_DATE_TIME), format = "%Y-%m")
  
  df.hist <- aggregate(data = df.hist, cycle.time ~ ITEM + OPERATION + year.mon, FUN = mean, na.rm = TRUE)
  df.hist$item.op <- paste(df.hist$ITEM, df.hist$OPERATION, sep = "-")
  
  items <- unique(df$ITEM)
  ops <- unique(df$OPERATION)

  # ##### Creating a plot ######
  # printLog("Creating the plot")
  # plotStr <- ""
  # plotStr <- "var data = [{";
  # 
  # plotStr <- paste(plotStr, "x: ['", paste(df$OPERATION, collapse = "','"),"'],", sep = "")
  # plotStr <- paste(plotStr, "y: ['item ", paste(df$ITEM, collapse = "','item "),"'],", sep = "")
  # plotStr <- paste(plotStr, "mode: 'markers+text', type: 'scatter', name: 'Mean cycle time', 
  #                  marker: {size: 12}, textposition: 'top center',", sep = "")
  # plotStr <- paste(plotStr, "text: [", paste(round(df$`mean.(sec)`, 2), collapse = ","), "]", sep = "")
  # 
  # plotStr <- paste(plotStr, "}];",
  #                 "var layout = {title: 'Item mean cycle times over past 24hrs as of  ", t2,
  #                 "', xaxis: {title: 'Operation'}, yaxis:{title: 'Item'}};",
  #                 "myChart = document.getElementById('myChart');
  #                  Plotly.newPlot(myChart, data, layout);", sep = "");   
  
  ##### Historical plot #####
  printLog("Creating plot")
  c <- "["
  first <- TRUE
  for (i in unique(df.hist$item.op)) {
    if (first) {
      first <- FALSE
      c <- paste(c, "{ ", sep = "")
    } else {
      c <- paste(c, "}", 
                 ", {", sep = "")
    }
     
    c <- paste(c, "y: [", paste(round(df.hist$cycle.time[df.hist$item.op == i], 2), collapse = ","),"]",
                 ",x: [", paste(paste("'", df.hist$year.mon[df.hist$item.op == i], "'", sep = ""), collapse = ","),"]",
                  ", type: 'scatter', mode: 'lines+markers', name: 'Item ", i,"'", sep = "")
  }
  c <- paste(c, "}]", sep  = "")
   
  c1 <- "" 
  first <- TRUE
  for (i in items) {
    if (first) { 
      first <- FALSE
      c1 <- paste(c1, "{", sep = "")
    } else
      c1 <- paste(c1, ",{", sep = "")
  
    c1 <- paste(c1, "method: 'restyle',", sep = "") 
    c1 <- paste(c1, "label: '", i,"',", sep = "") 
    c1 <- paste(c1, "args: ['visible', [", paste(tolower(df.hist$ITEM == i), collapse = ","), "]]", sep = "")
    
    c1 <- paste(c1, "}", sep = "")
  }

  c2 <- ""
  first <- TRUE
  for (i in ops) {
    if (first) { 
      first <- FALSE
      c2 <- paste(c2, "{", sep = "")
    }
    else
      c2 <- paste(c2, ",{", sep = "")
    
    c2 <- paste(c2, "method: 'restyle',", sep = "") 
    c2 <- paste(c2, "label: '", i,"',", sep = "")
    c2 <- paste(c2, "args: ['visible', [", paste(tolower(df.hist$OPERATION == i), collapse = ","), "]]", sep = "")
    
    c2 <- paste(c2, "}", sep = "")    
  }

  
  plotStr <- paste("var data = ", c, ";",
                   "var layout = {",
                    "updatemenus: [{y: 0.8, yanchor: 'top', buttons: [", c1 ,"]}, 
                                  {y: 0.9, yanchor: 'top', buttons: [", c2 ,"]}],",
                    "yaxis: {title: 'Activity duration (sec)'},
                    xaxis: {title: 'Month'},
                    title: 'Historical operation cycle times for items since ", start.date, "'};
                    
                    myChart = document.getElementById('myChart');
                    Plotly.newPlot(myChart, data, layout);",
                    
                    sep = "");
  
  ##### End of plot #####
  
  printLog("Returning dataset")
  #return(plotStr)
  s <- toJSON(list(previous.24h = df, historics = df.hist.rep, plot = plotStr), na = "string", null = "list")
  return(s)
  }




