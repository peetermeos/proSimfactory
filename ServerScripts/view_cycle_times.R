source("ServerScripts/utils.R")

metadata <- list(
  title = "CycleTimes",
  version = "v0.0.1",
  description =  "Shows cycle times for operations and production batches for past 24 hrs.",
  inputs = list(),
  outputs = list(result = "character")
)

#' Shows cycle times for operations and production batches for past 24 hrs..
#' @version v0.0.1
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
serviceCode <- function(item = ""){
  library("RODBC")
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  
  printLog <- function(s){print(paste(Sys.time(), s, sep = ": "))}
  
  printLog("Init")
  printLog("Loading started, getting last 24 hrs")
  
  # load("c:/Temp/2017-01-18.RData")
  # df <- df.ods
  
  t2 <- as.POSIXct(Sys.time())
  t1 <- t2 - 3600 * 24
  
  t1 <- format(t1, "%Y-%m-%d %H:%M:%S")
  t2 <- format(t2, "%Y-%m-%d %H:%M:%S")
  
  sql <- list()
  sql$db.name <- "SAPMEWIP"
  sql$host.name <- "eeel163.encnet.ead.ems"
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
  
  df <- sqlQuery(db, paste("SELECT a.SFC, a.SHOP_ORDER_BO, a.QTY, a.QTY_DONE, a.QTY_SCRAPPED, 
                                   a.ACTUAL_COMP_DATE, a.MODIFIED_DATE_TIME, b.STATUS_DESCRIPTION,  
                                   ITEM.ITEM 
                            FROM SFC AS a 
                            INNER JOIN STATUS AS b ON a.STATUS_BO = b.HANDLE 
                            INNER JOIN ITEM ON a.ITEM_BO = ITEM.HANDLE 
                            WHERE a.MODIFIED_DATE_TIME >='", t1, " ' 
                              AND a.MODIFIED_DATE_TIME   <= '", t2, "'", sep = ""))
  
  odbcClose(db)
  if (nrow(df) > 2)
    printLog("Loading successful")
  else{
    printLog("Loading failed")
    stop()
  }
  
  if( item != ""){
    df <- df[grep(item, df$ITEM),]
  }

  # Summarise for plotting 
  df <- aggregate(data = df, QTY ~ ITEM  + STATUS_DESCRIPTION, FUN = "sum", na.rm = TRUE)

  # Order levels alphabetically
  df$ITEM <- factor(df$ITEM, levels = levels(df$ITEM)[order(levels(df$ITEM))])
  df$STATUS_DESCRIPTION <- factor(df$STATUS_DESCRIPTION, 
                                  levels = levels(df$STATUS_DESCRIPTION)[order(levels(df$STATUS_DESCRIPTION))])
  
  li <- levels(df$ITEM)
  ls <- levels(df$STATUS_DESCRIPTION)
  
  ##### Creating a plot ######
  printLog("Creating the plot")
  plotStr <- "var data = [";
  
  first.row <- TRUE  
  for (i in li) {
    y <- rep(0, length(li))
    for (j in ls) {
      # Y vector
      y[which(ls == j)] <- ifelse(length(df$QTY[df$ITEM == i & df$STATUS_DESCRIPTION == j]) > 0, 
                                  df$QTY[df$ITEM == i & df$STATUS_DESCRIPTION == j], 0)
    }
    
    if (sum(y) > 0) {
      ifelse(!first.row, plotStr <- paste(plotStr, ", ", sep = ""), first.row <- FALSE)
      
      #plotStr <- paste(plotStr, "{type: 'scatter', model: 'lines+markers', line: {shape: 'spline'},", sep = "")
      plotStr <- paste(plotStr, "{type: 'bar',", sep = "")
      plotStr <- paste(plotStr, "name: '", i, "',", sep = "")
      # Assemble x and y
      plotStr <- paste(plotStr, "x: ['", paste(ls[y > 0], collapse = "','"),"'],", sep = "")
      plotStr <- paste(plotStr, "y: [", paste(y[y > 0], collapse = ","),"]", sep = "")
      
      plotStr <- paste(plotStr, "}", sep = "")
    }
  }
  
  plotStr <- paste(plotStr, "];",
                   #"var layout = {title: 'Item statuses over past 24hrs as of ", t2,"'};",
                   "var layout = {title: 'Item statuses over past 24hrs as of  ", t2,"', barmode: 'stack'};",
                   "myChart = document.getElementById('myChart');
                              Plotly.newPlot(myChart, data, layout);", sep = "");   
  
  ##### Returning dataset ##### 
  df <- dcast(data = df, ITEM~STATUS_DESCRIPTION, fun.aggregate = sum, value.var = "QTY")
  printLog("Returning dataset")
  s <- toJSON(list(result = df, plot = plotStr), na = "string", null = "list")
  return(s)
  }




