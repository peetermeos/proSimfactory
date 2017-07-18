source("ServerScripts/utils.R")

metadata <- list(
  title = "DailyComponentFlow",
  version = "v0.0.4",
  description =  "SFC workflow for past 24 hrs",
  inputs = list(),
  outputs = list(result = "character")
)

#' Analyses and summarises daily component flow through shop floor.
#' @version v0.0.4
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

  df <- sqlQuery(db, paste("SELECT SFC, ACTION_CODE, DATE_TIME, SFC, OPERATION, ITEM, ITEM_REVISION,
                            ROUTER, ROUTER_REVISION, STEP_ID, RESRCE, SHOP_ORDER_BO, PARTITION_DATE
                            FROM dbo.ACTIVITY_LOG
                            WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep = ""))

  odbcClose(db)
  if (nrow(df) > 2)
    printLog("Loading successful")
  else{
    printLog("Loading failed")
    stop()
  }

  df <- df[!is.na(df$RESRCE),]
  df <- df[!is.na(df$OPERATION),]
  
  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, drop = TRUE, value.var = "DATE_TIME",
              fun.aggregate = mean, na.rm = TRUE)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  
  printLog("DCAST successful")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  df <- df[, c("SFC", "OPERATION", "RESRCE", "event_end")]
  names(df)[names(df) %in% "event_end"] <- "time"
  
  # Prepare data for plotting
  df$hour <- format(as.POSIXct(df$time), format = "%d %b %H:00 ")
  df$count <- 1
  
  df <- aggregate(data = df, count ~ RESRCE  + hour, FUN = "sum", na.rm = TRUE)
  df$hour <- factor(df$hour)
  
  # Order levels alphabetically
  df$RESRCE <- factor(df$RESRCE, levels = levels(df$RESRCE)[order(levels(df$RESRCE))])
  
  lr <- levels(df$RESRCE)
  lh <- levels(df$hour)
  
  ##### Creating a plot ######
  printLog("Creating the plot")
  c <- "["
  
  c <- paste(c, "{y: ['", paste(lr, collapse = "','"),"']",
             ",x: ['", paste(lh,   collapse = "','"),"']",
             ",z: [", sep = "")
  
  for (i in lr){
    c <- paste(c, "[", sep = "")
    for(j in lh){
      n <- df$count[df$RESRCE == i & df$hour == j]
      if (length(df$count[df$RESRCE == i & df$hour == j]) == 0)
        n <- 0
      
      c <- paste(c, n, sep = "")
      if (j != lh[length(lh)]) c <- paste(c, ",", sep = "")
    }
    
    c <- paste(c, "]", sep = "")
    
    if (i != lr[length(lr)])
      c <- paste(c, ",", sep = "")
    
  }
  
  c <- paste(c, "]", ", type: 'heatmap'}]", sep = "")
  
  plotStr <- paste("var data = ", c, ";",
                   "var layout = {
                          title: 'SFC flow in past 24 hrs',
                          xaxis: {
                            title: 'Time (day hour)'
                          }
                   };
                    myChart = document.getElementById('myChart');
                    Plotly.newPlot(myChart, data, layout);", sep = "");    
  
  ##### Returning dataset ##### 
  printLog("Returning dataset")
  s <- toJSON(list(result = data.frame(), plot = plotStr), na = "string", null = "list")
  return(s)
}




