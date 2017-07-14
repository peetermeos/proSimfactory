source("ServerScripts/utils.R")

metadata <- list(
  title <- "DailyWorkDistribution",
  version = "v0.0.2",
  description <- "Distribution of work between resources in past 24 hours",
  inputs = list(),
  outputs = list(result = "character")
)

#' Title
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
  
  load("c:/Temp/2017-01-18.RData")
  df <- df.ods
  
  # t2 <- as.POSIXct(Sys.Date()) 
  # t1 <- t2 - 3600 * 24 
  # 
  # t1 <- format(t1, "%Y-%m-%d")
  # t2 <- format(t2, "%Y-%m-%d")
  # 
  # sql <- list()
  # sql$db.name <- "SAPMEWIP"
  # sql$host.name <- "eeel163.encnet.ead.ems"
  # sql$driver.name <- "SQL Server"
  # sql$port <- ""
  # sql$user.name <- "proekspert"
  # sql$pwd <- "proekspert1!"
  # 
  # s.odbc <- paste("DRIVER=", sql$driver.name,
  #                 ";Database=", sql$db.name,
  #                 ";Server=", sql$host.name,
  #                 ";Port=", sql$port,
  #                 ";PROTOCOL=TCPIP",
  #                 ";UID=", sql$user.name,
  #                 ";PWD=", sql$pwd,
  #                 ";TDS_Version=8.0", sep = "")  
  # db <- odbcDriverConnect(s.odbc)
  # 
  # df <- sqlQuery(db, paste("SELECT SFC, ACTION_CODE, DATE_TIME, SFC, OPERATION, ITEM, ITEM_REVISION, 
  #                           ROUTER, ROUTER_REVISION, STEP_ID, RESRCE, SHOP_ORDER_BO, PARTITION_DATE
  #                           FROM dbo.ACTIVITY_LOG
  #                           WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep = ""))
  # 
  # odbcClose(db)
  # if (nrow(df) > 2) 
  #   printLog("Loading successful")
  # else{
  #   printLog("Loading failed")
  #   stop()
  # }

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
  
  # df$waited <- round(as.numeric(difftime(max(df$event_end), df$event_end, units = "hours")),2)
  
  df <- df[, c("SFC", "OPERATION", "RESRCE", "event_end")]
  names(df)[names(df) %in% "event_end"] <- "time"
  
  printLog("Creating plot")
  c <- "["
  first <- TRUE
  for (i in unique(df$RESRCE)) {
    if (first)
      first <- FALSE
    else
      c <- paste(c, ", ", sep = "")
      c <- paste(c, "{y: [", paste(df$time[df$RESRCE == i], collapse = ","),"]",
                 ",x: [", paste(paste("'", df$OPERATION[df$RESRCE == i], "'", sep = ""), collapse = ","),"]",
                 ", type: 'box', name: '", i,"', boxpoints: 'all', jitter: 0.4, marker: {opacity: 0.75}}", sep = "")
  }
  c <- paste(c, "]", sep = "")

  plotStr <- paste("var data = ", c, ";",
                   "var layout = {
                          yaxis: {
                                title: 'Time'
                          },
                          xaxis: {
                            title: 'Operation'
                          },
                          boxmode: 'group'
                   };
                    myChart = document.getElementById('myChart');
                    Plotly.newPlot(myChart, data, layout);",
                   sep =  "");

  printLog("Returning dataset")
  write.table(plotStr, file="text.txt")
  s <- toJSON(list(result = data.frame(), plot = plotStr))
  return(s)
}




