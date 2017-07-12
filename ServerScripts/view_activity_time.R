source("ServerScripts/utils.R")

deploy <- function(){
  title <- "dailyActivity"
  version = "v0.0.1"
  description <- "Shop floor activity for past 24 hours"
  inputs = list(operation = "character")
  outputs = list(result = "character")
  
  inject(serviceCode, title, version, description, inputs, outputs)
}

#' Title
#' @version v0.0.1
#' 
#' @param operation 
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#' 
#'
#' @examples
serviceCode <- function(operation = ""){
  library("RODBC")
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  
  print(paste(Sys.time(), "Init", sep=": "))
  print(paste(Sys.time(), "Inputs", sep=": "))
  print(paste(Sys.time(), paste("operation:", operation), sep=": "))
  print(paste(Sys.time(), "Loading started, getting last 24 hrs", sep=": "))
  
  #load("c:/Temp/2017-01-18.RData")
  #df <- df.ods
  
  t2 <- as.POSIXct(Sys.Date()) 
  t1 <- t2 - 3600 * 24 
  
  t1 <- format(t1, "%Y-%m-%d")
  t2 <- format(t2, "%Y-%m-%d")
  
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
                  ";TDS_Version=8.0", sep="")  
  db <- odbcDriverConnect(s.odbc)
  
  df <- sqlQuery(db, paste("SELECT SFC, ACTION_CODE, DATE_TIME, SFC, OPERATION, ITEM, ITEM_REVISION, 
                            ROUTER, ROUTER_REVISION, STEP_ID, RESRCE, SHOP_ORDER_BO, PARTITION_DATE
                            FROM dbo.ACTIVITY_LOG
                            WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))
  
  odbcClose(db)
  if (nrow(df) > 2) 
    print(paste(Sys.time(), "Loading successful", sep=": "))
  else
    stop(paste(Sys.time(), "Loading failed", sep=": "))

  df <- df[!is.na(df$RESRCE),]
  df <- df[!is.na(df$OPERATION),]
    
  if( operation != ""){
    df <- df[df$OPERATION == operation,]
  }

  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, drop = TRUE, value.var = "DATE_TIME",
              fun.aggregate = mean, na.rm = TRUE)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  print(paste(Sys.time(), "DCAST successful", sep=": "))
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  df$duration <- round(as.numeric(difftime(df$event_end, df$START, units = "mins")), 2)
  df <- df[!is.na(df$duration),]
  
  df <- df[, c("SFC", "OPERATION", "RESRCE", "duration")]

  print(paste(Sys.time(), "Creating plot", sep=": "))
  c <- "["
  first <- TRUE
  for(i in unique(df$RESRCE)){
    if (first)
      first <- FALSE
    else
      c <- paste(c, ", ", sep="")
    c <- paste(c, "{y: [", paste(df$duration[df$RESRCE == i], collapse = ","),"]",
               ",x: [", paste(paste("'", df$OPERATION[df$RESRCE == i], "'", sep=""), collapse = ","),"]",
               ", type: 'box', name: '", i,"', boxpoints: 'suspectedoutliers', jitter: 0.1, marker: {size: 2, opacity: 0.75}}", sep="")
  }
  c <- paste(c, "]", sep="")

  plotStr <- paste("var data = ", c, ";",
                   "var layout = {
                          yaxis: {
                            title: 'Activity duration [min]'
                          },
                          xaxis: {
                            title: 'Operation'
                          },
                          boxmode: 'group',
                          title: 'Activity durations by operation'
                          };

                   myChart = document.getElementById('myChart');
                   Plotly.newPlot(myChart, data, layout);",
                   sep= "");

  print(paste(Sys.time(), "Returning dataset", sep=": "))
  
  df1 <- aggregate(data=df, duration ~ RESRCE, FUN = quantile, probs = 0.5, na.rm = TRUE)
  names(df1) <- c("Resource", "MedianDuration[min]")
  df2 <- aggregate(data=df, duration ~ RESRCE, FUN = sd, na.rm = TRUE)
  df2$duration <- round(df2$duration, 2)

  names(df2) <- c("Resource", "StandardDeviation")
  df <- merge(df1, df2, by="Resource")
  
  s <- toJSON(list(result = df, plot = plotStr))
  return(s)
}