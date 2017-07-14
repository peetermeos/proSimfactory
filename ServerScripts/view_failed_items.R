source("ServerScripts/utils.R")

deploy <- function(){
  title <- "FindFailsRepairs"
  version = "v0.1.3"
  description <- "Fails in past 24 hours in hourly heatmap and summary table"
  inputs = list()
  outputs = list(result = "character")
  
  inject(serviceCode, title, version, description, inputs, outputs)
}

#' Creates summary of failed SFCs in past 24 hrs
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
  
  ##### Helper functions #####
  printLog <- function(s){print(paste(Sys.time(), s, sep = ": "))}

  tagFailsRepairs <- function(df){
    # Tag fails and repairs
    df$failure <- ifelse(
      # If theres LOG_FAILURE that has not been closed
      (is.na(df$CLOSE_FAILURE) & !is.na(df$LOG_FAILURE)) |
        # If there is a fail
        !is.na(df$FAIL),  TRUE, FALSE)
    
    df$repair  <- ifelse(!is.na(df$LOG_REPAIR) &
                           is.na(df$CLOSE_REPAIR),  TRUE, FALSE)
    return(df)
  }
  
  ##### Data import #####    
  printLog("Init")
  printLog("Loading started, getting last 24 hrs")
  
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

  ###### Data Analysis ######
  
  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, drop = TRUE, value.var = "DATE_TIME",
              fun.aggregate = mean, na.rm = TRUE)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  printLog("DCAST successful")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  # Order the dataset by date
  df <- df[order(df$event_end), ]

  # Find fails and repairs
  printLog("Tagging events with LOG_FAILURE and no CLOSE_FAILURE")
  printLog("Tagging events with FAIL")
  df <- tagFailsRepairs(df)
  
  # Now we have fail events, filter them out
  df.fail <- df[df$failure == TRUE, ]
  df.fail$failed.at   <- df.fail$RESRCE
  df.fail$failed.time <- df.fail$event_end
  df.fail <- df.fail[, c("SFC","failed.time", "failed.at")]

  # Take last of every SFC
  df <- df %>% group_by(SFC) %>% filter(event_end == max(event_end))
  # Now we should have a list containing data about last known event of every SFC
  df <- df[!is.na(df$RESRCE),]
  
  # Filter out fails
  df <- df[df$failure == TRUE, ]

  # Add fail times and fail locations
  df <- merge(df, df.fail, by = "SFC")
  
  # For returning the dataset just keep the necessary columns
  df.ret <- df[, c("SFC", "failed.at", "failed.time", "RESRCE", "event_end")] 
  names(df.ret)[4:5] <- c("last.event.at", "last.event.time")
  df.ret$failed.time <- as.POSIXct(df.ret$failed.time)
  df.ret$last.event.time <- as.POSIXct(df.ret$last.event.time)
  df.ret <- df.ret[order(df.ret$failed.time), ]
  
  printLog("Here's a quick summary")
  s <- summary(df.ret)
  print(s)
  
  printLog("Saving dataset")
  write.csv(df.ret, file = "data.csv")

  # Prepare data for plotting
  df$hour <- format(as.POSIXct(df$event_end), format = "%d %b %H:00 ")
  df$count <- 1
  
  df <- aggregate(data = df, count ~ RESRCE  + hour, FUN = "sum", na.rm = TRUE)
  df$hour <- factor(df$hour)
  
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
                          title: 'Failed items by resources and failure times',
                          xaxis: {
                            title: 'Failure time (day hour)'
                          }
                   };
                    myChart = document.getElementById('myChart');
                    Plotly.newPlot(myChart, data, layout);", sep = "");    
   
  ##### Returning dataset ##### 
  printLog("Returning dataset")
  s <- toJSON(list(result = df.ret, plot = plotStr))
  #return(plotStr)
  return(s)
}
