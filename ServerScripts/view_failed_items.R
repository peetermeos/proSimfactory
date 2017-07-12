source("ServerScripts/utils.R")

deploy <- function(){
  title <- "FindFailsRepairs"
  version = "v0.0.1"
  description <- "Find fails and repairs"
  inputs = list()
  outputs = list(result = "character")
  
  inject(serviceCode, title, version, description, inputs, outputs)
}

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
  
  only.fails <- TRUE
  
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
  df <- tagFailsRepairs(df)
  printLog("Tagging fails and repairs successful")
   
  ## Take last of every SFC
  # This bit seems to fail
  df1 <- group_by(df, SFC) 
  df1 <- filter(df1, event_end == max(event_end))
  
  df1 <- df1[!is.na(df1$RESRCE),]
  
  df <- df1 
  
  # Now we should have a list containing data about last known event of every SFC
  # Filter out fails
  if (only.fails) df <- df[df$failure == TRUE, ]
   
  df <- df[order(df$event_end), ]
  #df$waited <- as.numeric(difftime(df$event_end, as.POSIXct(t2), units = "hours"))
  df$waited <- round(as.numeric(difftime(max(df$event_end), df$event_end, units = "hours")), 2)
  
  df <- df[, c("SFC", "OPERATION", "RESRCE", "waited")]
  
  printLog("Saving dataset")
  write.csv(df, file = "data.csv")

  s <- summary(df)
  print(s)
  
  printLog("Creating the plot")
  c <- "["
  first <- TRUE
  for (i in unique(df$RESRCE)) {
    if (first)
      first <- FALSE
    else
      c <- paste(c, ", ", sep = "")
    c <- paste(c, "{y: [", paste(df$waited[df$RESRCE == i], collapse = ","),"]",
               ",x: [", paste(paste("'", df$OPERATION[df$RESRCE == i], "'", sep = ""), collapse = ","),"]",
               ", type: 'box', name: '", i,"', boxpoints: 'all', jitter: 0.3}", sep = "")
  }
  c <- paste(c, "]", sep = "")

  plotStr <- paste("var data = ", c, ";",
                   "var layout = {
                          title: 'Failed item, their last known activities and the time since we heard from them',
                          yaxis: {
                                title: 'Time in queue [h]'
                          },
                          xaxis: {
                            title: 'Operation'
                          },
                          boxmode: 'group'
                   };
                    myChart = document.getElementById('myChart');
                    Plotly.newPlot(myChart, data, layout);", sep = "");    
    
  printLog("Returning dataset")
  s <- toJSON(list(result = data.frame(df[,c(1:4)]), plot = plotStr))
  return(s)
}
