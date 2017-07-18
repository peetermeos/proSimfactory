source("ServerScripts/utils.R")

metadata <- list(
  title = "FindFailsRepairs",
  version = "v0.2.0",
  description = paste("Fails in past 24 hours in hourly barchart and summary table.",
                     " Split SFCs are represented as their last known code.", sep = ""),
  inputs = list(),
  outputs = list(result = "character")
)


#' Creates summary of failed SFCs in past 24 hrs. SFCs are represented by their last known code.
#'
#' @return Dataframe of failed items and a plotly plot of failed items.
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
      # If there's LOG_FAILURE that has not been closed
      (is.na(df$CLOSE_FAILURE) & !is.na(df$LOG_FAILURE)) |
        # If there is a fail
        !is.na(df$FAIL),  TRUE, FALSE)
    
    df$repair  <- ifelse(!is.na(df$LOG_REPAIR) &
                           is.na(df$CLOSE_REPAIR),  TRUE, FALSE)
    return(df)
  }
  
  ##### Data import #####    
  printLog("Init")
  printLog("Event log loading started, getting last 24 hrs of events")
  
  # load("tmp.RData")
  
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

  df <- sqlQuery(db, paste("SELECT SFC, ACTION_CODE, DATE_TIME, OPERATION, ITEM, ITEM_REVISION,
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

  printLog("SFC history loading started, getting last 24 hrs of events")

  sql$db.name <- "SAPMEODS"
  sql$host.name <- "eeel164.encnet.ead.ems"

  s.odbc <- paste("DRIVER=", sql$driver.name,
                  ";Database=", sql$db.name,
                  ";Server=", sql$host.name,
                  ";Port=", sql$port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", sql$user.name,
                  ";PWD=", sql$pwd,
                  ";TDS_Version=8.0", sep = "")
  db <- odbcDriverConnect(s.odbc)

  df.link <- sqlQuery(db, paste("SELECT *
             FROM dbo.ODS_SFC_ID_HISTORY_WIP
             WHERE (REASON = 'S' OR REASON = 'P')
             AND DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep = ""))
  odbcClose(db)

  if (nrow(df.link) > 2)
    printLog("Loading successful")
  else{
    printLog("Loading failed")
    stop()
  }
  
  #save(df, df.link, file = "tmp.RData")
  #stop("Download complete.")

  ###### Data Analysis ######
  printLog("Processing SFC parent - child linkages.")
  
  # First we need to go through the SFC history dataset and pick out parent/sibling pairs
  df.link$child <- gsub("^.*,","", as.character(df.link$SFC_BO))
  df.link$child <- factor(df.link$child)
  
  # Take out lines where SFC has mutated into itself
  #df.link <- df.link[as.character(df.link$child) != as.character(df.link$SFC), ]
  
  # Rename SFC to parent
  names(df.link)[names(df.link) %in% "SFC"] <- "parent"
  
  # We use only parent child and datetime
  df.link <- df.link[, c("parent", "child")]
  
  # Now we need to replace all the SFCs that are present in link table.
  df <- merge(df, df.link, by.x = "SFC", by.y = "parent", all.x = TRUE, all.y = TRUE)
  
  #If there's no mapping use old SFC
  df$child <- as.character(df$child)
  df$SFC <- as.character(df$SFC)
  df$child[is.na(df$child)] <- df$SFC[is.na(df$child)]
  
  # We only need child SFCs
  df$SFC <- NULL
  
  # Now take unique rows
  df <- unique(df)
  
  # Rename child to SFC and we are good to go
  names(df)[names(df) %in% "child"] <- "SFC"
  
  ##### Now the rest of the analysis #####
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
  
  # Order levels alphabetically
  df$RESRCE <- factor(df$RESRCE, levels = levels(df$RESRCE)[order(levels(df$RESRCE))])
  
  lr <- levels(df$RESRCE)
  lh <- levels(df$hour)
   
  ##### Creating a plot ######
  printLog("Creating the plot")
  plotStr <- "var data = [";

  first.row <- TRUE  
  for (i in lr) {
    y <- rep(0, length(lh))
    for (j in lh) {
      # Y vector
      y[which(lh == j)] <- ifelse(length(df$count[df$hour == j & df$RESRCE == i]) > 0, 
                                  df$count[df$hour == j & df$RESRCE == i], 0)
    }
    
    if (sum(y) > 0) {
      ifelse(!first.row, plotStr <- paste(plotStr, ", ", sep = ""), first.row <- FALSE)
      
      #plotStr <- paste(plotStr, "{type: 'scatter', model: 'lines+markers', line: {shape: 'spline'},", sep = "")
      plotStr <- paste(plotStr, "{type: 'bar',", sep = "")
      plotStr <- paste(plotStr, "name: '", i, "',", sep = "")
      # Assemble x and y
      plotStr <- paste(plotStr, "x: ['", paste(lh[y > 0], collapse = "','"),"'],", sep = "")
      plotStr <- paste(plotStr, "y: [", paste(y[y > 0], collapse = ","),"]", sep = "")

      plotStr <- paste(plotStr, "}", sep = "")
    }
  }
  
  plotStr <- paste(plotStr, "];",
                             #"var layout = {title: 'SFC failures over past 24hrs'};",
                             "var layout = {title: 'SFC failures over past 24hrs', barmode: 'stack'};",
                             "myChart = document.getElementById('myChart');
                              Plotly.newPlot(myChart, data, layout);", sep = ""); 

  ##### Returning dataset ##### 
  printLog("Returning dataset")
  s <- toJSON(list(result = df.ret, plot = plotStr))
  #return(plotStr)
  return(s)
}
