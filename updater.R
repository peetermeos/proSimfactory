findRepairQueueService <- function(){
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  
  load("c:/Temp/2017-01-18.RData")
  print("Loading successful")
  df <- df.ods
  only.fails <- TRUE
  
  tagFailsRepairs <- function(df){
    # Tag fails and repairs
    df$failure <- ifelse(
      # If theres LOG_FAILURE that has not been closed
      (is.na(df$CLOSE_FAILURE) & !is.na(df$LOG_FAILURE))|
        # If there is a fail
        !is.na(df$FAIL),  TRUE, FALSE)
    
    df$repair  <- ifelse(!is.na(df$LOG_REPAIR) &
                           is.na(df$CLOSE_REPAIR),  TRUE, FALSE)
    
    return(df)
  }
  
  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, drop = TRUE, value.var = "DATE_TIME",
              fun.aggregate = mean, na.rm = TRUE)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  print("DCAST successful")
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  # Order the dataset by date
  df <- df[order(df$event_end), ]
  
  # Find fails and repairs
  df <- tagFailsRepairs(df)
  print("Tagging fails and repairs successful")
   
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
  
  print("Saving dataset")
  write.csv(df, file="data.csv")
  print("Returning dataset")
  s <- summary(df)
  print(s)
  
  c <- "["
  first <- TRUE
  for(i in unique(df$RESRCE)){
    if (first)
      first <- FALSE
    else
      c <- paste(c, ", ", sep="")
    c <- paste(c, "{y: [", paste(df$waited[df$RESRCE == i], collapse = ","),"]",
               ",x: [", paste(paste("'", df$OPERATION[df$RESRCE == i], "'", sep=""), collapse = ","),"]",
               ", type: 'box', name: '", i,"', boxpoints: 'all', jitter: 0.3}", sep="")
  }
  c <- paste(c, "]", sep="")

  plotStr <- paste("var data = ", c, ";",
                   "var layout = {
                          yaxis: {
                                title: 'Time in queue [h]'
                          },
                          xaxis: {
                            title: 'Operation'
                          },
                          boxmode: 'group'
                   };
                    myChart = document.getElementById('myChart');
                    Plotly.newPlot(myChart, data, layout);", sep= "");    
    
  s <- toJSON(list(result = data.frame(df[,c(1:4)]), plot = plotStr))
  return(s)
}

#api <- publishService(
api <- updateService(
  "FindService",
  code = findRepairQueueService,
  descr = "Find fails and repairs",
  outputs = list(result = "character"),
  v = "v1.3.0"
)

