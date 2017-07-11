serviceCode <- function(operation = ""){
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  
  print("Inputs")
  print(paste("operation:", operation))
  
  load("c:/Temp/2017-01-18.RData")
  print("Loading successful")
  df <- df.ods

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
  print("DCAST successful")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  df$duration <- round(as.numeric(difftime(df$event_end, df$START, units = "mins")), 2)
  df <- df[!is.na(df$duration),]
  
  df <- df[, c("SFC", "OPERATION", "RESRCE", "duration")]

  print("Creating plot")
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

  print("Returning dataset")
  
  df1 <- aggregate(data=df, duration ~ RESRCE, FUN = quantile, probs = 0.5, na.rm = TRUE)
  names(df1) <- c("Resource", "MedianDuration[min]")
  df2 <- aggregate(data=df, duration ~ RESRCE, FUN = sd, na.rm = TRUE)
  df2$duration <- round(df2$duration, 2)

  names(df2) <- c("Resource", "StandardDeviation")
  df <- merge(df1, df2, by="Resource")
  
  s <- toJSON(list(result = df, plot = plotStr))
  return(s)
  }

version <- "v0.2.0"

inject <- function(type="update", version=version){
  if (type == "publish"){
    api <- publishService(
      name = "ActivityDuration",
      code = serviceCode,
      descr = "Length of activities by operation",
      inputs = list(operation = "character"),
      outputs = list(result = "character"),
      v = "v0.2.0"
    )
  }
  
  if (type == "update"){
    api <- updateService(
      name = "ActivityDuration",
      code = serviceCode,
      descr = "Length of activities by operation",
      inputs = list(operation = "character"),
      outputs = list(result = "character"),
      v = "v0.2.0"
    )  
  }
  return(api)
}


