serviceCode <- function(){
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  
  load("c:/Temp/2017-01-18.RData")
  print("Loading successful")
  df <- df.ods

  df <- df[!is.na(df$RESRCE),]
  df <- df[!is.na(df$OPERATION),]
  
  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, drop = TRUE, value.var = "DATE_TIME",
              fun.aggregate = mean, na.rm = TRUE)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  print("DCAST successful")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  df$waited <- round(as.numeric(difftime(max(df$event_end), df$event_end, units = "hours")),2)
   
  df <- df[, c("SFC", "OPERATION", "RESRCE", "waited")]
  
  print("Creating plot")
  c <- "["
  first <- TRUE
  for(i in unique(df$RESRCE)){
    if (first)
      first <- FALSE
    else
      c <- paste(c, ", ", sep="")
      c <- paste(c, "{y: [", paste(df$waited[df$RESRCE == i], collapse = ","),"]",
                 ",x: [", paste(paste("'", df$OPERATION[df$RESRCE == i], "'", sep=""), collapse = ","),"]",
                 ", type: 'box', name: '", i,"', boxpoints: 'all', jitter: 0.4, marker: {opacity: 0.75}}", sep="")
  }
  c <- paste(c, "]", sep="")

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
                   sep= "");

  print("Returning dataset")
  s <- toJSON(list(result = data.frame(), plot = plotStr))
  return(s)
}

version <- "v0.1.0"
name <- "Daily work"
description <- "Distribution of daily work between resources"

inject <- function(type="update", version=version){
  if (type == "publish"){
    api <- publishService(
      name = "DailyWork",
      code = serviceCode,
      descr = "Distribution of daily work",
      outputs = list(result = "character"),
      v = "v0.1.0"
    )
  }
  
  if (type == "update"){
    api <- updateService(
      name = "DailyWork",
      code = serviceCode,
      descr = "Distribution of daily work between resources",
      outputs = list(result = "character"),
      v = "v0.1.0"
    )  
  }
  return(api)
}


