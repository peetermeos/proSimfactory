serviceCode <- function(operation = "", resource = ""){
  library("jsonlite")
  
  print(paste(Sys.time(), "Init", sep=": "))
  
  print("Inputs")
  print(paste("operation:", operation))
  print(paste("resource:", resource))
  
  load("c:/Temp/production_log.RData")
  print(paste(Sys.time(), "Loading successful", sep=": "))
  
  df <- df.production.log

  df <- df[!is.na(df$RESRCE),]
  df <- df[!is.na(df$OPERATION),]
    
  if( operation != ""){
    df <- df[grep(operation, df$OPERATION),]
  }
  
  if( resource != ""){
    df <- df[grep(resource, df$RESRCE),]
  }

  df$duration <-  df$PASS1_ELAPSED_TIME / 1000 / 60
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
               ", type: 'box', 
                  name: '", i,"', 
                  boxpoints: false,
                  marker: {opacity: 0.75}
               }", sep="")
  }
  c <- paste(c, "]", sep="")

#  jitter: 0.1, 
  
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

  print(paste(Sys.time(), "Calculating median", sep=": "))
  df1 <- aggregate(data=df, duration ~ RESRCE, FUN = quantile, probs = 0.5, na.rm = TRUE)
  names(df1) <- c("Resource", "MedianDuration[min]")
  
  print(paste(Sys.time(), "Calculating 90pct quantile", sep=": "))
  df2 <- aggregate(data=df, duration ~ RESRCE, FUN = quantile, probs = 0.90, na.rm = TRUE)
  names(df2) <- c("Resource", "90pctPercentile[min]")
  df3 <- merge(df1, df2, by="Resource")
  
  print(paste(Sys.time(), "Returning dataset", sep=": "))
  s <- toJSON(list(result = df3, plot = plotStr))
  return(s)
  }

version <- "v0.3.0"

inject <- function(type="update", version=version){
  if (type == "publish"){
    api <- publishService(
      name = "HistoricalActivityDuration",
      code = serviceCode,
      descr = "Historical length of activities by operation",
      inputs = list(operation = "character"),
      outputs = list(result = "character"),
      v = "v0.3.0"
    )
  }
  
  if (type == "update"){
    api <- updateService(
      name = "HistoricalActivityDuration",
      code = serviceCode,
      descr = "Historical length of activities by operation",
      inputs = list(operation = "character", resource = "character"),
      outputs = list(result = "character"),
      v = "v0.3.0"
    )  
  }
  print(paste(Sys.time(), "done", sep=": "))
  return(api)
}
