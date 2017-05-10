# Generation of random data frame for simFactory testing
#
# Author: Peeter Meos, Prekspert
# Date: 9. May 2017

library(stringr)

source("stats.R")

# Generates random SFC code
generateSFC <- function(prefix="EE", n=1, len=10){
  # EE1702R67517 
  return(paste(prefix, str_pad(round(runif(n, min=0, max=10^len)), width=len, side="left", pad="0"),sep=""))
}

findMean <- function(x, df){
  m <- 0
  if (length(df.stats$ops.m[df.stats$res == x]) > 0)
    if(!is.na(df$ops.m[df$res == x]) & is.numeric(df$ops.m[df$res == x]))
      m <- df$ops.m[df$res == x]
  return(m)
}

findMeanQ <- function(x, df){
  m <- 0
  if (length(df.stats$ops.m.q[df.stats$res == x]) > 0)
    if(!is.na(df$ops.m.q[df$res == x]) & is.numeric(df$ops.m.q[df$res == x]))
      m <- df$ops.m.q[df$res == x]
    return(m)
}

# n - number of SFCs to generate
generatePaths <- function(proc.list, n=1, start.date=NULL){
  # Need to generate following colums
  # "SFC" "OPERATION"     "STEP_ID" "RESRCE" "PASS1_QTY_STARTED" "PASS1_QTY_COMPLETED" "TIMES_PROCESSED"
  # "PASS1_ELAPSED_TIME" "PASS1_ELAPSED_QUEUE_TIME" "PREVIOUS_RESRCE" "ODS_DATE_TIME" "PASS1_NC_DEFECT_COUNT"
  
  # Handle start date
  if(is.null(start.date)){
    start.date = Sys.Date()
  }
  
  # Calculate descriptive stats
  df.stats <- calcStats(df1)
  
  # Calculate resource-operation tuple
  res.tuple <- calcResources(df1)
  
  # proc.unique has the list of processes
  
  sfc <- generateSFC(n=n)
  
  for(i in 1:n){
    # Pick a random process
    p <- strsplit(proc.list[round(runif(1, 1, length(proc.list)))], split=" ")[[1]]
    prv <- c(NA, p[-length(p)])
    step <- seq(10, length(p)*10, by=10)
    
    #"PASS1_ELAPSED_TIME" 
    t.elapsed <- sapply(p, function(x){round(rexp(1,1/findMean(x, df.stats)))})
    #"PASS1_ELAPSED_QUEUE_TIME"
    t.queue <- sapply(p, function(x){round(rexp(1,1/findMeanQ(x, df.stats)))})
    
    df.tmp <-  data.frame(cbind(sfc[i], step, p, prv, t.elapsed, t.queue))
    
    
    if(i == 1){
      df <- df.tmp
    } else {
      df <- rbind(df, df.tmp)
    }
    
  }
  
  names(df) <- c("SFC", "STEP_ID", "RESRCE", "PREVIOUS_RESRCE", "PASS1_ELAPSED_TIME", "PASS1_ELAPSED_QUEUE_TIME" )
  
  # Add operation column
  df$OPERATION = sapply(df$RESRCE, function(x){return(as.character(res.tuple$OPERATION[as.character(res.tuple$RESRCE) == as.character(x)]))})
  
  # Other stuff

  df$PASS1_QTY_STARTED <- 1
  df$PASS1_QTY_COMPLETED <- 1
  df$TIMES_PROCESSED <- 1
  
  df$PASS1_NC_DEFECT_COUNT <- 0
  
  # Times
  df$ODS_DATE_TIME <- format(start.date, format="%Y-%m-%d %H:%M:%S")
  
  row.names(df) <- NULL
  
  return(df)
}

