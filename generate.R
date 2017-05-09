# Generation of random data frame for simFactory testing
#
# Author: Peeter Meos, Prekspert
# Date: 9. May 2017


source("stats.R")

generatePaths <- function(){
  # Need to generate following colums
  # "SFC" "OPERATION"     "STEP_ID" "RESRCE" "PASS1_QTY_STARTED" "PASS1_QTY_COMPLETED" "TIMES_PROCESSED"
  # "PASS1_ELAPSED_TIME" "PASS1_ELAPSED_QUEUE_TIME" "PREVIOUS_RESRCE" "ODS_DATE_TIME" "PASS1_NC_DEFECT_COUNT"
  
  # Calculate descriptive stats
  df.stats <- calcStats(df1)
  
  # proc.unique has the list of processes
  
}

