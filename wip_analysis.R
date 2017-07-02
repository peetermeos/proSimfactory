
library(reshape2)
library(dplyr)
source("func.R")

# Get the plotting functionality
source("wip_plots.R")

notRun <- function(){
# Pivot on action codes
df <- dcast(data = df.activity.log, SFC + RESRCE + OPERATION ~ ACTION_CODE, value.var = "DATE_TIME", 
            fun.aggregate = mean)

# For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")

# Tag fails and repairs
df$failure <- ifelse(is.na(df$LOG_FAILURE) &  is.na(df$CLOSE_FAILURE) &  is.na(df$FAIL),  FALSE, TRUE)
df$repair  <- ifelse(is.na(df$LOG_REPAIR) &  is.na(df$CLOSE_REPAIR),  FALSE, TRUE)

df.agg.fail <- aggregate(data = df, failure ~ SFC, FUN = function(x) {length(which(x == TRUE)) > 0})
df.agg.rep <- aggregate(data = df, repair ~ SFC, FUN = function(x) {length(which(x == TRUE)) > 0})
df.agg <- merge(df.agg.fail, df.agg.rep, by = "SFC")
rm(df.agg.fail, df.agg.rep)
plotFailRepair(df.agg)

# Now lets try to find last known status
# Order the dataset by date
df <- df.activity.log[order(df.activity.log$DATE_TIME), ]
# Take last of every SFC
df.res <- aggregate(data = df, RESRCE ~ SFC, FUN = function(x) {tail(x, 1)})
df.opr <- aggregate(data = df, OPERATION ~ SFC, FUN = function(x) {tail(x, 1)})
df.act <- aggregate(data = df, ACTION_CODE ~ SFC, FUN = function(x) {tail(x, 1)})
df.status <- merge(df.res, df.act, by = "SFC")
df.status <- merge(df.status, df.opr, by = "SFC")
rm(df.res, df.act, df.opr)

plotStatusCrosstab(df.status)
}

#' Finds SFCs that are waiting for repair
#' We begin with finding items that have failed in their last step
#' Then we assume that they are in queue
#'
#' @param df Data frame containing event log
#'
#' @return data frame containing SFCs that have failed and wait for resolution
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
findRepairQueue <- function(df){
  require(reshape2)
  require(dplyr)

  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, value.var = "DATE_TIME", 
              fun.aggregate = mean)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)

  # Order the dataset by date
  df <- df[order(df$event_end), ]
    
  # Now find the SFCs that have failed
  df$failure <- ifelse(is.na(df$LOG_FAILURE) &  is.na(df$CLOSE_FAILURE) &  is.na(df$FAIL),  FALSE, TRUE)
  
  ## Take last of every SFC
  df.fail <- aggregate(data = df, failure ~ SFC, FUN = function(x) {tail(x, 1)})
  df.res <- aggregate(data = df, RESRCE ~ SFC, FUN = function(x) {tail(x, 1)})
  df.opr <- aggregate(data = df, OPERATION ~ SFC, FUN = function(x) {tail(x, 1)})
  df.date <- aggregate(data = df, event_end ~ SFC, FUN = function(x) {tail(x, 1)})
  
  df <- merge(df.fail, df.res, by = "SFC") %>% 
    merge(df.opr, by = "SFC") %>%
    merge(df.date, by = "SFC")
  #df <- merge(df, df.opr, by = "SFC")
  #df <- merge(df, df.date, by = "SFC")
   
  # Now we should have a list containing data about last known event of every SFC
  # Filter out fails
  df <- df[df$failure == TRUE, ]
  df <- df[order(df$event_end), ]
  
  return(df)
}