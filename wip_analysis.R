#
# simFactory
# Analysis code (actually mostly pivoting and aggrecation) for WIP database 
#
# Author: Peeter Meos, Proekspert AS
# Date: 1. July 2017
#

library(reshape2)
library(dplyr)

source("func.R")
source("db.R")
source("wip_plots.R")

# Get the plotting functionality
source("wip_plots.R")

notRun <- function(){
  # Database retrieval stuff
  t1 <- "2017-06-08"
  t2 <- "2017-06-09"
  
  sql <- initSQL("WIP")
  db <- connectSQL(sql)
  df.activity.log  <- getTable(db, "ACTIVITY_LOG", t1, t2)
  df.sfc <- getTable(db, "ACTIVITY_LOG", t1, t2, "MODIFIED_DATE_TIME")
  df.item <- getTable(db, "ITEM")
  df.status <- getTable(db, "STATUS")
  
  disconnectSQL(db)
  
  # Analysis stuff
  df.agg <- findFailsRepairs(df.activity.log)
  plotFailRepair(df.agg)

  # Now lets try to find last known status
  df.status <- findLastKnownStatus(df.activity.log)
  plotStatusCrosstab(df.status)
}

#' Finds and tags SFCs that have failed or been repaired at least once
#'
#' @param df containing WIP event log
#'
#' @return data frame containing boolean vectors of "repair" and "failure"
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
findFailsRepairs <- function(df){
  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, value.var = "DATE_TIME", 
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
  
  return(df.agg)
}

#' Filters out last known statuses of SFCs based on given activity logs
#'
#' @param df activity_log data frame from WIP database
#'
#' @return data frame containing last known statuses, resources and operations of SFC
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
findLastKnownStatus <- function(df){
  # Now lets try to find last known status
  # Order the dataset by date
  df <- df[order(df$DATE_TIME), ]
  
  # Take last of every SFC
  df.res <- aggregate(data = df, RESRCE ~ SFC, FUN = function(x) {tail(x, 1)})
  df.opr <- aggregate(data = df, OPERATION ~ SFC, FUN = function(x) {tail(x, 1)})
  df.act <- aggregate(data = df, ACTION_CODE ~ SFC, FUN = function(x) {tail(x, 1)})
  df.status <- merge(df.res, df.act, by = "SFC")
  df.status <- merge(df.status, df.opr, by = "SFC")
  rm(df.res, df.act, df.opr)
  
  return(df.status)
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

  # Now we should have a list containing data about last known event of every SFC
  # Filter out fails
  df <- df[df$failure == TRUE, ]
  df <- df[order(df$event_end), ]
  
  return(df)
}