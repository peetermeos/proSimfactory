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
  #df.sfc <- getTable(db, "ACTIVITY_LOG", t1, t2, "MODIFIED_DATE_TIME")
  df.item <- getTable(db, "ITEM")
  df.status <- getTable(db, "STATUS")
  
  disconnectSQL(db)
  
  # Analysis stuff
  df.agg <- findFailsRepairs(df.activity.log)
  plotFailRepair(df.agg)

  # Now lets try to find last known status
  df.status <- findLastKnownStatus(df.activity.log)
  plotStatusCrosstab(df.status)
  
  df.queue <- findRepairQueue(df.activity.log)
  plotWaitedTime(df.queue, title = paste("When we last heard about the SFC Y hours ago it was at resource X. \n Red = failure \n Covering period",
                                         t1 , "-", t2))
  #plotResource(df.queue, title = "Failed SFCs by last known resource")
}

# Last SFC status
#SELECT     a.SFC, a.SHOP_ORDER_BO, a.QTY, a.QTY_DONE, a.QTY_SCRAPPED, a.ACTUAL_COMP_DATE, a.MODIFIED_DATE_TIME, b.STATUS_DESCRIPTION,  
#ITEM.ITEM 
#FROM            SFC AS a INNER JOIN 
#STATUS AS b ON a.STATUS_BO = b.HANDLE INNER JOIN 
#ITEM ON a.ITEM_BO = ITEM.HANDLE 
#WHERE        (ITEM.ITEM LIKE '9151')

#' Tags fails and repairs in the event log
#'
#' @param df casted event log data frame
#'
#' @return same data frame with fail and repair added
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
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
  
  # Find fails and repairs
  df <- tagFailsRepairs(df)
  
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
findRepairQueue <- function(df, only.fails = FALSE){
  require(reshape2)
  require(dplyr)

  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, drop = TRUE, value.var = "DATE_TIME", 
              fun.aggregate = mean, na.rm = TRUE)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)

  # Order the dataset by date
  df <- df[order(df$event_end), ]
    
  # Find fails and repairs
  df <- tagFailsRepairs(df)

  # Take last of every SFC
  df <- df %>% group_by(SFC) %>% filter(event_end == max(event_end))
  df <- df[!is.na(df$RESRCE),]

  # Now we should have a list containing data about last known event of every SFC
  # Filter out fails
  if (only.fails) df <- df[df$failure == TRUE, ]

  df <- df[order(df$event_end), ]
  #df$waited <- as.numeric(difftime(df$event_end, as.POSIXct(t2), units = "hours"))
  df$waited <- as.numeric(difftime(max(df$event_end), df$event_end, units = "hours"))
  
  return(df[, c("SFC", "OPERATION", "RESRCE", "waited")])
}

#' Hourly operation flow
#'
#' @param df Activity log data frame
#'
#' @return Hourly summary of operations completed
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
createFlow <- function(df){
  require(reshape2)
  
  # Pivot on action codes
  df <- dcast(data = df, SFC + RESRCE + OPERATION ~ ACTION_CODE, value.var = "DATE_TIME", 
              fun.aggregate = mean)
  
  # For some reason dcast doesn't like dates and turns them into numeric. Turn them back.
  df[, -(1:3)] <- lapply(df[,-(1:3)], as.POSIXct, origin = "1970-01-01")
  
  # Find the event end time
  df$event_end <- apply(df[,-(1:3)], 1, max, na.rm = TRUE)
  
  # Extract hour
  df$hour <- as.numeric(format(as.POSIXct(df$event_end), format = "%H"))
  
  r <- dcast(data = df, hour ~ OPERATION, value.var = "OPERATION", fun.aggregate = length)

  return(r)
}