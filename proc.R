#
# This bit of code lists processes by either operation or resource
#
# Author: Peeter Meos, Proekspert
# Date: 9. May 2017

library(plyr)

# Helper function just in case
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

uniquePaths <- function(data, data.wip, type="RESRCE", ignore.repairs=TRUE){
  if(type %not in% c("RESRCE", "OPERATION")){
    stop("Only path types allowed are: OPERATION, RESRCE")
  }
  
  df.proc <- data
  
  # Process SFC pairings, this extracts parent and child SFC pairs from
  # SFC_ORDER_HIST_WIP table
  df.pair <- data.wip
  
  df.pair$child <- gsub("^.*,","", as.character(df.pair$SFC_BO))
  
  df.pair <- df.pair[, c("child", "SFC")]
  names(df.pair) <- c("SFC", "parent.SFC")
  df.pair$parent.SFC <- as.character(df.pair$parent.SFC)
  df.proc$SFC <- as.character(df.proc$SFC)
  
  
  # Merge parent-child SFC pairs with main data frame. 
  # We are tracking child SFCs. If there is no child SFC, then fill in the blank with
  # the original SFC number
  df.pair <- merge(df.pair, df.proc, by.x="SFC", by.y="SFC")
  df.pair$parent.SFC[is.na(df.pair$parent.SFC)] <- df.pair$SFC[is.na(df.pair$parent.SFC)]
  
  # Order by SFC and date
  df.pair <- df.pair[order(df.pair$SFC, df.pair$date, decreasing=FALSE),]
  
  # Temporarily get rid of repairs, if requested
  if(ignore.repairs){
    df.pair <- df.pair[substr(df.pair$OPERATION, 1,3) != "REP", ]
    df.pair <- df.pair[substr(df.pair$OPERATION, 1,4) != "SREP", ]
  }
  
  df.pair.s <- df.pair[, c("parent.SFC", "RESRCE", "date")]
  df.pair <- df.pair[order(df.pair$parent.SFC, df.pair$date, decreasing=FALSE),]
  df.pair.s <- df.pair[, c("parent.SFC", "RESRCE")]
  df.pair.s <- unique(df.pair.s)
  
  
  # Pivot operations into string, a slow operation!
  df.pair.s <- ddply(df.pair.s, .(parent.SFC), summarize, Process = paste(RESRCE, collapse = " "))
  
  # Unique
  proc.unique <- unique(df.pair.s$Process)
  
  # Dimension reduction
  proc.unique <- proc.unique[order(nchar(proc.unique))]
  p <- proc.unique
  
  # Find all the subsprocesses that are fragments of bigger processes
  for(i in 1:length(proc.unique)){
    l <- grep(proc.unique[i], proc.unique)
    if(length(l) > 1) p[i] <- ""
  }
  
  # Eliminate empty strings
  proc.unique <- unique(p)[-1]
  
  # Order it alphabetically
  proc.unique <- proc.unique[order(proc.unique)]
  
  # .. and return the result
  return(proc.unique)
}

