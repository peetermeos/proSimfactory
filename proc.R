#
# This bit of code lists processes by either operation or resource
#
# Author: Peeter Meos, Proekspert
# Date: 9. May 2017

library(plyr)
library(Matrix)

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

findChildren <- function(m, sfc, parent){
  children <- which(m[parent,] > 0) 
  s <- sfc[parent]
  for(i in children[children != parent]){
    s <- paste(s, findChildren(m, sfc, i), sep=":")
  }
  
  return(s)
}

# Generates product tree from SFC_ORDER_HIST_WIP table
generateProductTree <- function(df){
  df$child <- gsub("^.*,","", as.character(df$SFC_BO))
  
  df <- df[, c("child", "SFC")]
  names(df) <- c("child", "parent")

  df$child <- as.character(df$child)
  df$parent <- as.character(df$parent)
  
  sfc <- unique(c(df$child, df$parent))
  sfc <- sfc[order(sfc)]
  
  # Create sparse adjacency matrix
  m <- Matrix(data=0, nrow=length(sfc), ncol = length(sfc), sparse = TRUE)
  #m <- matrix(data=0, nrow=length(sfc), ncol = length(sfc))
    
  # Populate matrix as a bidirectional graph
  for(i in 1:nrow(df)){
    i1 <- which(sfc == df$parent[i])
    i2 <- which(sfc == df$child[i])  
    m[i1, i2] <- 1
    #m[i2, i1] <- 1
  }
  
  child  <-  ""
  
  # # Loop through all the parents
  # for(i in 1:nrow(m)){
  #   child[i] <- findChildren(m, sfc, i)
  # }
  # 
  # 
  # # Find all the subsprocesses that are fragments of bigger processes
  # child <- child[order(child)]
  # p <- child
  # 
  # for(i in 1:length(child)){
  #   l <- grep(child[i], child)
  #   if(length(l) > 1) p[i] <- ""
  # }
  # # Eliminate empty strings
  # child <- unique(p)[-1]
  # rm(p)
  # 
  # group <- 0
  # group <- sapply(sfc, function(x){grep(x, child)[1]})
  # 
  # # Return the results
  # return(list(sfc=data.frame(sfc, group, row.names=seq(1, length(sfc))), matrix=m, child=child))  
  return(list(matrix=m))
}

