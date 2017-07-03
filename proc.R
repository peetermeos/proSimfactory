#
# This bit of code lists processes by either operation or resource
#
# Author: Peeter Meos, Proekspert
# Date: 9. May 2017

library(plyr)
library(Matrix)

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
  
  # Create vector of unique SFCs
  sfc <- unique(c(df$child, df$parent))
  sfc <- sfc[order(sfc)]
  sfc <- factor(sfc)

  # Make SFCs in df factors
  df$child  <- factor(df$child, levels=sfc)
  df$parent <- factor(df$parent, levels=sfc)
  
  # Eliminate self referrals
  df <- df[df$child != df$parent,]

  # Create sparse adjacency matrix
  # Populate matrix as a unidirectional graph from parent (row)
  # to child (column). 
  m <- sparseMatrix(i=as.integer(df$parent), j=as.integer(df$child), x=1, dims=c(length(sfc), length(sfc)))
  
  visited <- rep(FALSE, nrow(m))

  # Annab tagasi character vectori.
  # Vektori pikkus on laste arv
  # Iga elemendi sisu on laps pluss selle lapsed
  iterateChildren <- function(node){
    s <- ""
    
    if(node <= ncol(m)){
      n <- which(m[node,] > 0)

      if(length(n) > 0)
        visited[n] <- TRUE
        s <- paste(sfc[node], sapply(n, iterateChildren), sep=":")
    }
    return(s)
  }
  
  tree <- character(0)
  for(i in 1:nrow(m)){
    if(!visited[i]){
      visited[i] <- TRUE
      if (length(which(m[i,] > 0)) > 0)
        tree <- c(tree, iterateChildren(i))
      else
        tree <- c(tree, sfc[i]) 
    }
  }
  
  # Order it and prune it
  tree <- unique(tree[order(tree)])
  
  for(i in 1:length(tree)){
    if(nchar(tree[i]) > 0)
      if(length(grep(tree[i], tree)) > 0) 
         tree[i] <- ""
  }
  
  tree <- tree[tree != ""]
  return(list(matrix=m, tree=tree))
}

