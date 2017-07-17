#
# Plotting functions for WIP analysis 
#
# Author: Peeter Meos, Proekspert AS
# Date: 2. July 2017

library(ggplot2)
library(gridExtra)


#' Simple barplot of failed and repaired SFCs
#'
#' @param df dataframe containing repair and failure boolean vectors
#'
#' @return returns grid.extra object
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
plotFailRepair <- function(df){
  require(ggplot2)
  require(gridExtra)
  
  # Barplot of failures and no failures
  p1 <- ggplot(data = df, aes(x = failure, fill = failure)) +
    geom_bar(stat = "count") +
    theme_bw()
  
  # Barplot of repairs and no repairs
  p2 <- ggplot(data = df, aes(x = repair, fill = repair)) +
    geom_bar(stat = "count") +
    theme_bw()
  
  return(grid.arrange(p1, p2, ncol = 2))
}

#' Plots last known status of the SFC
#'
#' @param df dataframe containing last statuses and resources for SFCs
#'
#' @return returns grid.extra object
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
plotLastStatus <- function(df){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(data = df, aes(x = OPERATION, fill = RESRCE)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 90))
  
  p2 <- ggplot(data = df, aes(x = ACTION_CODE, fill = ACTION_CODE)) +
    geom_bar(stat = "count") +
    guides(fill = FALSE) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(grid.arrange(p1, p2, ncol = 2))
}


#' Plots barplot of resources
#'
#' @param df data frame containing last statuses of SFCs (RESRCE)
#' @param title string containing plot title
#'
#' @return ggplot object
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
plotResource <- function(df, title = ""){
  require(ggplot2)
  
  df$ops.resrce <- paste(df$OPERATION, df$RESRCE, sep = " - ")
  
  p <- ggplot(df, aes(x=ops.resrce, fill=ops.resrce)) +
       geom_bar(stat = "count") +
       guides(fill = FALSE) +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 90))
  
  if (title != "") p <- p + ggtitle(title)
  
  return(p)
}

#' A shot at the last known status of an SFC crosstab
#'
#' @param df containing last operations, status codes for given SFC
#'
#' @return returns ggplot object
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
plotStatusCrosstab <- function(df){
  require(ggplot2)
  require(reshape2)
  
  df.tmp <- dcast(df, OPERATION ~ ACTION_CODE, fun.aggregate = length, value.var = "OPERATION")
  df.tmp <- melt(data = df.tmp, id.vars = "OPERATION", value.name = "count", variable.name = "ACTION_CODE")
  
  p <- ggplot(df.tmp, aes(x = OPERATION, y = ACTION_CODE)) + 
    geom_point(aes(size = count, col = count)) + 
    theme_bw() + 
    xlab("") + 
    ylab("") +
    guides(color = FALSE) +
    scale_size_continuous(range=c(0, 5)) + 
  #  geom_text(aes(label = count))
    scale_colour_gradient(low = "white", high = "black") +
    #scale_colour_gradientn(colours=rainbow(4)) +
    theme(axis.text.x = element_text(angle = 90))
  
 return(p)
}


#' Violin plot by resource 
#'
#' @param df 
#' @param title 
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
plotWaitedTime <- function(df, title = ""){
  require(ggplot2)
  
  df$ops.resrce <- paste(df$OPERATION, df$RESRCE, sep = " - ")
  df$failure <- as.numeric(factor(df$failure, levels = c("FALSE", "TRUE"))) * 0.1
  
  p <- ggplot(df, aes(x = ops.resrce, y = waited, fill = ops.resrce)) +
       geom_violin(scale = "width") +
       geom_jitter(height = 0, width = 0.1, aes(size = failure, col = failure)) +
       scale_size_continuous(range=c(0, 1)) + 
       scale_y_continuous("Time since last reported event [hr]") +
       scale_x_discrete("Operation and resource of last reported event") +
       scale_color_gradient(low = "black", high = "red") +
       guides(fill = FALSE, color = FALSE, size = FALSE) +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 90))
  
  if (title != "") p <- p + ggtitle(title)
  
  return(p)
}


#' Plot of operations through time
#'
#' @param df data frame containing hourly summary of operations
#'
#' @return ggplot object
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
plotFlow <- function(df){
  require(ggplot2)
  require(scales)
  
  p <- ggplot(data = df, aes(x = hour, group = OPERATION, fill = OPERATION)) +
    geom_bar(stat = "count", position = "stack") +
    scale_x_continuous(breaks = pretty_breaks(20)) +
    scale_y_continuous(breaks = pretty_breaks(20)) +
    theme_bw()
  
  return(p)
}