library(ggplot2)
library(reshape2)


fn <- "2017_04_27_03_46_19.protocol"
df <- read.csv(fn, sep=";", skip=1)
df$Time <- as.POSIXct(as.character(df$Time), format="%d.%m.%Y %H:%M:%S")


plot.dim <- function(data, var){
  df.s <- data[,c(1,var)]
  names(df.s)[2] <- "Y"
  
  p <- ggplot(data=df.s, aes(x=Time, y=Y)) +
    scale_x_datetime() +
    geom_point() +
    theme_bw()
  
  
  print(p)
}

df.m <- melt(data=df, id.vars = "Time")
p <- ggplot(data=df.m, aes(x=Time, y=value)) +
     scale_x_datetime() +
     geom_point() +
     facet_wrap(~variable, scales = "free") +
     theme_bw()
   
print(p)
