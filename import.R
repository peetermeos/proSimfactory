library(ggplot2)
library(reshape2)
library(scales)
library(gganimate)
library(knitr)
library(animation)

source("outlier.R")
source("filters.R")

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

fn <- "Ersa Powerflow N2 - 04/04/01/2017_04_01_01_58_09.Trend"
#fn <- "Ersa Powerflow N2 - 04/04/01/2017_04_01_10_05_24.Trend"
#fn <- "Ersa Powerflow N2 - 04/04/01/2017_04_01_18_05_45.Trend"

#f.folder <- "Ersa Powerflow N2 - 04/04/01"
f.folder <- "Ersa Powerflow N2 - 04/04"

# if (exists("df1")) rm(df1)
# 
# for(j in list.dirs(f.folder)[-1]){
#   print(paste("Folder ", j))
#   for(i in list.files(j)){
#     print(i)
#     df.tmp <- read.csv(paste(j, i, sep="/"), sep = ";")
# 
#     if(!exists("df1")){
#       df1 <- df.tmp
#     } else {
#       df1 <- rbind(df1, df.tmp)
#     }
#   }
# }
# df.csv <- df1
# rm(df1)

df.csv <- read.csv(fn, sep = ";")

df.csv$Time <- as.POSIXct(as.character(df.csv$Time), format="%d.%m.%Y %H:%M:%S")

# Filtreerime ivälja mittevajalikud
df.csv <- df.csv[, names(df.csv) %in% outlier.filter]

df1 <- df.csv
df1$outlier <- OutlierDetector(df1, alpha = 0.05)

# Filtreerime igavad välja
df1 <- df1[, names(df1) %in% draw.filter]

# Animation setup
num.frames <- 200
a.h <- 1024
a.w <- 800

# Create what-if (region to be replaced)
t1.tmp <- as.POSIXct("2017-04-01 06:48:25", format="%Y-%m-%d %H:%M:%S")
t2.tmp <- as.POSIXct("2017-04-01 07:22:43", format="%Y-%m-%d %H:%M:%S")

# How many data points?
n.points <- nrow(df1[df1$Time >= t1.tmp & df1$Time <= t2.tmp,])
t3.tmp <- as.POSIXct("2017-04-01 06:00:00", format="%Y-%m-%d %H:%M:%S")

# Extract earlied good time series
df1.tmp <- df1[df1$Time >= t3.tmp,][(1:n.points), ]
df1.tmp$Time <- df1$Time[df1$Time >= t1.tmp][(1:n.points)]

# Replace tag and add to the data frame
df1.tmp$outlier <-  "WHAT-IF"
df1 <- rbind(df1, df1.tmp)
rm(df1.tmp)

# Distribute data into animation frames
df1 <- df1[order(df1$Time),]
df1$Frame <- round(seq(1, nrow(df1)) / (nrow(df1)/num.frames))
df1$Line <- sapply(df1$Frame, function(x){
  return(as.character(df1$Time[df1$Frame == x][which.max(df1$Time[df1$Frame == x])]
  ))   })
df1$Line <- as.POSIXct(df1$Line, format="%Y-%m-%d %H:%M:%S")

# Joonistame
df.m <- melt(data=df1, id.vars = c("Time", "Frame", "Line", "outlier"))
df.m$value[df.m$value == "False"] <- 0
df.m$value[df.m$value == "True"] <- 1
df.m$value <- as.numeric(df.m$value)

p <- ggplot(data=df.m, aes(x=Time, y=value, size=outlier)) +
  scale_x_datetime(breaks=pretty_breaks(20)) +
  scale_y_continuous(breaks = pretty_breaks(5)) +
  scale_color_manual(values=c("black", "red", "blue")) + 
  scale_size_manual(values=c(0.5, 1.75, 0.5)) +
  geom_point(aes(frame=Frame, cumulative = TRUE, col=outlier)) +
  geom_vline(aes(xintercept = as.numeric(Line), frame = Frame), lty = 2, color = "red") +
  facet_grid(variable~., scales = "free") +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) 



ani.options(ani.width=a.h, ani.height=a.w)
a <- gganimate(p, interval=0.2, format="avi", title_frame = FALSE)

print(a, format="avi", ani.width=a.h, ani.height=a.w)
#print(p)
