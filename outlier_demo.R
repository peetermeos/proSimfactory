library(ggplot2)
library(scales)
library(reshape2)
library(gganimate)

source("outlier.R")

# Create data frame
len <- 500
s.dev <- c(0.1, 0.02, 0.075)

shift <- c(3,3,3)

Time <- seq(1, len)
df <-  as.data.frame(Time)

# Add signal
df$s1 <- sin(Time/50)
df$s2 <- sin(Time/50)
df$s3 <- sin(Time/50)

# Add jump to s1
df$s1[(len/2):nrow(df)] <- sin(Time[(len/2):nrow(df)] / 100) * 2.5 - 0.6
# Change phase for s2 and s3
df$s2[(len/2):nrow(df)] <- sin(Time[(len/2):nrow(df)]/50 + 500) / 5 - 1.1
df$s3[(len/2):nrow(df)] <- sin(Time[(len/2):nrow(df)]/50 + 500) / 5 - 1.1

# Add noise and shift
df$s1 <- df$s1 + rnorm(len, 0, s.dev[1]) + shift[1]
df$s2 <- df$s2 + rnorm(len, 0, s.dev[2]) + shift[2]
df$s3 <- df$s3 + rnorm(len, 0, s.dev[3]) + shift[3]

# Do outlier detection
df$outlier <- OutlierDetector(df[,-1], alpha = 0.15)

# Animation setup
num.frames <- 200
a.h <- 1024
a.w <- 800

# Distribute data into animation frames
df <- df[order(df$Time),]
df$Frame <- round(seq(1, nrow(df)) / (nrow(df)/num.frames))
df$Line <- sapply(df$Frame, function(x){
  return(as.character(df$Time[df$Frame == x][which.max(df$Time[df$Frame == x])]))   })
#df$Line <- as.POSIXct(df1$Line, format="%Y-%m-%d %H:%M:%S")

df.m <- melt(df, id.vars=c("Time", "Frame", "Line", "outlier"))

p <- ggplot(df.m, aes(x=Time, y=value, size=outlier)) +
     scale_color_manual(values=c("black", "red")) +
     scale_size_manual(values=c(0.5, 1.75)) +
     geom_point(aes(frame=Frame, cumulative = TRUE, col=outlier)) +
     geom_vline(aes(xintercept = as.numeric(Line), frame = Frame), lty = 2, color = "red") +
     facet_grid(variable~., scales="free") +
     theme_bw() +
     theme(strip.text.y = element_text(angle=0)) 

print(p)

ani.options(ani.width=a.h, ani.height=a.w)
a <- gganimate(p, interval=0.2, format="avi", title_frame = FALSE)

print(a, format="mp4", ani.width=a.h, ani.height=a.w)