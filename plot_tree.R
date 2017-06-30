library(reshape2)
library(ggplot2)
library(scales)

# Every tree element is grouping

# We have to match the event in production log with correct grouping by SFC
# What if there is more than one grouping for SFC?

# Use m (list of product trees and adjacency matrix)
# use df.prod

df.m <- df.prod

m$tree <- factor(m$tree)

#df.m$group <- ""
#for(i in 1:nrow(df.m)){
#  df.m$group[i] <- grep(as.character(as.character(df.m$SFC[i])), as.character(m$tree))[1]
#}
#df.m$group <- sapply(df.m$SFC, function(x){return(m$tree[grep(as.character(x), as.character(m$tree))][1])})


df.m$OPERATION <- factor(as.character(df.m$OPERATION), levels=c(
  "LPR",  
    
  "SPI",
  "REP_SMA",      
  "SREP_SMA",  
  "SMA",
  "SAOI",
  "PFI",
  "THT",
  "MDE",
  "SSO", 
  "MMA",  
  "ICT",     
  "PRG",
  "REP_ICT", 
  "HSO",
  "FCT",  
  "REP_FCT",
  "WSH",
  "CCO",
  "FBA",   
  "HPT",     "REP_HPT", 
  "PBT", 
  "BIT",
  "REP_BIT",
  "FMT",     "REP_FMT", 
  "PCB",
  "ADE"
))

#df.m <- df.m[order(df.m$ODS_DATE_TIME, df.m$OPERATION),]
#df.m$group <- factor(df.m$group)
#df.m <- df.m[!is.na(df.m$group),]

order.filter <- unique(df.m$SHOP_ORDER)[1:286]
#order.filter <- c("195003276", "195007715", "195007716", "195007716")
#order.filter <- c("194987120", "194987511", "194989305", "194982696")

p <- ggplot(data = df.m[as.character(df.m$SHOP_ORDER) %in% order.filter 
                        & !is.na(df.m$OPERATION)
                        & df.m$ODS_DATE_TIME < as.POSIXct("2017-05-09"),], 
            aes(x=ODS_DATE_TIME, y=OPERATION, col=SHOP_ORDER, group=SFC)) +
     geom_point() +
     geom_line() +
     scale_color_discrete(guide=FALSE) +
     scale_x_datetime("Date", date_breaks = "4 hours", date_minor_breaks = "1 hour") +
     scale_y_discrete(limits = rev(levels(df.m$OPERATION))) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 15, hjust = 1))

pdf(file =   "170523 order flow.pdf", width=18, height=12)
print(p)
dev.off()
