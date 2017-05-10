library(ggplot2)
library(scales)

t1 <- as.POSIXct("2017-01-07 05", format="%Y-%m-%d %H")
t2 <- as.POSIXct("2017-01-15 18", format="%Y-%m-%d %H")

df2 <- df1[df1$date >= t1 & df1$date <= t2, ]
df2$type <- substr(df2$SFC, 1, 2)
df2$type <- factor(df2$type)

df2$PASS1_ELAPSED_TIME <- df2$PASS1_ELAPSED_TIME 

df2$OPERATION <- factor(as.character(df2$OPERATION), levels=c(
  "ADE",    

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
  
  "PCB"
  
#  "ADE",      "BIT",      "CCO",      "FBA",      "FCT",      "FMT",      "HPT",      "HSO",      "ICT",     
#  "LPR",      "MDE",      "MMA",      "PBT",      "PCB",      "PFI",      "PRG",      "REP_BIT",  "REP_FCT", 
#  "REP_FMT",  "REP_HPT",  "REP_ICT",  "REP_SMA",  "SAOI",     "SMA",      "SPI",      "SREP_SMA", "SSO",     
#  "THT",      "WSH"  

  ))

#[df2$type == "EE",]
p <- ggplot(data = df2, aes(x=date, y=OPERATION, group=SFC, col=type, size=PASS1_ELAPSED_TIME)) +
     ggtitle(paste("Processes in period from", t1, "through", t2, sep = " ")) +
     geom_point() +
     geom_line(size=0.15) +
     scale_x_datetime(breaks=pretty_breaks(20), name="Date") +
     scale_y_discrete(limits = rev(levels(df2$OPERATION))) +
     scale_color_discrete(name="SFC first letters") +
     #facet_wrap(~type) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))

pdf(file =   "170508 flow with lines.pdf", width=18, height=12)
print(p)
dev.off()
