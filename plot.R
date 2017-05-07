library(ggplot2)

t1 <- as.POSIXct("2017-01-07 05", format="%Y-%m-%d %H")
t2 <- as.POSIXct("2017-01-07 18", format="%Y-%m-%d %H")

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
  "CCO",
  "REP_FCT", "FCT",  

  "WSH",

  
    
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
     geom_point() +
     geom_line(size=0.15) +
     scale_x_datetime() +
     scale_y_discrete(limits = rev(levels(df2$OPERATION))) +
     #facet_wrap(~type) +
     theme_bw()

print(p)
