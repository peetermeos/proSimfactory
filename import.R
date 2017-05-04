library(ggplot2)
library(reshape2)

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

fn <- "Ersa Powerflow N2 - 04/04/01/2017_04_01_01_58_09.Trend"
#fn <- "Ersa Powerflow N2 - 04/04/01/2017_04_01_10_05_24.Trend"
#fn <- "Ersa Powerflow N2 - 04/04/01/2017_04_01_18_05_45.Trend"

#f.folder <- "Ersa Powerflow N2 - 04/04/01"
f.folder <- "Ersa Powerflow N2 - 04/04"

if (exists("df1")) rm(df1)

for(j in list.dirs(f.folder)[-1]){
  print(paste("Folder ", j))
  for(i in list.files(j)){
    print(i)
    df.tmp <- read.csv(paste(j, i, sep="/"), sep = ";")

    if(!exists("df1")){
      df1 <- df.tmp
    } else {
      df1 <- rbind(df1, df.tmp)
    }
  }
}
df.csv <- df1
rm(df1)

#df.csv <- read.csv(fn, sep = ";")

df.csv$Time <- as.POSIXct(as.character(df.csv$Time), format="%d.%m.%Y %H:%M:%S")

# Filtreerime igavad välja
df.csv <- df.csv[, names(df.csv) %not in% c("X", 
                                            "Message.of.type.Cyclic.is.available",
                                            "Message.of.type.Info.is.available",
                                            "Message.of.type.Waiting.is.available",
                                            "Periphery..1.ON.",
                                            "Solderpot.PH.bottom.pyro..offset...C.or...",
                                            "Preheating.bottom.1_1.pyro..offset...C.or...",
                                            "Preheating.bottom.1_2.pyro..offset...C.or...",
                                            "Preheating.bottom.2_1.pyro..offset...C.or...",
                                            "Preheating.bottom.3_1.pyro..offset...C.or...",
                                            "Conveyor.width..mm.",
                                            "Preheating.top.3.pyro..offset...C.or...")]

df1 <- df.csv
df1$outlier <- OutlierDetector(df1, alpha = 0.05)

# Joonistame
df.m <- melt(data=df1, id.vars = c("Time", "outlier"))
p <- ggplot(data=df.m, aes(x=Time, y=value, col=outlier)) +
  scale_x_datetime() +
  scale_color_manual(values=c("black", "red")) + 
  geom_point(size=0.5) +
  facet_wrap(~variable, scales = "free") +
  theme_bw()

print(p)
