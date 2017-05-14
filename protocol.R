library(dplyr)
library(reshape2)
library(ggplot2)

fn <- "2017_04_27_03_46_19.protocol"
df <- read.csv(fn, sep=";", dec=",", skip=1)

# First two rows are bad
df <- df[-(1:2),]
# Ignore last row
df <- df[-nrow(df),]

# Separate actual and set data
df.set    <- df[is.na(df$Serial.board.number),]
df.actual <- df[!is.na(df$Serial.board.number),]

# From set data, erase zero and NA columns
df.set.vals <- names(df.set %>% select_if(colSums(!is.na(.)) > 0) %>% select_if(colSums(. != 0) > 0) )

# Set types
df.actual$type <- "actual"
df.set$type <- "set"

df.set$Serial.board.number <- df.actual$Serial.board.number
df.set$Running.in.moment <- df.actual$Running.in.moment

# Merge actuals and sets
df.new <- rbind(df.actual, df.set)
df.new$type <- factor(df.new$type)

# Melt df
df.new <- melt(data = df.new, id.vars = c("Serial.board.number", "type", "Running.in.moment"))
df.new$Running.in.moment <- as.POSIXct(as.character(df.new$Running.in.moment), format="%Y-%m-%d %H:%M:%S")

# Eliminate zeroes and NAs
df.new <- df.new[df.new$value > 0, ]
df.new <- df.new[!is.na(df.new$value), ]
df.new <- df.new[df.new$variable %in% df.set.vals, ]

df.new <- df.new[df.new$variable %in% c("Flow.rate.sensor.1..ml.", "Preheating.top.3.temperature..Ā.C.", 
                                        "Preheating.bottom.3_1.temperature..Ā.C.", "N2.control..ppm."), ]

# Plot
p <- ggplot(data=df.new, aes(x=Running.in.moment, y=value, col=type)) +
     geom_point(size=1) +
     #geom_line() +
     scale_x_datetime() +
     scale_size_manual(values=c(1, 0.5)) +
     facet_wrap(~variable, scales = "free") +
     theme_bw() +
     theme(axis.text.y = element_text(size=7))

#pdf(file="pilt.pdf", width = 16, height = 10)
#png(file="pilt.png", width = 1024, height = 800)
print(p)
#dev.off()
