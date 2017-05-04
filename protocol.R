library(dplyr)

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
df.set <- df.set %>% select_if(colSums(!is.na(.)) > 0) %>% select_if(colSums(. != 0) > 0) 
