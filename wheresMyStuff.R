hlibrary(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

# Helper function just in case
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

# df.prodist tuleb asju metamorfeerunud SFCd välja võtta

df.tmp <- df.sfc.order.hist.wip
df.tmp$child <- gsub("^.*,","", as.character(df.tmp$SFC_BO))

# Võtame välja need read, kus SFC on muundunud iseendaks
df.tmp <- df.tmp[as.character(df.tmp$child) != as.character(df.tmp$SFC), ]

# Parentis on need SFCd, mis on transformeerunud, st sellised, mida enam ei tohiks eksisteerida
parent <- unique(as.character(df.tmp$SFC))

result <- df.prod[as.character(df.prod$SFC) %not in% parent,] %>% 
#result <- df.prod %>%   
  group_by(SFC) %>%
  filter(ODS_DATE_TIME == max(ODS_DATE_TIME)) %>%
  arrange(SFC)

result$STEP_ID <- factor(result$STEP_ID)


df.p <- result[result$SHOP_ORDER %in% unique(result$SHOP_ORDER)[10:120],] 
df.p$n <- 1
df.p <- aggregate(n~STEP_ID+OPERATION+SHOP_ORDER, df.p, FUN="length")
df.p <- df.p[order(df.p$SHOP_ORDER),]

# df.p.sum <- aggregate(n~SHOP_ORDER, df.p, FUN="sum")
# names(df.p.sum) <- c("SHOP_ORDER", "TOTAL")
# df.p <- merge(df.p, df.p.sum, by=c("SHOP_ORDER"))
# df.p$percent <- df.p$n / df.p$TOTAL

p <- ggplot(data=df.p, aes(x=SHOP_ORDER, y=OPERATION, size=n, col=OPERATION)) +
     geom_point() +
     #geom_bar(position = "stack") +
#     scale_y_continuous(breaks = pretty_breaks(20)) +
     #facet_grid(STEP_ID~.) +
     scale_size(name="Count") +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
     ggtitle("What is the order status")

print(p)