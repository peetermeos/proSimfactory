library(ggplot2)
library(scales)

p <- ggplot(data=df.prod, aes(x=PASS1_ELAPSED_TIME, fill=RESRCE)) +
     geom_histogram() +
     scale_x_continuous(labels=function(x)x/1000) +
     scale_fill_hue(l=40) +
     facet_wrap(~OPERATION, scales = "free") +
     theme_bw()

print(p)
