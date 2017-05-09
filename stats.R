# This calculates the distributions for service times.
# We assume, that the service times are independent of items
# and randomly distributed following and unknown distribution,
# possibly exponential?

# Peeter Meos, Proespert
# 9. May 2017

# Calculates operation - resources tuples with probabilities
calcResources <- function(df){
  df1 <- df[, c("OPERATION", "RESRCE")]
  
  df1 <- unique(df1)
  df1 <- df1[order(df1$OPERATION, df1$RESRCE), ]
  
  return(df1)
}

calcStats <- function(df){
  ops <- unique(df$RESRCE)
  
  # Mean, standard dev and count
  ops.m <- 0
  ops.s <- 0
  
  ops.m.q <- 0
  ops.s.q <- 0
  
  ops.n <- 0
  
  for(i in 1:length(ops)){
    ops.n[i] <- nrow(df[df$RESRCE == ops[i], ])
    
    # Collect descriptives for processing time
    ops.m[i] <- mean(df$PASS1_ELAPSED_TIME[df1$RESRCE == ops[i]])
    if (ops.n[i] > 1) 
      ops.s[i] <- sd(df$PASS1_ELAPSED_TIME[df1$RESRCE == ops[i]])
    
    # Collect descriptives for queue time
    ops.m.q[i] <- mean(df$PASS1_ELAPSED_QUEUE_TIME[df1$RESRCE == ops[i]])
    if (ops.n[i] > 1) 
      ops.s.q[i] <- sd(df$PASS1_ELAPSED_QUEUE_TIME[df1$RESRCE == ops[i]])
  }  
  
  r <- as.data.frame(cbind(as.character(ops), ops.n, ops.m, ops.s, ops.m.q, ops.s.q))
  names(r)[1] <- "res"
  
  r[,-1] <- as.numeric(as.character(r[,-1]))
  
  return(r)
}

