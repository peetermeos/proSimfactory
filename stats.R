# This calculates the distributions for service times.
# We assume, that the service times are independent of items
# and randomly distributed following and unknown distribution,
# possibly exponential?

# Peeter Meos, Proespert
# 9. May 2017

calcStats <- function(df){
  ops <- unique(df$OPERATION)
  
  # Mean, standard dev and count
  ops.m <- 0
  ops.s <- 0
  
  ops.m.q <- 0
  ops.s.q <- 0
  
  ops.n <- 0
  
  for(i in 1:length(ops)){
    ops.n[i] <- nrow(df[df$OPERATION == ops[i], ])
    
    # Collect descriptives for processing time
    ops.m[i] <- mean(df$PASS1_ELAPSED_TIME[df1$OPERATION == ops[i]])
    if (ops.n[i] > 1) 
      ops.s[i] <- sd(df$PASS1_ELAPSED_TIME[df1$OPERATION == ops[i]])
    
    # Collect descriptives for queue time
    ops.m.q[i] <- mean(df$PASS1_ELAPSED_QUEUE_TIME[df1$OPERATION == ops[i]])
    if (ops.n[i] > 1) 
      ops.s.q[i] <- sd(df$PASS1_ELAPSED_QUEUE_TIME[df1$OPERATION == ops[i]])
  }  
  
  return(as.data.frame(cbind(as.character(ops), ops.n, ops.m, ops.s, ops.m.q, ops.s.q)))
}

