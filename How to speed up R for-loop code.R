##==== Let's Speed-up R Code ====##
#This chapter shows a number of approaches including simple tweaks to logic design, parallel processing and Rcpp, 
# increasing the speed by orders of several magnitudes, so you can comfortably process data as large as 100 Million rows and more.

# 1. Example 
# Create the data frame
col1 <- runif (12^5, 0, 2)
col2 <- rnorm (12^5, 0, 2)
col3 <- rpois (12^5, 3)
col4 <- rchisq (12^5, 2)
df <- data.frame (col1, col2, col3, col4)
df
# So, The logic we are about to optimize:
# For every row on this data frame df, check if the sum of all values is greater than 4. 
# If it is, a new 5th variable gets the value greater_than_4, else, it gets lesser_than_4.

system.time({
  for (i in 1:nrow(df)) {
    if ((df[i,"col1"] + df[i, "col2"] + df[i, "col3"] + df[i, "col4"]) > 4){
      df[i,5] <- "greater_than_4"
    } else {
      df[i,5] <- "lesser_than_4"
    }
  }
}
  
)

# Method 1 : Vectorize and Pre-allocate

# After vectorization and pre-allocation
output <- character (nrow(df)) # initialize output vector
system.time({
  for (i in 1:nrow(df)) {
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) {
      output[i] <- "greater_than_4"  # assign to vector 
    } else {
      output[i] <- "lesser_than_4"
    }
  }
  df$output <- output  # finally assign to data frame
})

# Method 2: Check what you want to see before running the loop
# After vectorization and pre-allocation, taking the condition checking outside the loop.
output <- character (nrow(df))
condition <- (df$col1 + df$col2 + df$col3 + df$col4) > 4  # condition check outside the loop
system.time({
  for (i in 1:nrow(df)) {
    if (condition[i]) {
      output[i] <- "greater_than_4"
    } else {
      output[i] <- "lesser_than_4"
    }
  }
  df$output <- output
})


# Method 3: Use ifelse() whenever possible!!

system.time({
  output <- ifelse ((df$col1 + df$col2 + df$col3 + df$col4) > 4, "greater_than_4", "lesser_than_4")
  df$output <- output
})

# Method 4: Use which() whenever possible!!

system.time({
  want = which(rowSums(df) > 4)
  output = rep("less than 4", times = nrow(df))
  output[want] = "greater than 4"
}) 

# Method 5: Use parallel processing if you have a multicore machine.



