##==== Parallel Computing ====##
# R provides a number of convenient facilities for parallel computing. 
# The following method shows you how to setup and run a parallel process on your current multi-core device, without need for additional hardware.

# 1. Prior Set-up

# Registering cores for parallel process
library(doSNOW)
cl <- makeCluster(4, type="SOCK") # 4 - number of cores
registerDoSNOW(cl) # Register back end Cores for Parallel Computing

# 2. Let's run it

# In the examples below, we can replace %dopar% with %do% to make it run as a non-parallel process.

library(foreach)
foreach(i = 1:28) %dopar% {sqrt(i)} # example 1

# returned output values of the parallel process are combined using 'c()' function
foreach(i = 1:28,.combine = "c") %dopar% {sqrt(i)} # example 2

# returned output values of the parallel process are combined using 'cbind()' function
foreach(i = 1:28,.combine = "cbind") %dopar% {letters[1:4]} # example 3 

# combine using your custom defined function: "myCustomFunc()" and store in 'output' variable
output <- foreach(i = 1:28, .combine = "myCustomFunc") %dopar% {
  sqrt(i)
}

# 3. Typical Parallel computing Cdoe

allRowIndices <- c(1:nrow(inputData)) # assign row indices of inputData, that will be processed in parallel

output <- foreach (rowNum = allRowIndices, .combine = rbind, .packages = c("caret", "ggplot2", "Hmisc")) %dopar% {
  # code to process each rowNum goes within this block.
  # 'n' rows will be processed simultaneously, where 'n' is number of registered cores.
  # after processing all rows, the returned value is combined using the function defined in `.combine` argument `rbind` in this case. The output thus aggregated is stored in output variable.
  # Finally, the packages required by functions in this block has to be mentioned within .packages argument.
}
stopCluster(cl) # undo the parallel processing setup

##### or #####
cl <- makeCluster(4, type="SOCK") # 4 - number of cores
registerDoSNOW(cl) # Register Backend Cores for Parallel Computing
allRowIndices <- c(1:nrow(inputData)) # row numbers of inputData, that will be processed in parallel
output_parallel <- foreach (rowNum = allRowIndices, .combine = c) %dopar% {
  calculatedOutput <- inputData[rowNum, 1] * inputData[rowNum, 2] + inputData[rowNum, 3] / inputData[rowNum, 4] # compute output
  return (calculatedOutput)
}
}