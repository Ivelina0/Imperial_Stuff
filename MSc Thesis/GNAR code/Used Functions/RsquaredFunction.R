# Source: https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r


Rsquared <- function(predicted, true){
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  return(R_square)
}



