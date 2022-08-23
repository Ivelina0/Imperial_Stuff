MDirAcc <- function(Actual, Forecast, lag=1) {
  return( mean(sign(diff(Actual, lag=lag))==sign(diff(Forecast, lag=lag))) )
}

