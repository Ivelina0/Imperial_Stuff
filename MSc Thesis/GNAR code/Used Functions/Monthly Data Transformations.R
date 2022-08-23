## df_var
library(tseries)

# Standardised dataset
df <- apply(df[,-1], 2, function(x){x / sd(x, na.rm = TRUE)})
dim(df) # 277  26
df[277,]
# Taking the log difference of variables
## ACF, PACF Plots
acf.pcf.transformed.variable <- function(indx, transf=3){
  par(mfrow=c(3,2))
  #par(mar=c(2.1,2.1,1.5,1.1))
  par(mar=c(1.8,1.8,1.5,1.1))
  

  data1 <- na.remove(df[,indx])
  x <- na.remove(df[,indx])
  
  if(transf == 1){
  data2 <- diff(x)
  }
  # log 1st difference 
  else if(transf == 2) {
    x <- x+0.01
    data2 <- diff(log(x))
  }
  # monthly rate of change
  else if(transf == 3) {
    x <- x+0.01
    data2 <- diff(x)/x[-length(x)]
  }
  # YoY rate of change
  else if(transf == 4) {
    x <- x+0.01
    data2 <- diff(x, 12)/x[-c((length( diff(x,12))+1):length(x))]
  }  
  
  #print("Original data test", adf.test(data1))
  #print("Transformed data test", adf.test(data2))
  
  plot(data1, type='l',
       main = colnames(df)[indx] )
  plot(data2, type='l',
       main = paste("Transformed ",colnames(df)[indx]) )
  
  acf(data1, 
      lag.max = 100, 
      na.action = na.pass,
      main='ACF')
  text(x = 60, y = 0.4, "ACF")
  acf(data2, 
      lag.max = 100, 
      na.action = na.pass,
      main='ACF')
  text(x = 60, y = 0.4, "ACF")
  
  pacf(data1, 
       lag.max = 100, 
       na.action = na.pass,
       main='PACF')
  text(x = 60, y = 0.4, "PACF")
  
  pacf(data2, 
       lag.max = 100, 
       na.action = na.pass,
       main='PACF')
  text(x = 60, y = 0.4, "PACF")
  
  par(mfrow=c(1,1))
  
  return(list(adf.test(data1), adf.test(data2)))
}


acf.pcf.transformed.variable(1, 1)

## IF p-value < 0.05, reject the null hypothesis.
## This means the time series is stationary.

#Since the p-value is not less than .05, we fail to reject the null hypothesis.
# if p = 0.4943, accept null. 

#This means the time series is non-stationary. In other words, 
#it has some time-dependent structure and does not have constant variance over time.
x <- na.remove(df[,1])
x <- x+0.0001
plot(x, type="l")
x
diff(x)/x[-length(x)]
