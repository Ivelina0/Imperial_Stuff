library("data.table")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("GNAR")
library("lubridate")
library("tseries")
library("fields")
library("forecast")
library(xts)

###########################################
## Exploratory Data Analysis - ACF, PACF etc.
###########################################

######### TS plots #########
par(mfrow=c(3,2))
par(mar=c(2.1,2,1.5,1.1))

## ADD: DATE AXIS & RECESSIIN PERIOD GREY SHADED
# Transformations
#data <- diff(na.remove(df[,i]))
#data <- diff(na.remove(df[,i]), 4)
#data <- diff(diff(na.remove(df[,i]), 4))
#data <- diff(log(na.remove(df[,i])))
#data <- log(na.remove(df[,i])+0.1)
###############  Plot Function #######################

acf.pcf.plots <- function(indx){
  par(mfrow=c(3,2))
  par(mar=c(2.1,2,1.5,1.1))
  indx.plus <- indx+1
  data1 <- na.remove(df[,indx])
  data2 <- na.remove(df[,indx.plus])
  
  plot(data1, type='l',
       main = colnames(df)[indx] )
  plot(data2, type='l',
       main = colnames(df)[indx.plus] )
  
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
}

single.data.acf.pcf <- function(indx){
  par(mfrow=c(3,1))
  par(mar=c(2.1,2,1.5,1.1))
  
  data1 <- na.remove(df[,indx])
  plot(data1, type='l',
       main = colnames(df)[indx] )
  acf(data1, 
      lag.max = 100, 
      na.action = na.pass,
      main='ACF')
  text(x = 60, y = 0.4, "ACF")
  pacf(data1, 
       lag.max = 100, 
       na.action = na.pass,
       main='PACF')
  text(x = 60, y = 0.4, "PACF")
  par(mfrow=c(1,1))}

odds <- seq(1, 12, 2)
acf.pcf.plots(odds[7])
single.data.acf.pcf(13)


###########################################
## ACF relationships
###########################################

#head(df,2)

par(mfrow=c(1,1))
data1 <- na.remove(df[,1])
data2 <- na.remove(df[,2])
ccf(data1, data2, lag.max = 30, ylab = "cross-correlation")

acf.pcf.ccp.plots <- function(indx){
  
  par(mfrow=c(3,3))
  par(mar=c(2.1,2,1.5,1.1))
  
  indx.plus <- indx:(indx+2)
  
  for(ii in indx.plus){
    data1 <- na.remove(df[,ii])
    acf(data1, 
        lag.max = 100, 
        na.action = na.pass,
        main='ACF')
    text(x = 60, y = 0.4, paste("ACF",colnames(df)[ii]))
    
    pacf(data1, 
         lag.max = 100, 
         na.action = na.pass,
         main='PACF')
    text(x = 60, y = 0.4, paste("PACF", colnames(df)[ii]))
    
    data2 <- na.remove(df[,(ii+1)])
    ccf(data1, data2, lag.max = 30, ylab = "cross-correlation")
    
    text(x = 0, y = 0.1, paste("Node", ii, "&", (ii+1)))
  }
  
  par(mfrow=c(1,1))
}
acf.pcf.ccp.plots(10)

