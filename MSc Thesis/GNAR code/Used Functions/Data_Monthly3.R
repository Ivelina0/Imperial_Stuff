library("data.table")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("GNAR")
library("lubridate")
library("tseries")
#library("fields")
#library("forecast")
#library(xts)
#require(astsa)
library(zoo)
library(vars) 

#file_place <- "~/Thesis/Full Data/Original_Data.csv"
file_place <- "~/Imperial_College_London/Project code & data/CleanerCode/DATA/New Monthly Dataset/Original_Data.csv"

df <- read.csv(file = file_place)
dim(df) # 640  77
df[640,1] # 1969 MAR to 2022 JUN
df[1,1]
summary(df[,-1])
class(df)
dim(df) # 640 77

######### ######### ######### ######### ######### 
######### Missing Data ######### 
######### ######### ######### ######### ######### 
library(naniar)

## 238 when CPI starts, RPI.Fuel.Light, HP.UK, Unempl.level, IOP.Prod.BE all full

#gg_miss_var(df[239:640,-1], show_pct = TRUE)
#gg_miss_var(df[300:640,2:26], show_pct = TRUE)
df <- df[239:640,]
dim(df) # 402  77
df[,2:3]
gg_miss_var(df[,-c(1,3:30)], show_pct = TRUE)
gg_miss_var(df[,-c(1,31:77)], show_pct = TRUE)

df_sd <- sapply(df[,-1], sd, na.rm = TRUE)
df_sd
# Check
sd(df[,76], na.rm = TRUE)


######### ######### ######### ######### ######### 
######### FULL DATA Description #########
######### ######### ######### ######### ######### 
summary(df)
colSums(df==0,na.rm = TRUE)
# Data with 0s:
# TiG.CrudeOil.EU.Imp, TIG.CrudeOil.NonEU.Exp, 
# CPI.Fuel.Rate, # CPI.M.Rate

######### ######### ######### ######### ######### 
######### ACF, PACF Plots #########
######### ######### ######### ######### ######### 
acf.pcf.plots_indx <- function(indx_v){
  par(mfrow=c(3,2))
  #par(mar=c(2.1,2.1,1.5,1.1))
  par(mar=c(1.8,1.8,1.5,1.1))
  indx <- indx_v[1]
  indx.plus <- indx_v[2]
  
  data1 <- na.remove(df[,indx])
  data2 <- na.remove(df[,indx.plus])
  
  ts_no1 <- length(data1)
  ts_no2 <- length(data2)
  
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

dim(df) # 341  77 incl date
seqL <- list(c(2,3))
evens <- seq(4, 77, 2)

for(ev in evens){
  new_vec <- list(c(ev,(ev+1)))
  seqL <- append(seqL, new_vec)
}

lapply(seqL, acf.pcf.plots_indx)

######### ######### ######### ######### ######### 
######### EDA & Data Transformations ######### 
######### ######### ######### ######### ######### 
# transf=4 mom
# transf=1 diff
acf.pcf.transformed.variable <- function(indx, transf=1, h=3){
  par(mfrow=c(3,2))
  #par(mar=c(2.1,2.1,1.5,1.1))
  par(mar=c(1.8,1.8,1.5,1.1))
  
  
  data1 <- na.remove(df[,indx])
  x <- na.remove(df[,indx])
  # DIff
  if(transf == 1){
    data2 <- diff(x)
    transf <- "Diff"
  }
  # log  
  else if(transf == 2) {
    data2 <- log(x)
    transf <- "Log"
  }
  # log 1st difference 
  else if(transf == 3) {
    data2 <- diff(log(x))
    transf <- "Diff log"
  }
  # monthly rate of change
  else if(transf == 4) {
    data2 <- diff(x)/x[-length(x)] *100
    transf <- "MonM"
  }
  # monthly rate of change with h months apart
  else if(transf == 5) {
    data2 <- x[h:length(x)]/x[1:(length(x)-h+1)] * 100
    transf <- "MonM h apart"
  }
  # Log of monthly rate of change with h months apart
  else if(transf == 6) {
    data2 <- log(x[h:length(x)]/x[1:(length(x)-h+1)]) *100
    transf <- "Log MonM h apart"
  }
  # YoY rate of change
  else if(transf == 7) {
    data2 <- diff(x, 12)/x[-c((length( diff(x,12))+1):length(x))] *100
    transf <- "YoY"
  }  
  
  plot(data1, type='l',
       main = colnames(df)[indx] )
  plot(data2, type='l',
       main = paste(transf,colnames(df)[indx]) )
  
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
       main='PACF',
       ylim = c(-0.6,0.6))
  text(x = 60, y = 0.4, "PACF")
  
  pacf(data2, 
       lag.max = 100, 
       na.action = na.pass,
       main='PACF',
       ylim = c(-0.6,0.6))
  text(x = 60, y = 0.4, "PACF")
  
  par(mfrow=c(1,1))
  
  return(list(adf.test(data1), adf.test(data2)))
}

##### Data Column No. ##### 
### Data with 0s:
# CPI.Fuel.Rate = 8
# CPI.M.Rate = 2
# TiG.CrudeOil.EU.Imp = 53
# TIG.CrudeOil.NonEU.Exp = 50

# BoE gives 0s when differenced 
# not helpful in the model, take log(x).

## Note HP.Engl = 16, HP.London = 17, HP.UK =21, HP.Price.UK = 22
## have 2 NA values in the last two months of the series:
## in May and June

# No. 2 == log(x)
# No.4 == monthly rate of change
# No.5 == monthly rate of change with h months apart
# No.6 == log of no 5
# no.7 == YoY monthly

indxs <- 1:77
indxs <- indxs[-c(1,2,8,50,53)]
acf.pcf.transformed.variable(2, 1)
acf.pcf.transformed.variable(3, 4)

## MoM on all Rates & indexes but need outlier fixing
lapply(indxs, acf.pcf.transformed.variable)

## zeros with diff - change default to 1
zero_indxs <- c(2,8,50,53)
lapply(zero_indxs, acf.pcf.transformed.variable)
### Variables no. 

## IF p-value < 0.05, reject the null hypothesis.
## This means the time series is stationary.
## If p = 0.4943, fail to reject null. Series is not stationary.

######### ######### ######### ######### ######### 
######### Data & Cross Correlations ######### 
######### ######### ######### ######### ######### 

# Matrix of interactions between variables
m <- t(matrix(rep(1:3,3), ncol=3, nrow=3))
m
## start with row 1 = get diagonal index 
i <- 1
# m[i,i] == 1
m[i,]
#Cross Cor with the rest of the indices j in the row i 
for(j in m[i,]){
  if(i == j) next 
  indxs_two <- m[i,j]
  cat("--")
  cat(i, indxs_two)
  cat("--")
}

######### ######### ######### ######### ######### 
######### Data Transformations ######### 
######### ######### ######### ######### ######### 
## All index/rate with MoM
## Except the four series including 0s & BoE Rate
### Data transformations based on number

### Data with 0s:
# CPI.Fuel.Rate = 8
# CPI.M.Rate = 2
# TiG.CrudeOil.EU.Imp = 53
# TIG.CrudeOil.NonEU.Exp = 50

## Note HP.Engl = 16, HP.London = 17, HP.UK =21, HP.Price.UK = 22
## have 2 NA values in the last two months of the series:
## in May and June

## BoE ==15

data.transform <- function(x, trans){
  
  l_total <- length(x)
  x <- na.remove(x)
  l_nonNA <- length(x)
  Na_length <- l_total - l_nonNA + 1
  #cat(l_total)
  #cat(" -- ")
  #cat(l_nonNA)
  #cat(" -- ")
  #cat(Na_length)
  
  
  # first difference
  if(trans == 1){
    x_tran <- diff(x)
  }
  # log 1st difference 
  else if(trans == 2) {
    x_tran <- diff(log(x))
  }
  # monthly rate of change
  else if(trans == 3) {
    x_tran <- diff(x)/x[-length(x)] *100
  }
  # YoY rate of change
  else if(trans == 4) {
    x_tran <- diff(x, 12)/x[-c((length( diff(x,12))+1):length(x))] * 100
  }
  else if(trans == 5) {
    x_tran <- log(x)
    
  }
  
  NA_series <- rep(NA, Na_length)
  x_final <- c(NA_series, x_tran)
  #cat(" -- ")
  #cat(paste("x final length: ", length(x_final)))
  #cat(" -- ")
  return(x_final)
}
# test
#CPI_diff <- data.transform(df[,2],1)


### Variables without zeros and not HP variables
## Default function trans=3
cols <- c(1,2,8,15, 16, 17, 21, 22, 50,53)
df[1:2,cols]
non_zero_nonHP <- df[,-cols]
dim(non_zero_nonHP) # 402 67

### Monthly rate of change variables 
df_MoM <- lapply(non_zero_nonHP, data.transform, trans=3)
df_transformed1 <- as.data.frame(do.call(cbind, df_MoM))
dim(df_transformed1) # 402 67

#### Indicators with zeros transformation ####
## Default function trans=1 differenced
zero_indxs <- c(2,8,50,53)
zero_nonHP <- df[,zero_indxs]
df_Diff <- lapply(zero_nonHP, data.transform, trans=1)
df_transformed2 <- as.data.frame(do.call(cbind, df_Diff))
dim(df_transformed2) # 402  4
#colnames(df_transformed2)

#### BoE Rate transformation - log difference ####
## Default function trans=2 log diff
BoE_Rate <- df[,15]
BOE_Rate_tr <- data.transform(BoE_Rate,2)#[2:403]
length(BOE_Rate_tr) # 402 
#BOE_Rate_tr

#### Indicators with  NAs at the end: House Prices Indexes ####
## Default function trans=3

data.transform.two <- function(x, trans){
  
  l_total <- length(x)
  x <- na.remove(x)
  l_nonNA_front <- length(x) + 2
  Na_length <- l_total - l_nonNA_front + 1
  #cat(l_total)
  #cat(" -- ")
  #cat(l_nonNA_front)
  #cat(" -- ")
  #cat(Na_length)
  # first difference
  if(trans == 1){
    x_tran <- diff(x)
  }
  # log 1st difference 
  else if(trans == 2) {
    x_tran <- diff(log(x))
  }
  # monthly rate of change
  else if(trans == 3) {
    x_tran <- diff(x)/x[-length(x)] *100
  }
  # YoY rate of change
  else if(trans == 4) {
    x_tran <- diff(x, 12)/x[-c((length( diff(x,12))+1):length(x))] * 100
    
  }
  NA_series <- rep(NA, Na_length)
  l_nonNA_back <- rep(NA, 2)
  x_final <- c(NA_series, x_tran, l_nonNA_back)
  #cat(" -- ")
  #cat(paste("x final length: ", length(x_final)))
  #cat(" -- ")
  return(x_final)
}
# Test
#HP_Eng_Mom <-  data.transform.two(df[,16])

## Default function trans=3 MoM change
## HP indexs
HP_indxs <- c(16, 17, 21, 22)
HP_indxs_nonzero <- df[,HP_indxs]
#dim(HP_indxs_nonzero)
df_HP_Mom <- lapply(HP_indxs_nonzero, data.transform.two, trans = 3)
df_transformed4 <- as.data.frame(do.call(cbind, df_HP_Mom))
dim(df_transformed4) # 402  4


###### ###### Combine All transformed data ######  ###### 
df_transformed <- cbind(df_transformed1, df_transformed2, df_transformed4, BOE_Rate_tr ) # 402  75
dim(df_transformed) ## 402  76

###### ###### Get rid of outliers ######  ###### 
## Anything above the 10* IQR range is considered an outlier.
## The NAs in VAR should be replaced by the IGR * 10 value.
## Other methods like splines can be considered.

replace.outliers <- function(yy, val=10){
  
  quartiles <- quantile(yy, probs=c(.25, .75), na.rm = TRUE)
  IQR <- IQR(yy,na.rm = TRUE)
  
  Lower <- quartiles[1] - val*IQR
  Upper <- quartiles[2] + val*IQR
  #cat(Lower, Upper)
  # For GNAR
  #yy[yy < Lower] = NA
  #yy[yy > Upper] = NA
  
  # For VAR
  yy[yy < Lower] = Lower
  yy[yy > Upper] = Upper
  
  return(yy)
}

# 
#y <- df_transformed[,5]
#newY <- replace.outliers(y)
#length(newY)
## Transformed data and replaced outliers 
df_TO_ <- lapply(df_transformed, replace.outliers)
df_TO <- as.data.frame(do.call(cbind, df_TO_))
dim(df_TO) # 402  76


######  ###### Standardised dataset ###### ###### 
#df_full_sd <-  sapply(df_TO, sd, na.rm = TRUE)
#df_full_sd_mat <- as.matrix(unlist(df_full_sd))
#dim(df_full_sd_mat)
#df_full <- apply(df_TO, 2, function(x){x / sd(x, na.rm = TRUE)})
#summary(df_full)



######  ###### Standardised dataset without Outliers taken out ###### ###### 
df_full_sd <-  sapply(df_transformed, sd, na.rm = TRUE)
df_full_sd_mat <- as.matrix(unlist(df_full_sd))
dim(df_full_sd_mat)
df_full_sd_mat
#write.csv(df_full_sd_mat, "df_full_sd_mat.csv")
df_full <- apply(df_transformed, 2, function(x){x / sd(x, na.rm = TRUE)})
df_full
summary(df_full)

#write.csv(df_full,"df_full.csv", row.names = FALSE)
#write.csv(df_full_sd_mat,"df_full_sd_mat.csv", row.names = FALSE)


### Df organise
cols <- c(1,2,8,15, 16, 17, 21, 22, 50,53)
part_df1 <- df[,-cols]

zero_indxs <- c(2,8,50,53)
part_df2 <- df[,zero_indxs]

HP_indxs <- c(16, 17, 21, 22)
part_df3 <- df[,HP_indxs]

part_df4 <- df[,15]

part_df <- cbind(part_df1, part_df2, part_df3, part_df4 ) # 402  75
dim(part_df) ## 402  76

#write.csv(part_df,"part_df.csv", row.names = FALSE)









