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
require(tseries); require(astsa)
library(zoo)
library(vars) 



###########################################
## DATA YoY Quarterly Indicators
###########################################

upload.clean.csv <- function(cs_vstr_file = 'UK_Macroeconomics_Variables_TS_YoY.csv'){
  Data1 <- read.csv(file = cs_vstr_file)
  # A slight problem with BOP
  Data1$BOP.YoY <- as.numeric(Data1[,2])
  #dim(Data1) #  265  16
  # Get rid of Dates, BOP-error; CPI
  df <- Data1[,c(3,5:16)]
  #dim(df) # 265  13
  dates <- unique(Data1$DATE) # 269 quarters
  
  ########## Scaling Data #############
  #cat(summary(df))
  
  df <- apply(df, 2, function(x){x / sd(x, na.rm = TRUE)})
  #summary(df)
  
  ## DROP TOTAL SHARE PRICES- not sure how data is collected.
  head(df,2)
  df <- df[,-6] 
  
  return(df)
}
newdata <- upload.clean.csv()
head(newdata,2)


########## MISSING DATA % ############
# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
library(naniar)
gg_miss_var(data.frame(df), show_pct = TRUE)