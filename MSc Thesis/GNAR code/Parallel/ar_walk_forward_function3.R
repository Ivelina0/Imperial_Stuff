## AR function ##

file1 <- "~/Imperial_College_London/Project code & data/CleanerCode/df_full.csv "
df_full <- read.csv(file = file1)
file2 <- "~/Imperial_College_London/Project code & data/CleanerCode/part_df.csv"
part_df <- read.csv(file = file2)
#dim(part_df)
#dim(df_full)
# indicators : CPI, CPI.Fuel, HP.London, BOE_Rate_tr,
## indicators : 
#### transf = 3 MOM
# RPI.all = 12
# RPI.Fuel.Light = 13
# Unempl.level = 15
# IOP.Prod.BE = 19
# IOS.Services = 65

###### transf = 1 Diff
# CPI.M.Rate = 68
# CPI.Fuel.Rate = 69

##### transf = 3 mom
# HP.London = 73
# HP.UK = 74

##### transf = log diff
# BoE = 76

transform.orignial <- function(dataFrame_pred, scale, tf, init_vec){
  DataFrame_pred <- (dataFrame_pred * sapply(scale, rep, dim(dataFrame_pred)[1]))
  
  ## Inverse Difference
  if(tf == 1){
    output <- diffinv(DataFrame_pred, lag=1, xi=init_vec)
    
    ## Diff(log) inverse BoE is only 1 vector
  }else if(tf == 2){
    xx <- log(init_vec)
    output_ <- diffinv(DataFrame_pred, lag=1, xi=xx)
    output <- exp(output_)
    
    ## MoM inverse
  }else if(tf == 3){
    output <- matrix(NA, nrow=(nrow(DataFrame_pred)+1), ncol=ncol(DataFrame_pred))
    output[1,]  <- init_vec
    rate_t <- DataFrame_pred/100 + 1
    x_tmins1 <- init_vec
    for(iii in 2:(nrow(DataFrame_pred)+1)){
      x_tmins1 <- output[iii-1,]
      output[iii,] <- rate_t[iii -1,] * x_tmins1
    }
  }
  return(output)
}


ar.walk.forward <- function(one_indx, trs, predPoints, how_far_back, how_far_forward){
  
  INDICATOR <- na.remove(df_full[, one_indx])
  tot_len_all <- length(part_df[,one_indx])
  start <- tot_len_all - length(INDICATOR) + 1
  indicatorReal <- part_df[start: tot_len_all,one_indx]
  
  #cat(length(INDICATOR))
  #cat(length(indicatorReal))
  
  totlen <- length(INDICATOR) 
  noOfFitPoints <- totlen - how_far_back  
  NoOfPred <- (noOfFitPoints+1):(noOfFitPoints+how_far_forward)
  M <- how_far_forward - predPoints + 1
  
  metrics <- matrix(rep(NA, (5* M)), 
                    nrow=M,
                    ncol=5)
  
  for(ppred in 1:M){
    
    # Train  & Test setup 
    dfTrain <- INDICATOR[1:noOfFitPoints]
    noOfFitPoints <- noOfFitPoints + 1 # stride 1
    
    indx_test <- NoOfPred[ppred:(ppred+predPoints-1)]
    dfRealTest <- indicatorReal[indx_test]
    
    # Fit & Predictions
    AR_fit <- arima(dfTrain, order = c(1,0,0))
    step_pred <- matrix(predict(AR_fit, predPoints)$pred, 
                        ncol=1, nrow =predPoints)
    
    ## Undo Transform
    Nstep_pred <- transform.orignial(dataFrame_pred = as.matrix(step_pred),
                                     scale= df_full_sd_mat[one_indx], 
                                     tf = 3, 
                                     init_vec = part_df[noOfFitPoints,one_indx])[-1]
    ## Forecast Metrics
    MSE <- sum( (Nstep_pred - dfRealTest)^2 )/predPoints
    metrics[ppred,1] <- (MSE)^(1/2)
    metrics[ppred,2] <- sum( abs(Nstep_pred - dfRealTest) )/predPoints
    metrics[ppred,3] <- BIC(AR_fit)
    metrics[ppred,4] <- sum( abs(dfRealTest - Nstep_pred)/dfRealTest * 100 )/predPoints
    #cat(" -- ")
    #cat(ppred)
    #cat(" INDX: ")
    #cat(indx_test)
    #cat(" Fitted points: ")
    #cat(noOfFitPoints)
    #cat(" Lengths: ")
    #cat(length(fitted(AR_fit)))
    #cat(" & ")
    #cat(length(dfTrain))
    #cat(" -- ")
    metrics[ppred,5] <- cor(fitted(AR_fit),dfTrain)^2 
  }
  colnames(metrics) <- c(paste("RMSE", colnames(df_full)[one_indx]),
                         paste("MAE", colnames(df_full)[one_indx]), 
                         paste("BIC", colnames(df_full)[one_indx]),
                         paste("MAPE", colnames(df_full)[one_indx]),
                         paste("R squared", colnames(df_full)[one_indx]))
  return(metrics)
}

test3 <- ar.walk.forward(one_indx = 12,
                         trs = 3,
                         predPoints = 3,
                         how_far_back = 102,
                         how_far_forward = 102)

######## RUNS h = 3 ########
# indic <- c(12, 13, 15, 19, 65, 68, 69, 73, 74, 76)
indic_mom <- c(12,13,15,19,65, 73,74) ## trans 3
inidc_diff <- c(68,69) ## trans 1
indic_logdiff <- 76 ### trans 2

test_mom <- lapply(indic_mom, ar.walk.forward, 
                   trs = 3,
                   predPoints = 3,
                   how_far_back = 102,
                   how_far_forward = 102)
write.csv(test_mom, "AR1_MoM_indicator_metrics5.csv")

test_diff <- lapply(inidc_diff, ar.walk.forward, 
                   trs = 1,
                   predPoints = 3,
                   how_far_back = 102,
                   how_far_forward = 102)
write.csv(test_diff, "AR1_Diff_indicator_metrics5.csv")

test_Logdiff <- ar.walk.forward(one_indx = 76,
                                trs = 2,
                                predPoints = 3,
                                how_far_back = 102,
                                how_far_forward = 102)
write.csv(test_Logdiff, "AR1_LOGDiff_indicator_metrics5.csv")



######## RUNS h = 6 ########

test_mom <- lapply(indic_mom, ar.walk.forward, 
                   trs = 3,
                   predPoints = 6,
                   how_far_back = 102,
                   how_far_forward = 102)
write.csv(test_mom, "AR1_MoM_indicator_metrics5_h6.csv")

test_diff <- lapply(inidc_diff, ar.walk.forward, 
                    trs = 1,
                    predPoints = 6,
                    how_far_back = 102,
                    how_far_forward = 102)
write.csv(test_diff, "AR1_Diff_indicator_metrics5_h6.csv")

test_Logdiff <- ar.walk.forward(one_indx = 76,
                                trs = 2,
                                predPoints = 6,
                                how_far_back = 102,
                                how_far_forward = 102)
write.csv(test_Logdiff, "AR1_LOGDiff_indicator_metrics5_h6.csv")

######## RUNS h = 12 ########
test_mom <- lapply(indic_mom, ar.walk.forward, 
                   trs = 3,
                   predPoints = 12,
                   how_far_back = 102,
                   how_far_forward = 102)
write.csv(test_mom, "AR1_MoM_indicator_metrics5_h12.csv")
test_diff <- lapply(inidc_diff, ar.walk.forward, 
                    trs = 1,
                    predPoints = 12,
                    how_far_back = 102,
                    how_far_forward = 102)
write.csv(test_diff, "AR1_Diff_indicator_metrics5_h12.csv")

test_Logdiff <- ar.walk.forward(one_indx = 76,
                                trs = 2,
                                predPoints = 12,
                                how_far_back = 102,
                                how_far_forward = 102)
write.csv(test_Logdiff, "AR1_LOGDiff_indicator_metrics5_h12.csv")





## PLOTS

test2[[1]]
length(test2)
boxplot(test2[[1]][,c(1,2,4)])
test2[[4]][,1]
mat <- matrix(c(test2[[1]][,1], test2[[2]][,1], 
                test2[[3]][,1], test2[[4]][,1], 
                test2[[5]][,1], test2[[6]][,1], 
                test2[[7]][,1], test2[[8]][,1],
                test2[[9]][,1], test2[[10]][,1]), ncol=10, nrow=100)
colnames(mat) <- sapply(test2, colnames)[1,]
mat
boxplot(mat, main="AR(1) Walk-Forward Forecasts with h=3, RMSE",las=2,cex = 0.65)

library(ggplot2)
#stack(as.data.frame(mat))
ggplot(stack(as.data.frame(mat)), aes(x = ind, y = values)) +
  geom_boxplot() +
  ggtitle("AR(1) Walk-Forward Forecasts with h=3, RMSE") +
  xlab("Indicators") + ylab("Root Mean Square Error") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



indicato <- na.remove(df_full[, 12])
indicato


AR_fit <- arima(dfTrain, order = c(1,0,0))
length(fitted(AR_fit))
fitted(AR_fit)#[-1]
dfTrain#[-1]
length(dfTrain)
cor( fitted(AR_fit)[-1], dfTrain[-1])
predict(AR_fit, predPoints)$pred

