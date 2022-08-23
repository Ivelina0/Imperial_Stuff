# Models
# ARIMA vs VAR vs GNAR

## DATA AND PARAMETERS
dim(df.var) # 84 13
dim(df)
class(df.var)
df_var_colnames <- colnames(df.var)
dates <- df.var[,13]
df_var <-apply(df.var, 2, as.numeric)[,1:12]
# df_stand <- apply(df, 2, function(x){x / sd(x, na.rm = TRUE)})
# df_var_stand!
#lag <- 5
#MaxNeigh <- 1
#alphaV <- create.models(lag,MaxNeigh)[[1]]
#betaL <- create.models(lag,MaxNeigh)[[2]]

## FUNCTIONS
make_par_GNAR_list <- function(model_NO, Alpha_V, BetaL, Alphatype = TRUE){
  return(list(model_NO, Alpha_V[model_NO], BetaL[[model_NO]], Alphatype))
}
GNAR_par_list <- make_par_GNAR_list( 3, alphaV, betaL )

#df.raw[181,]

ts.modelForecast.plots <- function( start_plot, dat,
                                    dat_pred,
                                    data, 
                                    how_far_forward, 
                                    how_far_back,
                                    GNAR_params_list, 
                                    net_objec,
                                    indicator_no,
                                    VAR_lag,
                                    ARIMA_lag,
                                    start_var = 181,
                                    ARIMA_seas = 0,
                                    ARIMA_MA = 0){
  
  model_metrics <- matrix(rep(NA, 6),nrow=3, ncol=2)
  
  #################################################################
  # GNAR TEST
  #################################################################
  columns <- dim(data)[2] 
  totlen <- dim(data)[1]
  noOfFitPoints <- totlen - how_far_back  
  NoOfPred <- c((noOfFitPoints+1):(noOfFitPoints+how_far_forward))
  
  model <- GNAR_params_list[[1]]
  alphas <- GNAR_params_list[[2]]
  betas <- GNAR_params_list[[3]]
  Alphatype <- GNAR_params_list[[4]]
  
  fit <- GNARfit(vts = data[1:noOfFitPoints,], 
                 net = net_objec, 
                 alphaOrder = alphas, 
                 betaOrder = betas, 
                 globalalpha = Alphatype)
  
  predictions <- matrix(predict(fit, n=how_far_forward), 
                        ncol = columns, nrow = how_far_forward)
  
  
  # GDP RSS only
  #actual.GDP <- as.matrix(DATA[(noOfFitPoints+1):(noOfFitPoints+how_far_forward), indicator_no])
  actual.GDP <- data[NoOfPred, indicator_no]
  model_metrics[1,1] <- sum((predictions[,indicator_no] - actual.GDP)^2) 
  model_metrics[1,2] <- BIC(fit)
  
  #========================================================
  # Models 2 PRE-COVID PERIOD "2016 Q2" until "2019 Q1"
  #========================================================
  
  z.real <- zooreg(data[(start_plot):(noOfFitPoints+how_far_forward), indicator_no], 
                   start = as.yearqtr(dat), 
                   frequency = 4)
  
  z.pred <- zooreg(predictions[,indicator_no], 
                   start = as.yearqtr(dat_pred), 
                   frequency = 4)
  
  
  #========================================================
  # VAR same data; PRE-COVID PERIOD
  #========================================================
  
  var.fit <- VAR(data[(start_var):(noOfFitPoints),], p = VAR_lag)
  var.pred <- predict(var.fit, n.ahead = how_far_forward)
  var.gdp.Pred <-as.vector(var.pred$fcst$GDP.Chained.Volume.YoY[,1])
  model_metrics[2,1] <- sum((var.gdp.Pred - actual.GDP)^2)
  model_metrics[2,2] <- BIC(var.fit)
  var.gdp.Pred <- zooreg(as.vector(var.gdp.Pred), 
                         start = as.yearqtr(dat_pred), 
                         frequency = 4)
  
  #========================================================
  #Fitting the ARIMA Model to the time series
  #========================================================
  gdp <- data[1:noOfFitPoints, indicator_no]
  AR <- arima(gdp, order = c(ARIMA_lag, ARIMA_seas, ARIMA_MA))
  gdp.arima <- predict(AR, how_far_forward)
  z.pred.ar <- zooreg(as.vector(gdp.arima$pred), 
                      start = as.yearqtr(dat_pred), 
                      frequency = 4)
  
  
  model_metrics[3,1] <- sum((gdp.arima$pred - actual.GDP)^2)
  model_metrics[3,2] <- BIC(AR)
  ##
  colnames(model_metrics) <- c("RSS_GDP", "BIC")
  rownames(model_metrics) <- c("GNAR", "VAR", "AR")
  print(model_metrics)
  
  ### PLOTTING 
  main <- paste( "GNAR(", alphas, ", [", 
                 toString(betas), "])" ,
                 " vs AR(", ARIMA_lag, 
                 ") vs VAR(", 
                 VAR_lag, ") Forecasts for Period: ",
                 as.yearqtr(date(z.pred[1])), " - ",
                 as.yearqtr(date(z.pred[how_far_forward])),
                 sep="" )
  
  gnar_spec <- paste( "GNAR(", alphas, ", [", 
                      toString(betas), "])", sep="" )
  VAR_spec <- paste( "VAR(", VAR_lag, ")", sep="")
  AR_spec <- paste( "AR(", ARIMA_lag, ")", sep="")
  legend_spec <- c("Actual GDP YoY", gnar_spec, AR_spec,  VAR_spec  )
  
  smallest <- min(c(floor(min(z.real)), floor(min(z.pred)), floor(min(z.pred.ar)),
                    floor(min(var.gdp.Pred))))
  largest <- max(c(ceiling(max(z.real)), ceiling(max(z.pred)), ceiling(max(z.pred.ar)),
                   ceiling(max(var.gdp.Pred))))
  
  return(list(z.real, main, c(smallest, largest),
              z.pred, z.pred.ar, var.gdp.Pred,
              legend_spec, model_metrics))
}

### FULLLNET ###
df_dates[(185)]
df[181,]
GNAR_par_list <- make_par_GNAR_list( 4, alphaV, betaL )
plots <- ts.modelForecast.plots( start_plot=(185),
                                 dat = "2002-1",
                                 dat_pred= "2016-1",
                                 data = df,
                                 how_far_forward=12,
                                 how_far_back=24,
                                 GNAR_params_list= GNAR_par_list,
                                 net_objec= full_net,
                                 indicator_no= 1,
                                 VAR_lag=2,
                                 ARIMA_lag=2)
                                 #,start_var = 0)

#plots
par(mfrow=c(1,1))
par(mar=c(2.5,4.5,1.5,1.1))
plot(plots[[1]], type="l", 
     main = plots[[2]],
     ylab = "UK GDP YoY Growth",
     xlab = "Year-Quarter",
     ylim= plots[[3]])
  
lines(plots[[4]], col=2,  lwd=2)
lines(plots[[5]], col=3, lwd=2)
lines(plots[[6]], col=7, lwd=2)
legend(x = "topleft", 
       legend=plots[[7]],
       col=c(1, 2, 3,7), lty=c(1,1,1,1), cex=0.3, text.font=4)


#####################################################################
## Change the DATA - VAR ONLY DATA 
#####################################################################

### VAR DATASET ONLY
data_VAR <- df[181:nrow(df),]
df_VAR_dates <- df_dates[181:nrow(df)]
length(df_VAR_dates) == dim(data_VAR)[1]


GNAR_par_list <- make_par_GNAR_list( 22, alphaV, betaL )
GNAR_par_list[[3]]
plots <- ts.modelForecast.plots( start_plot=1,
                                 dat = "2002-1",
                                 dat_pred= "2016-1",
                                 data = df,
                                 how_far_forward=12,
                                 how_far_back=24,
                                 GNAR_params_list= GNAR_par_list,
                                 net_objec= netL[[3]],
                                 indicator_no= 1,
                                 VAR_lag=2,
                                 ARIMA_lag=2,
                                 start_var = 1)

#plots
par(mfrow=c(1,1))
par(mar=c(2.5,4.5,1.5,1.1))
plot(plots[[1]], type="l", 
     main = plots[[2]],
     ylab = "UK GDP YoY Growth",
     xlab = "Year-Quarter",
     ylim= plots[[3]])

lines(plots[[4]], col=2,  lwd=2)
lines(plots[[5]], col=3, lwd=2)
lines(plots[[6]], col=7, lwd=2)
legend(x = "topleft", 
       legend=plots[[7]],
       col=c(1, 2, 3,7), lty=c(1,1,1,1), cex=0.3, text.font=4)
#library(xtable)
plots[[8]]
xtable(plots[[8]], type = "latex")

#####################################################################
## Change the DATA - GNAR vs AR predicting 2007 GDP YoY
#####################################################################

### VAR DATASET ONLY
df_dates[185+4*7-1] # 212 "2009 Q4"
df_dates[264 - 52]
full_net <- igraphtoGNAR( make_full_graph(ncol(df)))

GNAR_par_list <- make_par_GNAR_list(54, alphaV, betaL )
#df_dates[185]
plots <- ts.modelForecast.plots( start_plot=185,
                                 dat = "2002-1",
                                 dat_pred= "2016-1",
                                 data = df,
                                 how_far_forward=12,
                                 how_far_back=24,
                                 GNAR_params_list= GNAR_par_list,
                                 net_objec= netL[[3]],
                                 indicator_no= 1,
                                 VAR_lag=1,
                                 ARIMA_lag=9)

#plots
par(mfrow=c(1,1))
par(mar=c(2.5,4.5,1.5,1.1))
plots[[2]]
plot(plots[[1]], type="l", 
     main = "GNAR(9, [1x7, 0x2]) vs AR(9) Forecasts for Period: 2009 Q1 - 2011 Q4",
     ylab = "UK GDP YoY Growth",
     xlab = "Year-Quarter",
     ylim= c(-2,2))

lines(plots[[4]], col=2,  lwd=2)
lines(plots[[5]], col=3, lwd=2)
#lines(plots[[6]], col=7, lwd=2)
#plots[[7]][1:3]
legend(x = "topleft", 
       legend=plots[[7]][1:3],
       col=c(1, 2, 3), lty=c(1,1,1), cex=0.3, text.font=4)
# HouseCrash_GNAR9_1x7_0x2_AR9_12stepahead
#library(xtable)
plots[[8]]
xtable(plots[[8]], type = "latex")

#####################################################################
## Change the DATA - VAR ONLY DATA no HP
#####################################################################

### VAR DATASET ONLY
data_VAR_noHP <- df[181:nrow(df),-c(8,9,10,11)]
df_VAR_dates <- df_dates[181:nrow(df)]
length(df_VAR_dates) == dim(data_VAR)[1]
# Networks
full_net <- igraphtoGNAR( make_full_graph(ncol(data_VAR_noHP)))
#rand.graphsNo <- 10
#seedNoV <- sample(1:100, rand.graphsNo, replace=F)
#netL <- lapply(seedNoV, seedToNet, nnodes=columns ,graph.prob = prob.edges)

## TEST FIXED PERIOD
FullNet_models <- model.metrics(net_obj=full_net, 
                                alpha_V=alphaV,
                                beta_L=betaL, 
                                DATA=data_VAR_noHP,
                                how_far_back=24,
                                how_far_forward=12,
                                alphaType=TRUE,
                                indic_specific_metrics=c(1,2))
FullNet_models
min(FullNet_models[,3])
which.min(FullNet_models[,6]) # 52 smallest GDP pred
## PLOT

GNAR_par_list <- make_par_GNAR_list( 52, alphaV, betaL )

plots <- ts.modelForecast.plots( start_plot=1,
                                 dat = "2002-1",
                                 dat_pred= "2016-1",
                                 data = data_VAR_noHP,
                                 how_far_forward=12,
                                 how_far_back=24,
                                 GNAR_params_list= GNAR_par_list,
                                 net_objec= full_net,
                                 indicator_no= 1,
                                 VAR_lag=1,
                                 ARIMA_lag=9,
                                 start_var = 1)

#plots
par(mfrow=c(1,1))
par(mar=c(2.5,4.5,1.5,1.1))
plot(plots[[1]], type="l", 
     main = plots[[2]],
     ylab = "UK GDP YoY Growth",
     xlab = "Year-Quarter",
     ylim= plots[[3]])

lines(plots[[4]], col=2,  lwd=2)
lines(plots[[5]], col=3, lwd=2)
lines(plots[[6]], col=7, lwd=2)
legend(x = "topleft", 
       legend=plots[[7]],
       col=c(1, 2, 3,7), lty=c(1,1,1,1), cex=0.3, text.font=4)

#####################################################################
## Change the indicators
#####################################################################
#lag <- 5
#MaxNeigh <- 1
#alphaV <- create.models(lag,MaxNeigh)[[1]]
#betaL <- create.models(lag,MaxNeigh)[[2]]

## DATA
colnames(df_var_stand)

# Networks
full_net <- igraphtoGNAR( make_full_graph(ncol(df)))
#rand.graphsNo <- 10
#seedNoV <- sample(1:100, rand.graphsNo, replace=F)
netL <- lapply(seedNoV, seedToNet, nnodes=columns ,graph.prob = prob.edges)


GNAR_par_list <- make_par_GNAR_list( 7, alphaV, betaL )
plots <- ts.modelForecast.plots( start_plot=185,
  #start_plot=1, VAR data
                                 dat = "2001-1",
                                 dat_pred= "2016-1",
                                 data = df_var_stand,
                                 how_far_forward=32,
                                 how_far_back=6,
                                 GNAR_params_list= GNAR_par_list,
                                 net_objec= netL[[3]],
                                 indicator_no= 1,
                                 VAR_lag=2,
                                 ARIMA_lag=1, start_var = 0)

#par(mar=c(2.5,4.5,1.5,1.1))
plot(plots[[1]], type="l", 
     main = plots[[2]],
     ylab = "UK GDP YoY Growth",
     xlab = "Year-Quarter",
     ylim= plots[[3]])

lines(plots[[4]], col=2,  lwd=2)
lines(plots[[5]], col=3, lwd=2)
lines(plots[[6]], col=7, lwd=2)
legend(x = "topleft", 
       legend=plots[[7]],
       col=c(1, 2, 3,7), lty=c(1,1,1,1), cex=0.3, text.font=4)



df[(noOfFitPoints+1):(noOfFitPoints+how_far_forward), 1]










