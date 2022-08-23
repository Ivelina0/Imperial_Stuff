## Params
lag <- 3
MaxNeigh <- 1
totlen <- dim(df.var)[1]
#rand.graphsNo <- 10
alphaV <- create.models(lag,MaxNeigh)[[1]]
betaL <- create.models(lag,MaxNeigh)[[2]]
length(betaL) == length(alphaV)
#seedNoV <- sample(1:100, rand.graphsNo, replace=F)
## NETS
prob.edges <- 0.8
seedNoV <- sample(1:100, rand.graphsNo, replace=F)
seeeed <- seedNoV[2]
columns <- dim(DATA)[2] # 12
net.full <- igraphtoGNAR( make_full_graph(columns))
net2 <- seedToNet(seed.no = seeeed,
                  nnodes = columns, 
                  graph.prob = prob.edges)
# seed_List not used atm

GNAR.rolling.forecast <- function(net_obj, alpha_V, beta_L,
                                  #seedNo_V,
                                  DATA,
                                  PredPoints,
                                  how_far_back,
                                  how_far_forward,
                                  #DATA=df.var,
                                  alpha_Type){
  
  ## INIT DATA NEEDED:
  PredPoints <- 1
  columns <- dim(DATA)[2] # 12
  totlen <- dim(DATA)[1] # 84
  noOfFitPoints <- totlen - how_far_back  # 60
  NoOfPred <- (noOfFitPoints+1):(noOfFitPoints+how_far_forward)
  metrics <- matrix(rep(NA, (length(alpha_V) * 3)), 
                    nrow=length(alpha_V), 
                    ncol=3)
  
  
  for(mmod in 1:length(alpha_V)){
    
    # Init Data Frames
    noOfFitPoints <- totlen - how_far_back  # 60
    dfTrain <- DATA[1:noOfFitPoints,]
    dfRealTest <- DATA[NoOfPred,]
    # Memory
    ForecastValues <- matrix(rep(NA, (columns*how_far_forward)), 
                             ncol = columns, 
                             nrow = how_far_forward) # 12
    
    ## Rolling Forecast
    for(pp in 1:how_far_forward){
      
      # Init Data Frames
      dfTrain <- DATA[1:noOfFitPoints,]
      noOfFitPoints <- noOfFitPoints + PredPoints
      
      # fit
      gnar.fit <- GNARfit(vts = dfTrain, 
                          net = net_obj,
                          alphaOrder = alpha_V[mmod],
                          betaOrder = beta_L[[mmod]],
                          globalalpha = alpha_Type)
      # Prediction indicators
      ForecastValues[pp,] <- t(matrix(predict(gnar.fit, n=PredPoints)))
      #cat(t(matrix(predict(gnar.fit, n=PredPoints))))
      
    }
    MSE <- sum( (ForecastValues - dfRealTest)^2 )/how_far_forward
    metrics[mmod,1] <- (MSE)^(1/2)
    metrics[mmod,2] <- sum( abs(ForecastValues - dfRealTest) )/how_far_forward
    metrics[mmod,3] <- MDirAcc(dfRealTest, ForecastValues)
    #metrics[mmod,4] <- seedNo_V
    
  }
  # set params
  parNames <-  NULL
  for (jj in 1:length(alpha_V)) {
    parNames <- c(parNames, paste( "GNAR(", alpha_V[jj], ", [", toString(beta_L[[jj]]), "])" , sep="" ))
  }
  #metrics <- cbind(metrics, seeeed)
  rownames(metrics) <-  parNames
  #colnames(metrics) <- c("RMSE", "MAE", "MDA", "NetSeed")
  colnames(metrics) <- c("RMSE", "MAE", "MDA")
  
  return(metrics)
}
#df.var[1:2 , 1:(dim(df.var)[2]-1)]
#head(df.var[ , 1:(dim(df.var)[2]-1)],3)
# Test 1 Run - 1 NET, DIFF models

# Parameters
lag <- 3
MaxNeigh <- 1
alphaV <- create.models(lag,MaxNeigh)[[1]]
betaL <- create.models(lag,MaxNeigh)[[2]]
length(alphaV) == length(betaL) 

# Networks
full.net <- igraphtoGNAR( make_full_graph(ncol(df)))

mat.test <- GNAR.rolling.forecast(net_obj=full.net,
                                  alpha_V=alphaV,
                                  beta_L=betaL,
                                  #seedNo_V=seeeed,
                                  DATA=df,
                                  PredPoints=1,
                                  how_far_back=24,
                                  how_far_forward=24,
                                  alpha_Type=TRUE)
mat.test
min(mat.test[,1]) # GNAR(2, [0, 0])  3.182450 3.993818 0.6123188

## Multi Run
prob.edges
netL <- lapply(seedNoV, seedToNet, nnodes=columns ,graph.prob = prob.edges)
results <- lapply(netL, GNAR.rolling.forecast, 
                  alpha_V=alphaV, 
                  beta_L=betaL,
                  DATA=df,
                  PredPoints=1,
                  how_far_back=24,
                  how_far_forward=24,
                  alpha_Type=TRUE)
results
min(sapply(results, function(x) x[, 1]))
# GNAR(2, [1, 1])   3.179710 NET 2 (NET 9 also close)
# GNAR(2, [1, 1])    3.179710 4.050092 0.6086957

NEt2adj <- as.matrix(GNARtoigraph(netL[[2]]))
NEt9adj <- as.matrix(GNARtoigraph(netL[[9]]))
NEt2adj
NEt9adj
NEt2adj - NEt9adj


colnames(DATA)
