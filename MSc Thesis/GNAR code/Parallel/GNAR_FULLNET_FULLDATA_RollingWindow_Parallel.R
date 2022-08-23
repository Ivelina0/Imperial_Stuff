## Running forward rolling with 
## -- FullNet 
## -- From 2013 DEC until  2022 Mar
## -- 3 step ahead
## -- Max lag is 22

## Need functions 
# transform.orignial
# Get.Real.Values
# create.models
# igraph, GNAR : make_full_graph; igraphtoGNAR
# f 

file_place1 <- "~/Thesis/Full Data/df_full.csv"
df_full <- read.csv(file = file_place1)
file_place2 <- "~/Thesis/Full Data/part_df.csv"
part_df <- read.csv(file = file_place2)
file_place3 <- "~/Thesis/Full Data/df_full_sd_mat.csv"
df_full_sd_mat <- read.csv(file = file_place3)
df_full_sd_mat <- t(df_full_sd_mat)

dim(df_full)
dim(part_df)

lag <- 5
MaxNeigh <- 1
mods <- create.models(lag,MaxNeigh)
full_net <- igraphtoGNAR( make_full_graph(ncol(df_full)))

f <- function(i) list(mods[[1]][i], mods[[2]][[i]])
parss <- lapply(1:length(mods[[1]]), f )
length(parss) # 275
#parss[[9]][[1]]
#parss[[9]][[2]]

indic <- c(12, 13, 15, 19, 65, 68, 69, 73, 74, 76)
results <- lapply(parss , N.step.ahead, 
                  net_objec= full_net, 
                  pred_points=3, 
                  how_far_back= 102, 
                  how_far_forward= 102, 
                  data= as.matrix(df_full), 
                  original_data = part_df,
                  alpha_type= TRUE, 
                  indic_specific_metrics= indic)

lapply(results, colMeans)

library(parallel)
#library(lme4)

numCores <- detectCores()
numCores

cl <- makeCluster(numCores)  #This line will take time

#clusterExport(cl)

## Load packages to each cluster
clusterEvalQ(cl, {
  library(igraph)
  library(GNAR)
  library(data.table)
  library(dplyr)
  library(tseries)
  library(zoo)
})

#clusterEvalQ(cl, getwd( ))

clusterEvalQ(cl, {
  file_place1 <- "~/Thesis/Full Data/df_full.csv"
  df_full <- read.csv(file = file_place1)
  file_place2 <- "~/Thesis/Full Data/part_df.csv"
  part_df <- read.csv(file = file_place2)
  file_place3 <- "~/Thesis/Full Data/df_full_sd_mat.csv"
  df_full_sd_mat <- read.csv(file = file_place3)
  df_full_sd_mat <- t(df_full_sd_mat)
})



clusterEvalQ(cl, dim(df_full))

clusterEvalQ(cl, {
  createModels <- function(lag, NeighB.Max = 1){
    
    ## Required packages
    #library(igraph)
    #library(GNAR)
    #lag =1 , Neigh = 1
    if(lag == 1){
      ll <- 1
      
      ### Create alphas
      alphas <- rep(ll, (ll+1) )
      
      ### Create betas
      zeros <- rep(0, ll)
      mat <- matrix(1, ll, ll)
      mat[lower.tri(mat)] <- 0
      newmat<-cbind(zeros, mat)
      colnames(newmat) <- 1:(ll+1)
      betas <- list( newmat[1], newmat[2]  )
    }
    
    else{
      ll <- 1
      
      ### Create alphas
      alphas <- rep(ll, (ll+1) )
      
      ### Create betas
      zeros <- rep(0, ll)
      mat <- matrix(1, ll, ll)
      mat[lower.tri(mat)] <- 0
      newmat<-cbind(zeros, mat)
      colnames(newmat) <- 1:(ll+1)
      betas <- list( newmat[1], newmat[2]  )
      for(ll in 2:lag){ #lag >1 , Neigh = 1
        alphas <- c(alphas , rep(ll, (ll+1) ) )
        zeros <- rep(0, ll)
        mat <- matrix(1, ll, ll)
        mat[lower.tri(mat)] <- 0
        newmat<-cbind(zeros, mat)
        colnames(newmat) <- 1:(ll+1)
        beta2 <- lapply(seq_len(ncol(newmat)), function(i) newmat[,i])
        betas <- append(betas, beta2)
      }
    }
    
    if(NeighB.Max > 1){
      if(lag == 1){ # lag = 1, Neigh > 1
        for(nn in 2:NeighB.Max){
          ### Create alphas
          ll <- 1
          alphas.add <- rep(ll, (ll) )
          
          ### Create betas' rth stage neigh for lag 1 alpha
          mat.add <- matrix(1, ll, ll)
          mat.add[lower.tri(mat.add)] <- 0
          colnames(mat.add) <- 1:(ll)
          betas.add <- list( mat.add[1]*nn )
          # Add to lists
          alphas <- c(alphas, alphas.add)
          betas <- append(betas, betas.add)
        }
      }
      else{ # lag > 1, Neigh > 1
        for(nn in 2:NeighB.Max){
          ### Create alphas lag 1, Neigh nn
          ll <- 1
          alphas.add <- rep(ll, (ll) )
          
          ### Create betas' rth stage neigh for lag 1 alpha
          mat.add <- matrix(1, ll, ll)
          mat.add[lower.tri(mat.add)] <- 0
          colnames(mat.add) <- 1:(ll)
          betas.add <- list( mat.add[1]*nn )
          #if(lag == 1) 
          ## lag >1, neigh nn
          for(ll in 2:lag){
            
            alphas.add <- c(alphas.add , rep(ll, (ll) ) )
            
            mat.add <- matrix(1, ll, ll)
            mat.add[lower.tri(mat.add)] <- 0
            mat.add <- mat.add*nn
            colnames(mat.add) <- 1:(ll)
            beta2.add <- lapply(seq_len(ncol(mat.add)), function(i) mat.add[,i])
            betas.add <- append(betas.add, beta2.add)
            
            alphas <- c(alphas, alphas.add)
            betas <- append(betas, betas.add)
          }
        }
      }
    }
    return(list(alphas, betas))
  }
  lag <- 12
  MaxNeigh <- 1
  mods <- createModels(lag,MaxNeigh)
  full_net <- igraphtoGNAR( make_full_graph(ncol(df_full)))
  f <- function(i) list(mods[[1]][i], mods[[2]][[i]])
  parss <- lapply(1:length(mods[[1]]), f )
  indic <- c(12, 13, 15, 19, 65, 68, 69, 73, 74, 76)
})

#clusterEvalQ(cl, full_net)



clusterEvalQ(cl, {
  transformOrignial <- function(dataFrame_pred, scale, tf, init_vec){
    DataFrame_pred <- (dataFrame_pred * sapply(scale, rep, dim(dataFrame_pred)[1]))
    
    ## Inverse Difference
    if(tf == 1){
      output <- diffinv(DataFrame_pred, lag=1, xi=t(init_vec))
      
      ## Diff(log) inverse BoE is only 1 vector
    }else if(tf == 2){
      xx <- log(init_vec)
      output_ <- diffinv(DataFrame_pred, lag=1, xi=xx)
      output <- exp(output_)
      
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
  
  
  GetRealValues <- function(pred, real_init){
    
    mom_pred1 <- pred[,1:67]
    diff_pred <- pred[,68:71]
    mom_pred2 <- pred[,72:75]
    logDiff_pred <- as.matrix(pred[,76] )
    
    ## Init vectors
    mom_init1 <- as.matrix(real_init[1:67])
    diff_init <- as.matrix(real_init[68:71])
    mom_init2 <- as.matrix(real_init[72:75])
    logDiff_init <- as.matrix(real_init[76] )
    
    
    mom_orig1 <- transformOrignial(dataFrame_pred = mom_pred1,
                                   scale = df_full_sd_mat[1:67],
                                   tf=3,
                                   init_vec = mom_init1)
    
    diff_orig <- transformOrignial(dataFrame_pred = diff_pred,
                                   scale = df_full_sd_mat[68:71],
                                   tf=1,
                                   init_vec = diff_init)
    
    mom_orig2 <- transformOrignial(dataFrame_pred = mom_pred2,
                                   scale = df_full_sd_mat[72:75],
                                   tf=3,
                                   init_vec = mom_init2)
    
    logDiff_orig <- transformOrignial(dataFrame_pred = logDiff_pred,
                                      scale = df_full_sd_mat[76],
                                      tf=2,
                                      init_vec = logDiff_init)
    
    all <- cbind(mom_orig1,diff_orig,mom_orig2, logDiff_orig )
    return(all)
  }
  
})


#clusterEvalQ(cl, mods)

clusterEvalQ(cl, {
  NStepAhead <- function( params_list, net_objec, 
                          pred_points, how_far_back, 
                          how_far_forward, data, 
                          original_data, indic_specific_metrics){
    # Init Data for all
    alpha_m <- params_list[[1]]
    beta_m <- params_list[[2]]
    cat("   ")
    parNames <- paste( "GNAR(", alpha_m, ", [", toString(beta_m), "])" , sep="" )
    cat(" Model", parNames)
    cat("   ")
    totlen <- dim(data)[1] 
    noOfFitPoints <- totlen - how_far_back  
    NoOfPred <- (noOfFitPoints+1):(noOfFitPoints+how_far_forward)
    M <- how_far_forward - pred_points + 1
    add_cols <- length(indic_specific_metrics)
    metrics <- matrix(rep(NA, ( (4+3*add_cols)* M)), 
                      nrow=M,
                      ncol=(4+3*add_cols))
    
    for(ppred in 1:M){
      cat("  ")
      cat(ppred)
      cat("  ")
      dfTrain <- data[1:noOfFitPoints,]
      noOfFitPoints <- noOfFitPoints + 1 # stride 1
      indx_test <- NoOfPred[ppred:(ppred+pred_points-1)]
      dfRealTest <- original_data[indx_test,]
      
      gnar_fit <- GNARfit(vts = dfTrain, 
                          net = net_objec,
                          alphaOrder = alpha_m,
                          betaOrder = beta_m,
                          globalalpha = TRUE)
      
      # Prediction indicators
      predictions <- matrix(predict(gnar_fit, n=pred_points), 
                            ncol=ncol(dfTrain), nrow =pred_points)
      
      ## Transform back
      Nstep_pred <- GetRealValues(predictions, original_data[noOfFitPoints,])[-1,]
      
      ## Forecast Measures
      MSE <- sum( (Nstep_pred - dfRealTest)^2 )/pred_points
      metrics[ppred,1] <- (MSE)^(1/2)
      metrics[ppred,2] <- sum( abs(Nstep_pred - dfRealTest) )/pred_points
      metrics[ppred,3] <- BIC(gnar_fit)
      metrics[ppred,4] <- sum( abs(dfRealTest - Nstep_pred)/dfRealTest * 100 )/pred_points
      
      df_specific_test <- dfRealTest[,indic_specific_metrics]
      df_specific_pred <- Nstep_pred[,indic_specific_metrics]
      
      ## MSE indiv
      diff_sqr <- (df_specific_pred - df_specific_test)^2
      indiv_MSE <- colSums(diff_sqr)/pred_points
      metrics[ppred,5:(4+add_cols)] <- indiv_MSE
      
      abs_diff <- abs(df_specific_pred - df_specific_test)
      metrics[ppred,(5+add_cols):(4+2*add_cols)] <- colSums(abs_diff)/pred_points
      
      abs_perc <- abs(df_specific_test- df_specific_pred)/df_specific_test * 100
      metrics[ppred,(5+2*add_cols):(4+3*add_cols)] <- colSums(abs_perc)/pred_points
    }
    
    indiv_col_names <- colnames(data)[indic_specific_metrics]
    # set params
    colnames(metrics) <- c("RMSE", "MAE", "BIC", "MAPE", paste("MSE",indiv_col_names),
                           paste("MAE",indiv_col_names),
                           paste("MAPE",indiv_col_names))
    #return(list(metrics,ForecastValues )) 
    return(metrics)
  }
})


results <- mclapply(parss, NStepAhead, 
                    net_objec = full_net, 
                    pred_points = 12, how_far_back = 102,
                    how_far_forward = 102, data = as.matrix(df_full),
                    original_data = as.matrix(part_df), indic_specific_metrics = indic,
                    mc.cores = cl)

results <- parLapply(cl, parss,  NStepAhead, 
                     net_objec = full_net, 
                     pred_points = 12, how_far_back = 102,
                     how_far_forward = 102, data = as.matrix(df_full),
                     original_data = as.matrix(part_df), indic_specific_metrics = indic)

#clusterEvalQ(cl, results)

stopCluster(cl)
