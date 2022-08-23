# Transform back into a tims series

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




