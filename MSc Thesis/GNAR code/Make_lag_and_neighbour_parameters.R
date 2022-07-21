create.models <- function(lag){
  
  ## Required packages
  #library(igraph)
  #library(GNAR)
  
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
  
  if(lag == 1) break
  
  for(ll in 2:lag){
    
    alphas <- c(alphas , rep(ll, (ll+1) ) )
    
    zeros <- rep(0, ll)
    mat <- matrix(1, ll, ll)
    mat[lower.tri(mat)] <- 0
    newmat<-cbind(zeros, mat)
    colnames(newmat) <- 1:(ll+1)
    beta2 <- lapply(seq_len(ncol(newmat)), function(i) newmat[,i])
    betas <- append(betas, beta2)
  }
  return(list(alphas, betas))
}