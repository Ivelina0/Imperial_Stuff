create.models <- function(lag, NeighB.Max = 1){
  
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
# lag 2, neigh 1
create.models(2)[[1]]
create.models(2)[[2]]
# lag 2, neigh 2
create.models(2, 2)[[1]]
create.models(2, 2)[[2]]
# lag 1, neigh 2
create.models(1, 2)[[1]]
create.models(1, 2)[[2]]
#unlist(create.models(2)[[2]])
