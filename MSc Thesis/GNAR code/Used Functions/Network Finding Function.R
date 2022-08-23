## function(data, alphaParams, BetaParams, globAlpha)
# findNet <- function(test, alphaParams, BetaParams, globAlpha)

betas <- list(c(1, 0, 0), c(1, 1, 0), c(1, 1, 1), 
              c(2, 0, 0), c(2, 1, 0), c(2, 2, 0),
              c(2, 2, 1), c(2, 2, 2), c(3, 0, 0),
              c(3, 1, 0), c(3, 2, 0), c(3, 3, 0))

#####################################
## Find RSS Model benchmarks function
## Note need networks with beta order less than 2 since
## it is fully connected!!!
#####################################

rss.benchmarks <- function(test, validate, alphaP, betaP){
  ## Params
  nodes <- dim(test)[2]
  test.ts <- dim(test)[1]
  valid.ts <- dim(validate)[1]
  if(length(alphaP) != length(betaP)) stop("Alpha parameters are not equal to beta parameters")
  models <- length(alphaP)
  alphaType = c(TRUE, FALSE)
  fullnet <- igraphtoGNAR( make_full_graph(nodes))
  
  ## Memory
  bench.memory <- matrix(rep(NA, models*2), ncol = models, nrow = 2)
  
  for( type in c(1:2)){
    for( mod in c(1:models)){
      
      fit <- GNARfit(vts = test, 
                     net = fullnet,
                     alphaOrder = alphaP[mod], 
                     betaOrder = betaP[[mod]], 
                     globalalpha = alphaType[type])
      #print(fit.one.one)
      pred <- matrix(predict(fit, n=valid.ts), ncol = nodes, nrow = valid.ts)
      RSS <- sum( (pred - validate)^2 )
      bench.memory[type, mod] <- RSS
      
    }
  }
  
  parNames <-  NULL
  for (jj in 1:models) {
    parNames <- c(parNames, paste("GNAR(", alphaP[jj], ", [", toString(betaP[[jj]]), "])" , sep="" ))
  }
  
  rownames(bench.memory) <- c("Global-Alpha", "Indiv-Alpha")
  colnames(bench.memory) <-  parNames
  return(bench.memory)
}


alphas <- c(rep(1, 2), rep(2, 3), rep(3, 4))
betas <- list( c(0), c(1), 
               c(0,0), c(1,0), c(1,1),
               c(0,0,0), c(1,0,0), c(1,1,0),c(1,1,1) )

benchvec <- rss.benchmarks(test, validate, alphas, betas)
benchvec

which(bench.memory == min(bench.memory), arr.ind = TRUE)
min(bench.memory)


#################################
## Find Rss matrix funcion
#################################
rss.minus.edges <- function(test, validate, alphaP, betaP, alphaGlob){
  
  ## Memory
  nodes <- dim(test)[2]
  test.ts <- dim(test)[1]
  valid.ts <- dim(validate)[1]
  rss.mat <- matrix(rep(NA, nodes^2), ncol=nodes, nrow=nodes)
  node.list <- c(1:nodes)
  
  # for i node to j node
  for(i in node.list){
    if(i == nodes){
      break
    }
    node.list.two <- c((i+1):nodes)
    for(j in node.list.two){
      # Create graph without 1 edge
      adjmat <- as.matrix(make_full_graph(nodes), "adjacency") 
      adjmat[i,j] <- adjmat[j,i] <- 0
      #adjmat[j,i] <- 0
      # Turn into igraph
      newg <- graph_from_adjacency_matrix(
        adjmat,
        mode = c("undirected"),
        weighted = NULL,
        diag = TRUE,
        add.colnames = NULL,
        add.rownames = NA
      )
      # Turn into GNAR graphs
      fullnet.minus1 <- igraphtoGNAR(newg)
      
      ## Fitting
      fit <- GNARfit(vts = test, 
                     net = fullnet.minus1,
                     alphaOrder = alphaP,
                     betaOrder = betaP, 
                     globalalpha = alphaGlob)
      
      pred <- matrix(predict(fit, n=valid.ts), ncol = nodes, nrow = valid.ts) 
      RSS.1 <- sum( (pred - validate)^2 )
      rss.mat[i,j] <- RSS.1
    }
  }
  return(rss.mat)
}

trial<-rss.minus.edges(test, validate, 2, c(1,0), TRUE)
trial
rss.mat.two



#################################
## Create Models Function
#################################




