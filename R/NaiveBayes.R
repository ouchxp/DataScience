predict <- function(trainx, trainy, X) {
  m <- nrow(trainx) # item number of tarining set
  n <- ncol(trainx) # number of features
  ny <- length(unique(trainy)) # number of classes
  sumByClass <- array(0, ny)
  featureSumMatrix <- matrix(0, n ,ny)
  
  # compute occurency
  for (i in 1:m){
    yi <- trainy[i]
    Xi <- trainx[i,] # the ith item (vector)
    for(j in 1:n) {
      
      xj <- Xi[j]
      if (xj != 0) {
        sumByClass[yi] <- sumByClass[yi] + 1
        featureSumMatrix[xj ,yi] <- featureSumMatrix[xj ,yi] + 1
      }
    }
  }
  
  # compute conditional probilities
  condProbMatrix <- featureSumMatrix
  cols <- ncol(condProbMatrix)
  rows <- nrow(condProbMatrix)
  for(c in 1:cols) {
    for(r in 1:rows) {
      condProbMatrix[r,c] <- featureSumMatrix[r,c]/sumByClass[c]
    }
  }
  
  # predict
  prediction <- 0
  predictProb <- 0
  for(c in 1:cols) {
    probc <- 1
    for(r in 1:rows) {
      if(X[r]!=0){ 
        probc <- probc*condProbMatrix[r,c]
      }
    }
    # save the maxmum probility class
    if (probc > predictProb) {
      predictProb <- probc
      prediction <- c
    }
  }
  
  # return value
  prediction
}

test <- function(){
  # 1 female 2 male 
  yt <- c(1,1,1,1,2,2,2,2)
  # 0 missing feature 1 long hair 2 white skin 3 wear pants
 
  Xt <-  matrix(c(
    # females
    1,2,3,  
    1,2,0, 
    1,0,0,
    0,2,0,
    
    # males
    0,0,3,
    1,2,3,
    0,2,3,
    1,0,3
    ), byrow = TRUE, nrow = 8, ncol = 3)
  X <- c(1,0,3)
  predict(Xt,yt,X)
}
