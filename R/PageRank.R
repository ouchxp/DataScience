pageRank <- function(M, n, beta, rankSum) {
  prer <- matrix(-100, n)
  r <- matrix(1/n, n)
  while (sum(abs(prer - r)) > 0.001) {
    tmpr <- beta * (M %*% r)
    prer <- r
    r <- tmpr + (1-sum(tmpr))/n
  }
  r*rankSum
}

test1 <- function(){
  #M <- matrix(0,3,3)
  #M[1,] <- t(as.matrix(c(0,0,0)))
  #M[2,] <- t(as.matrix(c(1/2,0,0)))
  #M[3,] <- t(as.matrix(c(1/2,1,1)))
  
  M <- matrix(c(0,0,0,  1/2,0,0,  1/2,1,1), byrow = TRUE, nrow = 3, ncol = 3)
  n <- 3
  rankSum <- 3
  beta <- 0.7
  pageRank(M, n, beta, rankSum)
}

test2 <- function(){

  M <- matrix(c(0,0,1,  1/2,0,0,  1/2,1,0), byrow = TRUE, nrow = 3, ncol = 3)
  n <- 3
  rankSum <- 1
  beta <- 0.85
  pageRank(M, n, beta, rankSum)
}

pageRankNaive <- function(M, n) {
  prer <- matrix(-100, n)
  r <- matrix(1, n)
  count <- 1
  while (sum(abs(prer - r)) > 0.001) {
    tmpr <- M %*% r
    prer <- r
    r <- tmpr
    
    print(count)
    print(r)
    count <- count+1
    #if(count > 5) break
  }
}

test3 <- function(){
  
  M <- matrix(c(0,0,1,  1/2,0,0,  1/2,1,0), byrow = TRUE, nrow = 3, ncol = 3)
  n <- 3
  rankSum <- 1
  beta <- 1
  
  pageRankNaive(M, n)
}
