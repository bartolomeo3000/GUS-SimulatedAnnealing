f <- function(M, N, D, S, l){
  b <- sum((1/m - 1/M) * M**2 * D**2)
  
  for (h in 1:length(M)){
    alpha <- M[h] / m[h]
    for (j in 1:lenght(M[h])){
      b <- b + alpha * (1/n[h][j] - 1/N[h][j]) * N[h][j]**2 * S[h][j]**2
    }
  }
  b
}

generate_data <- function(H = 5, sumM = 50, pop = 10000, minN = 10){
  M <- rep(1, H)
  sumM <- sumM - H
  
  for (i in 1:sumM){
    s <- sample(1:5, 1)
    M[s] <- M[s] + 1
  }
  
  N <- list()
  
  for (i in 1:H){
    N[[i]] <- rep(minN - 1, M[i])
  }
  
  pop <- pop - (sumM + H) * (minN - 1)
  
  p <- sample(1:(pop - 1), sumM + H - 1)
  p <- sort(p)
  p <- c(p[1], diff(p), pop - p[length(p)])
  
  l <- 1
  for (i in 1:H){
    for (j in 1:M[i]){
      N[[i]][j] <- N[[i]][j] + p[l]
      l <- l + 1
    }
  }
  
  D <- runif(H, 1, 10)
  
  S <- list()
  for (i in 1:H){
    S[[i]] <- runif(M[i], 1, 10)
  }
  
  return(list(M, N, D, S))
}

a <- generate_data()
M <- a[[1]]
N <- a[[2]]
D <- a[[3]]
S <- a[[4]]

generate_state <- 

f(M, N, D, S, 500)



