f <- function(M, N, D, S, l, m, n){
  b <- sum((1/m - 1/M) * M**2 * D**2)
  for (h in 1:length(M)){
    alpha <- M[h] / m[h]
    for (j in 1:length(M[h])){
      b <- b + alpha * (1/n[[h]][j] - 1/N[[h]][j]) * N[[h]][j]**2 * S[[h]][j]**2
    }
  }
  b
}

check <- function(totm, totn, m, n, M, eps=1){
  sumn <- 0
  for(h in 1:length(m)){
    sumn <- sumn+sum(n[[h]])*m[h]/M[h]
  }
  if(sum(m)==totm & sumn<totn+eps & sumn>totn-eps){return(c(TRUE,sumn-totn))}
  return(c(FALSE, sumn-totn))
}

generate_test_data <- function(H = 5, sumM = 50, pop = 10000, minN = 10){
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
a <- generate_test_data()
M <- a[[1]]
N <- a[[2]]
D <- a[[3]]
S <- a[[4]]

generate_state_0 <- function(m_ch, M, n_ch, N, H){
  gora <- rep(0, H)
  for(i in 1:H){
    gora[i] <- sum(N[[i]])
  }
  while(TRUE){
    m_tmp <- m_ch-H
    m <- rep(1,H)
    while(m_tmp>0){
      s <- sample(1:H, 1, prob=M) # wagi jak proporcje w M
      if(m[s]<M[s]){m[s] <- m[s]+1; m_tmp <- m_tmp-1}
    }
    if(sum((m/M)*gora)>=n_ch){break}
  }
  while(sum((m/M)*gora)-n_ch>0){
    s <- sample(1:H, 1)
    if(gora[s]-1>=length(N[[s]])){gora[s] <- gora[s]-1}
  }
  n <- list()
  for (i in 1:H){
    n[[i]] <- rep(1, M[i])
    gora[i] <- gora[i]-sum(n[[i]])
  }
  for(i in 1:H){
    while(TRUE){
      if(sum(N[[i]])>=gora[i]){
        g <- gora[i]
        while(g>0){
          c <- sample(M[i], 1)
          if(n[[i]][c]+1<=N[[i]][c]){
            n[[i]][c] <- n[[i]][c]+1
            g <- g-1
          }
        }
        break}
    }
  }
  return(list(m,n))
}
b <- generate_state_0(25,M,5000,N,5)
m <- b[[1]]
n <- b[[2]]



f(M, N, D, S, 500, m, n)
check(25,5000,m,n,M)
unlist(n)-unlist(N)