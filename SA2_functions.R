f <- function(M, N, D, S, m, n){
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


check2 <- function(totn, m, n, M, eps=1){
  sumn <- 0
  for(h in 1:length(m)){
    sumn <- sumn+sum(n[[h]])*m[h]/M[h]
  }
  return(totn-eps<sumn & sumn<totn+eps)
}


generate_test_data <- function(H = 5, sumM = 50, pop = 10000, minN = 10){
  M <- rep(1, H)
  sumM <- sumM - H
  for (i in 1:sumM){
    s <- sample(1:H, 1)
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
  D <- runif(H, 100, 200)
  S <- list()
  for (i in 1:H){
    S[[i]] <- runif(M[i], 0.5, 2)
  }
  return(list(M, N, D, S))
}


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


SA <- function(D, S, M, N, totm, n_exp, alpha = 0.05, beta = 0.95, K = 10, p=0.2){
  eps <- alpha * n_exp
  H <- length(M)
  t <- 1
  steps <- 0
  
  # Generujemy stan startowy
  x <- generate_state_0(totm, M, n_exp, N, H)
  m <- x[[1]]
  n <- x[[2]]
  
  f_prev <- f(M, N, D, S, m, n)
  
  k <- 0 # licznik pozostawania w tym samym stanie
  
  while(k<K){
    steps <- steps + 1
    # losujemy czy przerzucamy szkole czy ucznia
    change_m <- runif(1)<p # TRUE - szkola lub FALSE - uczen
    if(change_m){ # przerzucamy szkole
      while(TRUE){
        # print("szukam legitnego stanu (wariant ze szkolami)") #################
        w <- sample(1:H, 2)
        # sprawdzamy, czy mozna odjac szkole
        if (m[w[1]] - 1 < 1){
          next
        }
        # sprawdzamy, czy mozna dodac szkole
        if (m[w[2]] + 1 > M[w[2]]){ 
          next
        }
        m[w[1]] <- m[w[1]] - 1
        m[w[2]] <- m[w[2]] + 1
        # sprawdzamy, czy warunek 2 spelniony
        if (!check2(n_exp, m, n, M, eps = eps)){
          # print("check2 failed - szkoly") #################
          m[w[1]] <- m[w[1]] + 1
          m[w[2]] <- m[w[2]] - 1
          next
        }
        break
      }
    }
    else{ # przerzucamy ucznia
      while(TRUE){
        # print("szukam legitnego stanu (wariant z uczniami)") #################
        # losujemy dwa wojewodztwa
        w <- sample(1:H, 2)
        # losujemy szkole z wojewodztwa w[1] i szkole z wojewodztwa w[2]
        s1 <- sample(1:length(n[[w[1]]]), 1)
        s2 <- sample(1:length(n[[w[2]]]), 1)
        # sprawdzamy, czy mozna odjac ucznia
        if (n[[w[1]]][s1] - 1 < 1){
          next
        }
        # sprawdzamy, czy mozna dodac ucznia
        if (n[[w[2]]][s2] + 1 > N[[w[2]]][s2]){ 
          next
        }
        n[[w[1]]][s1] <- n[[w[1]]][s1] - 1
        n[[w[2]]][s2] <- n[[w[2]]][s2] + 1
        # sprawdzamy, czy warunek 2 spelniony
        if (!check2(n_exp, m, n, M, eps = eps)){
          # print("check2 failed - uczniowie") #################
          n[[w[1]]][s1] <- n[[w[1]]][s1] + 1
          n[[w[2]]][s2] <- n[[w[2]]][s2] - 1
          next
        }
        break
      }
    }
    # W tym momencie mamy już zaproponowany legitny nowy stan
    # Obliczamy nowa wartosc funkcji celu
    f_next <- f(M, N, D, S, m, n)
    # Obliczamy roznice
    delta <- f_next - f_prev
    # Jesli roznica jest ujemna, to akceptujemy nowy stan
    if (delta < 0){
      f_prev <- f_next
      k <- 0
      # print("ujemna delta") ###############
    }
    # Jesli roznica jest nieujemna, to akceptujemy nowy stan z pewnym prawdopodobienstwem
    else{
      prob <- exp(-delta / t)
      if (runif(1) < prob){
        f_prev <- f_next
        if(delta == 0){
          k <- k+1
          # print("Przechodze do innego stanu z tym samym f") ##############
        }
        else{k <- 0}
      }
      # Jesli nie zaakceptowalismy nowego stanu, to wracamy do poprzedniego
      else{
        if(change_m){ # przerzucamy szkole
          m[w[1]] <- m[w[1]] + 1
          m[w[2]] <- m[w[2]] - 1
        }
        else{ # przerzucamy ucznia
          n[[w[1]]][s1] <- n[[w[1]]][s1] + 1
          n[[w[2]]][s2] <- n[[w[2]]][s2] - 1
        }
        k <- k + 1 # zostalismy w tym samym stanie, wiec zwiekszamy licznik k
        # print("Zostaje w tym samym stanie") ################
      }
    }
    t <- t * beta # po wykonaniu każdego kroku zmniejszamy temperature
    # print("temperaturka:")
    # print(t)
    # print("k:")
    # print(k)
    # print(f(M, N, D, S, m, n))
    # print("m:")
    # print(m)
    # print("n:")
    # print(n)
  }
  #print("Liczba wykonanych kroków:")
  #print(steps)
  return(list(m,n))
}


NieWes <- function(D, S, M, N, totm, n_exp){
  g <- M*D^2
  for(h in 1:length(M)){
    g[h] <- g[h] - sum(N[[h]]*S[[h]]^2)
  }
  
  m <- rep(0, length(M))
  for(h in 1:length(M)){
    m[h] <- sqrt(M[h] * g[h])
  }
  m <- m / sum(m) * totm
  
  n <- list()
  for(h in 1:length(M)){
    n[[h]] <- rep(0, M[h])
    for(j in 1:M[h]){
      n[[h]][j] <- N[[h]][j] * S[[h]][j]
    }
  }
  suma <- 0
  for(h in 1:length(n)){
    suma <- suma + sum(n[[h]])
  }
  for(h in 1:length(M)){
    n[[h]] <- n[[h]] * n_exp * M[h] / m[h] / suma
  }
  return(list(m,n))
}

