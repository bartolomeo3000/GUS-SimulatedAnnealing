library(stratallo)

generate_test_data <- function(H = 5, pop = 500, seed = 42) {
  set.seed(seed)
  
  # Losowanie proporcji dla każdej warstwy (żeby ich suma = 1)
  proportions <- runif(H)
  proportions <- proportions / sum(proportions)
  
  # Liczności populacji w warstwach (zaokrąglone, suma = N)
  N <- round(proportions * pop)
  
  # Korekta sumy (jeśli zaokrąglenie ją zmieni)
  diff <- pop - sum(N)
  if (diff != 0) {
    N[which.max(N)] <- N[which.max(N)] + diff
  }
  
  # Współczynniki wariancji A (np. w zakresie 1–10)
  A <- runif(H, min = 1, max = 10)
  
  # Ograniczenia na liczność próby z każdej warstwy
  # min. 5, max. 40% liczności warstwy
  m <- pmin(20, N)
  M <- ceiling(0.1 * N)
  
  data <- data.frame(
    h = 1:H,
    N = N,
    A = A,
    m = m,
    M = M
  )
  
  return(data)
}


df <- generate_test_data(H = 16, pop = 38000000)
head(df)

f <- function(A, x){
  sum(A ** 2 / x)
}

generate_state_0 <- function(m, M, n, H){
  x <- m
  
  k <- n - sum(m)
  while (k > 0){
    i <- sample(1:H, 1)
    if (x[i] + 1 < M[i]){
      x[i] <- x[i] + 1
      k <- k - 1
    }
  }
  x
}

SA <- function(A, m, M, n, beta = 0.95, K = 100){
  H <- length(A)
  t <- 1
  
  if (n < sum(m)){
    return(FALSE)  
  }
  if (n > sum(M)){
    return (FALSE)
  }
  
  x <- generate_state_0(m, M, n, H)
  x
  f_prev <- f(A, x)
  
  k <- 0 # Licznik pozostawania w tym samym stanie
  
  while (k < K){
    while (TRUE){
      w <- sample(1:H, 2)
      if (m[w[1]] > x[w[1]] - 1){
        next
      }
      if (x[w[2]] + 1 > M[w[2]]){
        next
      }
      x[w[1]] <- x[w[1]] - 1
      x[w[2]] <- x[w[2]] + 1
      
      f_next <- f(A, x)
      
      delta <- f_next - f_prev
      if (delta < 0){
        f_prev <- f_next
        t <- t * beta
        k <- 0
        break
      }
      
      prob <- exp(-delta / t)
      u <- runif(1)
      
      if (u < prob){
        f_prev <- f_next
        t <- t * beta
        k <- 0
        break
      }
      # Zostajemy w tym samym stanie, wiec odwracamy przejście
      x[w[1]] <- x[w[1]] + 1
      x[w[2]] <- x[w[2]] - 1
      k <- k + 1
      
      t <- t * beta
      break
    }
  }
  x
}
