source("SA2_functions.R")

# Pomniejsze testy działania funkcji dla dwustopniowego wariantu
a <- generate_test_data(H=5, sumM=1400, pop=500000)
M <- a[[1]]
N <- a[[2]]
D <- a[[3]]
S <- a[[4]]

# TEST generate state_0
b <- generate_state_0(10,M,2000,N,length(M))
m <- b[[1]]
n <- b[[2]]
f(M, N, D, S, m, n)
check(10,2000,m,n,M)
unlist(n)-unlist(N)

# TEST SA
totm <- 100 # ile szkol
n_exp <- 2000 # ile uczniow
beta <- 0.999 # wspolczynnik schladzania
alpha <- 0.001 # wspolczynnik dopuszczalnego odchylenia w warunku 2 (alpha*n_exp)
K <- 2000 # warunek stopu

sa_result <- SA(D, S, M, N, totm=totm, n_exp=n_exp, K=K, beta=beta, alpha = alpha)
m <- sa_result[[1]]
n <- sa_result[[2]]
f(M, N, D, S, m, n)
check(totm,n_exp,m,n,M, eps=alpha*n_exp)

# TEST NieWes
NieWes_result <- NieWes(D, S, M, N, totm, n_exp)
m_NieWes <- NieWes_result[[1]]
n_NieWes <- NieWes_result[[2]]
f(M, N, D, S, m_NieWes, n_NieWes)
check(totm,n_exp,m_NieWes,n_NieWes,M)
# blad
f(M, N, D, S, m_NieWes, n_NieWes) - f(M, N, D, S, m, n)
# blad wzgledny
(f(M, N, D, S, m_NieWes, n_NieWes) - f(M, N, D, S, m, n)) / f(M, N, D, S, m_NieWes, n_NieWes)

all(unlist(N) > unlist(n_NieWes)) # sprawdzamy, czy nie przekroczylismy liczby uczniow w szkole
unlist(n_NieWes) - unlist(n)

# uruchamiamy funkcje SA w petli wiele razy i zapamietujemy najlepszy wynik
# czyli najmniejsza wartosc funkcji celu
L <- 20
min_f <- Inf
best_result <- NULL
for(i in 1:L){
  print(i)
  sa_result <- SA(D, S, M, N, totm=totm, n_exp=n_exp, K=K, beta=beta, alpha = alpha)
  m <- sa_result[[1]]
  n <- sa_result[[2]]
  if(f(M, N, D, S, m, n) < min_f){
    min_f <- f(M, N, D, S, m, n)
    best_result <- sa_result
  }
}
m_best <- best_result[[1]]
n_best <- best_result[[2]]
min_f
f(M, N, D, S, m_NieWes, n_NieWes)
f(M, N, D, S, round(m_NieWes), lapply(n_NieWes, round)) # zaokrąglamy do liczb całkowitych)
f(M, N, D, S, m_NieWes, n_NieWes) - f(M,N,D,S,m_best,n_best) # im wieksze tym lepiej
(f(M, N, D, S, m_NieWes, n_NieWes) - f(M,N,D,S,m_best,n_best)) / f(M, N, D, S, m_NieWes, n_NieWes)
check(totm, n_exp, m_best, n_best, M, eps=alpha*n_exp)

check(totm, n_exp, m_NieWes, n_NieWes, M, eps=alpha*n_exp)

sum(abs(unlist(n_NieWes) - unlist(n_best))) / 2 # +- o tyle kroków alg są oddalone

# sprawdzenie wpływu n na funkcję celu
f(M, N, D, S, m_NieWes, n_NieWes)
f(M, N, D, S, m_best, n_NieWes)
f(M, N, D, S, m_best, n_best)

f(M, N, D, S, m_NieWes, n_best)



