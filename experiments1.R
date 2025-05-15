# Ustawienie lokalizacji na lokalizację pliku
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import funkcji
source("simulated_annealing.r")

# Import 3 przykładów danych
data_polska <- read.csv("data/wojewodztwa_polska.csv")
data_niemcy <- read.csv("data/landy_niemcy.csv")
data_usa <- read.csv("data/stany_usa.csv")

head(data_polska)
head(data_niemcy)
head(data_usa)

# Generowanie współczynników wariancji
set.seed(123)
data_polska$A <- runif(nrow(data_polska), 1, 10)
data_niemcy$A <- runif(nrow(data_niemcy), 1, 10)
data_usa$A <- runif(nrow(data_usa), 1, 10)

head(data_polska)
head(data_niemcy)
head(data_usa)

x_polska_rnabox <- rnabox(1000, data_polska$A, ceiling(0.1 * data_polska$Populacja), pmin(20, data_polska$Populacja))
x_niemcy_rnabox <- rnabox(1000, data_niemcy$A, ceiling(0.1 * data_niemcy$Populacja), pmin(20, data_niemcy$Populacja))
x_usa_rnabox <- rnabox(5000, data_usa$A, ceiling(0.1 * data_usa$Populacja), pmin(50, data_usa$Populacja))

f_polska_rnabox <- f(data_polska$A, x_polska_rnabox)
f_niemcy_rnabox <- f(data_niemcy$A, x_niemcy_rnabox)
f_usa_rnabox <- f(data_usa$A, x_usa_rnabox)

# Eksperyment dla K ustalonego, beta zmieniającego się
betas <- seq(0.1, 0.99, 0.01)
K <- 10

# Liczba prob dla każdej bety
L <- 3
result_polska <- numeric(length(betas))
result_niemcy <- numeric(length(betas))
result_usa <- numeric(length(betas))


for (i in 1:length(betas)){
  beta <- betas[i]
  cat(c("beta:", beta, "\n"))
  
  for (l in 1:L){
  
    x_polska <- SA(data_polska$A, pmin(20, data_polska$Populacja),
                   ceiling(0.1 * data_polska$Populacja), 1000,
                   beta = beta, K = K)
    x_niemcy <- SA(data_niemcy$A, pmin(20, data_niemcy$Populacja),
                   ceiling(0.1 * data_niemcy$Populacja), 1000,
                   beta = beta, K = K)
    x_usa <- SA(data_usa$A, pmin(50, data_usa$Populacja),
                   ceiling(0.1 * data_usa$Populacja), 5000,
                   beta = beta, K = K)
    
    result_polska[i] <- result_polska[i] + f(data_polska$A, x_polska)
    result_niemcy[i] <- result_niemcy[i] + f(data_niemcy$A, x_niemcy)
    result_usa[i] <- result_usa[i] + f(data_usa$A, x_usa)
  }
  result_polska[i] <- result_polska[i] / L
  result_niemcy[i] <- result_niemcy[i] / L
  result_usa[i] <- result_usa[i] / L
}

df <- cbind(betas, result_polska, result_niemcy, result_usa)
df <- rbind(df, c(-1, f_polska_rnabox, f_niemcy_rnabox, f_usa_rnabox))
df <- as.data.frame(df)
df
write.csv(df, "./data/experiment1_betas.csv", row.names = F)

# Eksperyment dla ustalonej temperatury beta i dla zmiennego K
beta <- 0.9
Ks <- seq(3, 50, 1)

# Liczba prob dla każdego K
L <- 3
result_polska <- numeric(length(Ks))
result_niemcy <- numeric(length(Ks))
result_usa <- numeric(length(Ks))


for (i in 1:length(Ks)){
  K <- Ks[i]
  cat(c("K:", K, "\n"))
  
  for (l in 1:L){
    
    x_polska <- SA(data_polska$A, pmin(20, data_polska$Populacja),
                   ceiling(0.1 * data_polska$Populacja), 1000,
                   beta = beta, K = K)
    x_niemcy <- SA(data_niemcy$A, pmin(20, data_niemcy$Populacja),
                   ceiling(0.1 * data_niemcy$Populacja), 1000,
                   beta = beta, K = K)
    x_usa <- SA(data_usa$A, pmin(50, data_usa$Populacja),
                ceiling(0.1 * data_usa$Populacja), 5000,
                beta = beta, K = K)
    
    result_polska[i] <- result_polska[i] + f(data_polska$A, x_polska)
    result_niemcy[i] <- result_niemcy[i] + f(data_niemcy$A, x_niemcy)
    result_usa[i] <- result_usa[i] + f(data_usa$A, x_usa)
  }
  result_polska[i] <- result_polska[i] / L
  result_niemcy[i] <- result_niemcy[i] / L
  result_usa[i] <- result_usa[i] / L
}

df <- cbind(Ks, result_polska, result_niemcy, result_usa)
df <- rbind(df, c(-1, f_polska_rnabox, f_niemcy_rnabox, f_usa_rnabox))
df <- as.data.frame(df)
df
write.csv(df, "./data/experiment1_Ks.csv", row.names = F)
