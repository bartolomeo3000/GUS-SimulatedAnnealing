library(doParallel)
library(foreach)

source("SA2_functions.R")

totm <- 200
n_exp <- 4000
beta <- 0.999
alpha <- 0.001
K <- 2000
L <- 20
Ld <- 20

# Przygotowanie klastra
cl <- makeCluster(detectCores() - 1)  # używa wszystkich rdzeni oprócz jednego
registerDoParallel(cl)

# Lista wyników
results <- foreach(j = 1:Ld, .packages = c()) %dopar% {
  source("functions.R")  # ważne, bo każdy worker ma własne środowisko!
  
  a <- generate_test_data(H=15, sumM=21000, pop=1500000)
  M <- a[[1]]
  N <- a[[2]]
  D <- a[[3]]
  S <- a[[4]]
  
  min_f <- Inf
  best_result <- NULL
  
  for(i in 1:L){
    print(i)
    sa_result <- SA(D, S, M, N, totm=totm, n_exp=n_exp, K=K, beta=beta, alpha=alpha)
    m <- sa_result[[1]]
    n <- sa_result[[2]]
    val <- f(M, N, D, S, m, n)
    if(val < min_f){
      min_f <- val
      best_result <- sa_result
    }
  }
  
  NieWes_result <- NieWes(D, S, M, N, totm, n_exp)
  m_NieWes <- NieWes_result[[1]]
  n_NieWes <- NieWes_result[[2]]
  
  m_best <- best_result[[1]]
  n_best <- best_result[[2]]
  
  list(
    min_f = min_f,
    NieWes_f = f(M, N, D, S, m_NieWes, n_NieWes),
    NieWes_rounded_f = f(M, N, D, S, round(m_NieWes), lapply(n_NieWes, round)),
    dist = sum(abs(unlist(n_NieWes) - unlist(n_best))) / 2
  )
}

# Rozpakowanie wyników
min_f_list <- sapply(results, `[[`, "min_f")
NieWes_f_list <- sapply(results, `[[`, "NieWes_f")
NieWes_rounded_f_list <- sapply(results, `[[`, "NieWes_rounded_f")
dist_list <- sapply(results, `[[`, "dist")

# Zamknięcie klastra
stopCluster(cl)

# stworzenie wektora bledow (roznicy NieWes - SA)
error_list <- NieWes_f_list - min_f_list
# bledy wzgledne
error_relative_list <- error_list / NieWes_f_list
# blad w stosunku do NieWes_rounded
NieWes_rounded_minus_min_f_list <- NieWes_rounded_f_list - min_f_list


results_df <- data.frame(
  Iteration = 1:Ld,
  Min_f_SA = min_f_list,
  f_NieWes = NieWes_f_list,
  error = error_list,
  relative_error = error_relative_list,
  f_NieWes_Rounded = NieWes_rounded_f_list,
  NieWes_rounded_minus_min_f = NieWes_rounded_minus_min_f_list,
  Distance_Steps = dist_list
)
results_df

library(ggplot2)
# Wykresy boxplot (error, ...)
b1<-ggplot(results_df, aes(x = factor(1), y = error)) +
  geom_boxplot() +
  labs(x = "Error", y = "Value") +
  ggtitle("Boxplot of Errors (NieWes - SA)")
b2<-ggplot(results_df, aes(x = factor(1), y = relative_error)) +
  geom_boxplot() +
  labs(x = "Relative Error", y = "Value") +
  ggtitle("Boxplot of Relative Errors (NieWes - SA)")
b3<-ggplot(results_df, aes(x = factor(1), y = NieWes_rounded_minus_min_f)) +
  geom_boxplot() +
  labs(x = "NieWes Rounded - Min f", y = "Value") +
  ggtitle("Boxplot of NieWes Rounded - Min f")
b4<-ggplot(results_df, aes(x = factor(1), y = Distance_Steps)) +
  geom_boxplot() +
  labs(x = "Distance Steps", y = "Value") +
  ggtitle("Boxplot of Distance Steps")

# analogicznie violin ploty z punktami
v1<-ggplot(results_df, aes(x = factor(1), y = error)) +
  geom_violin() +
  geom_boxplot(width = 0.05, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Error", y = "Value") +
  ggtitle("Violin Plot of Errors (NieWes - SA)")
v2<-ggplot(results_df, aes(x = factor(1), y = relative_error)) +
  geom_violin() +
  geom_boxplot(width = 0.05, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Relative Error", y = "Value") +
  ggtitle("Violin Plot of Relative Errors (NieWes - SA)")
v3<-ggplot(results_df, aes(x = factor(1), y = NieWes_rounded_minus_min_f)) +
  geom_violin() +
  geom_boxplot(width = 0.05, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "NieWes Rounded - Min f", y = "Value") +
  ggtitle("Violin Plot of NieWes Rounded - Min f")
v4<-ggplot(results_df, aes(x = factor(1), y = Distance_Steps)) +
  geom_violin() +
  geom_boxplot(width = 0.05, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Distance Steps", y = "Value") +
  ggtitle("Violin Plot of Distance Steps")

# Zapisanie wykresów do plików
ggsave("boxplot_errors.png", plot = b1, width = 8, height = 6)
ggsave("boxplot_relative_errors.png", plot = b2, width = 8, height = 6)
ggsave("boxplot_NieWes_rounded_minus_min_f.png", plot = b3, width = 8, height = 6)
ggsave("boxplot_distance_steps.png", plot = b4, width = 8, height = 6)
ggsave("violin_plot_errors.png", plot = v1, width = 8, height = 6)
ggsave("violin_plot_relative_errors.png", plot = v2, width = 8, height = 6)
ggsave("violin_plot_NieWes_rounded_minus_min_f.png", plot = v3, width = 8, height = 6)
ggsave("violin_plot_distance_steps.png", plot = v4, width = 8, height = 6)






