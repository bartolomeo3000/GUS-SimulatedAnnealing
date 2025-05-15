library(ggplot2)

# Ustawienie lokalizacji na lokalizację pliku
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df1 <- read.csv("data/experiment1_betas.csv")
df2 <- read.csv("data/experiment1_Ks.csv")
head(df1)
head(df2)

# Wykres 1
ggplot(data = df1[-91,], aes(x = betas, y = result_polska)) +
  geom_point() +
  geom_hline(yintercept = df1[91, 2], color = "red") +
  labs(title = "Średnia znaleziona optymalna wartość funkcji dla K = 10 w zależności od Beta",
       subtitle = "Dane: województwa, Polska",
       x = "Współczynnik Beta",
       y = "Wartość funkcji")

# Wykres 2

# Wykres 3

# Wykres 4
ggplot(data = df2[-49,], aes(x = Ks, y = result_polska)) +
  geom_point() +
  geom_hline(yintercept = df2[49, 2], color = "red") +
  labs(title = "Średnia znaleziona optymalna wartość funkcji dla Beta = 0.9 w zależności od K",
       subtitle = "Dane: województwa, Polska",
       x = "Kryterium stopu (K)",
       y = "Wartość funkcji")

# Wykres 5

# Wykres 6
