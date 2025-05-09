install.packages("rvest")
library(rvest) 
library(dplyr)

### POLSKA ----------------------------------------------
# URL strony Wikipedii
url <- "https://pl.wikipedia.org/wiki/Wojew%C3%B3dztwo"
# Wczytanie strony
strona <- read_html(url)
# Wczytanie wszystkich tabel z Wikipedii
tabele <- html_table(strona, fill = TRUE)
# Sprawdzenie ile tabel zostało znalezionych
length(tabele)
# Wyświetlenie nagłówków pierwszych kilku tabel (opcjonalnie)
lapply(tabele, function(x) head(x, 2))
# Wybór pierwszej tabeli
tabela_wojewodztwa <- tabele[[1]]
tabela_wojewodztwa <- as.data.frame(tabela_wojewodztwa)
# Wybor kolumny 2 i 5
tabela_wojewodztwa <- tabela_wojewodztwa[, c(2, 5)]
# Zmiana nazw kolumn
colnames(tabela_wojewodztwa) <- c("Wojewodztwo", "Populacja")
# Zmienienie typu Population na int
tabela_wojewodztwa$Populacja <- as.integer(gsub("\\s+", "", tabela_wojewodztwa$Populacja))
tabela_wojewodztwa
# Zapisanie tabeli do pliku CSV
write.csv(tabela_wojewodztwa, "wojewodztwa_polska.csv", row.names = FALSE)


### USA ---------------------------------------------
# URL strony Wikipedii
url <- "https://pl.wikipedia.org/wiki/Podzia%C5%82_administracyjny_Stan%C3%B3w_Zjednoczonych"
# Wczytanie strony
strona <- read_html(url)
# Wczytanie wszystkich tabel z Wikipedii
tabele <- html_table(strona, fill = TRUE)
# Sprawdzenie ile tabel zostało znalezionych
length(tabele)
# Wyświetlenie nagłówków pierwszych kilku tabel (opcjonalnie)
lapply(tabele, function(x) head(x, 2))
# Wybór pierwszej tabeli
tabela_stany <- tabele[[1]]
tabela_stany <- as.data.frame(tabela_stany)
# Wybor kolumny 2 i 5
tabela_stany <- tabela_stany[, c(2, 5)]
# Zmiana nazw kolumn
colnames(tabela_stany) <- c("Stan", "Populacja")
# Zmienienie typu Population na int
tabela_stany$Populacja <- as.integer(gsub(",", "", tabela_stany$Populacja)) * 10^6
tabela_stany
# Zapisanie tabeli do pliku CSV
write.csv(tabela_stany, "stany_USA.csv", row.names = FALSE)

### NIEMCY ---------------------------------------------
# URL strony Wikipedii
url <- "https://pl.wikipedia.org/wiki/Kraje_zwi%C4%85zkowe_Niemiec"
# Wczytanie strony
strona <- read_html(url)
# Wczytanie wszystkich tabel z Wikipedii
tabele <- html_table(strona, fill = TRUE)
# Sprawdzenie ile tabel zostało znalezionych
length(tabele)
# Wyświetlenie nagłówków pierwszych kilku tabel (opcjonalnie)
lapply(tabele, function(x) head(x, 2))
# Wybór pierwszej tabeli
tabela_niemcy <- tabele[[1]]
tabela_niemcy <- as.data.frame(tabela_niemcy)
# Wybor kolumny 3 i 9
tabela_niemcy <- tabela_niemcy[, c(3, 9)]
# Zmiana nazw kolumn
colnames(tabela_niemcy) <- c("Land", "Populacja")
# Zmienienie typu Population na int
tabela_niemcy$Populacja <- as.integer(gsub("\\s+", "", tabela_niemcy$Populacja))
tabela_niemcy
# Zapisanie tabeli do pliku CSV
write.csv(tabela_niemcy, "landy_niemcy.csv", row.names = FALSE)


