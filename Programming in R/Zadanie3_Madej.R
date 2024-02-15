# Instalacja i ładowanie pakietów
install.packages("ggplot2")
install.packages("readr")
install.packages("plotly")
install.packages("corrplot")
install.packages("ggrepel")
library(ggrepel)
library(corrplot)
library(plotly)
library(rvest)
library(httr)
library(dplyr)
library(ggplot2)
library(readr)

# Pobranie danych z Wikipedii
url <- "https://pl.wikipedia.org/wiki/Wskaźnik_rozwoju_społecznego"
resp <- GET(url)
page <- read_html(url)

# Ekstrakcja tabeli z HTML
tab1 <- page %>% 
  html_nodes(css = "table.wikitable")
tab1

tabela <- tab1[[1]] %>% html_table(fill=T)
tabela <- as.data.frame(tabela)

# Zmiana nazw kolumn na bardziej przyjazne dla R
colnames(tabela) <- gsub(" ", "_", colnames(tabela))

# Przetworzenie kolumny Wskaźnik_HDI - usunięcie wartości w nawiasach
tabela$Wskaźnik_HDI <- gsub("\\s*\\(.*?\\)", "", tabela$Wskaźnik_HDI)

#
tabela$DN_per_capita <- tabela$`Dochód_narodowy_na_osobę(w_tys._dolarów_USA,_według_parytetu_siły_nabywczej_w_2011_r.)`

# Konwersja danych do odpowiednich typów
tabela$Państwo <- as.factor(tabela$Państwo)
tabela$Wskaźnik_HDI <- as.numeric(gsub(",", ".", tabela$Wskaźnik_HDI))
tabela$Spodziewana_długość_życia <- as.numeric(gsub(",", ".", tabela$Spodziewana_długość_życia))
tabela$DN_per_capita <- as.numeric(gsub(",", ".", tabela$DN_per_capita))

# Wybór istotnych kolumn
tabela <- tabela %>%
  select(Państwo, Wskaźnik_HDI, Spodziewana_długość_życia, DN_per_capita)


# Analizuję wartość wskaźnika HDI w zależności od Dochodu Narodowego per capita (w USD) oraz od spodziewanej długości życia

# Sprawdzam korelacje
cor = cor(tabela[, c("DN_per_capita", "Wskaźnik_HDI", "Spodziewana_długość_życia")],
          method = "spearman")
round(cor, 2)

# Wykres korelacji
corrplot(cor, method = "color", 
         type = "upper",
         order = "hclust",
         tl.col="black", 
         tl.srt=45)

# Podsumowanie Wskaźnika_HDI
summary(tabela$Wskaźnik_HDI)

# Sprawdzenie kilku początkowych i końcowych wartości
head(tabela)
tail(tabela)

# Wykresy

# Wykres słupkowy HDI dla różnych państw
tabela %>%
  ggplot() +
  geom_col(aes(y = reorder(Państwo, Wskaźnik_HDI), 
               x = Wskaźnik_HDI),
           fill = "lightblue",
           col = "navyblue",
           linewidth = 0.1) +
  theme_light() +
  labs(title = "Wskaźnik HDI dla różnych państw", x = "Wskaźnik_HDI", y = "Państwo")


# Wykres słupkowy spodziewanej długości życia dla różnych państw
tabela %>%
  ggplot() +
  geom_col(aes(x = Spodziewana_długość_życia, 
               y = reorder(Państwo, Spodziewana_długość_życia)), 
           fill = "lightblue",
           col = "navyblue",
           linewidth = 0.1) +
  theme_light() +
  labs(title = "Spodziewana długość życia dla różnych państw", x = "Spodziewana dł. życia", y = "Państwo")


# Wykres słupkowy Dochodu Narodowego per capita dla różnych państw
tabela %>%
  ggplot() +
  geom_col(aes(x = DN_per_capita, 
               y = reorder(Państwo, DN_per_capita)), 
           fill = "lightblue",
           col = "navyblue",
           linewidth = 0.1) +
  theme_light() +
  labs(title = "Dochód Narodowy per capita dla różnych państw", x = "DN per capita", y = "Państwo")


# Interaktywny wykres punktowy zależności HDI od DN per capita i Spodziewanej Długości Życia
interaktywny_wykres <- tabela %>%
  ggplot(aes(x = DN_per_capita, y = Spodziewana_długość_życia, size = Wskaźnik_HDI, text = paste("Państwo: ", Państwo, "<br>HDI: ", Wskaźnik_HDI, "<br>DN per capita: ", DN_per_capita, "<br>Spodziewana dł. życia: ", Spodziewana_długość_życia))) +
  geom_point(aes(color = Wskaźnik_HDI)) +
  labs(title = "Zależność HDI od DN i Spodziewanej Długości Życia", x = "Dochód Narodowy", y = "Spodziewana Długość Życia") +
  theme_minimal()

# Przekształcenie na plotly
interaktywny_wykres <- ggplotly(interaktywny_wykres, tooltip = "text")

# Wyświetlenie interaktywnego wykresu
interaktywny_wykres

# Wskaźnik HDI w zależności od kontynentu

# Podział państw na kontynenty
kontynenty <- tabela %>%
  mutate("Kontynent" = case_when(
    Państwo %in% c("Andora", "Austria", "Belgia", "Chorwacja", "Czarnogóra", "Czechy", "Dania", "Estonia", "Finlandia", "Francja", "Grecja", "Hiszpania", "Holandia", "Irlandia", "Liechtenstein", "Litwa", "Luksemburg", "Łotwa", "Malta", "Niemcy", "Norwegia", "Polska", "Portugalia", "Rumunia", "Słowacja", "Słowenia", "Szwajcaria", "Szwecja", "Węgry", "Wielka Brytania", "Włochy") ~ "Europa",
    Państwo %in% c("Arabia Saudyjska", "Bahrajn", "Brunei", "Cypr", "Hong Kong", "Izrael", "Japonia", "Katar", "Korea Płd.", "Kuwejt", "Rosja", "Singapur", "Zjednoczone Emiraty Arabskie") ~ "Azja",
    Państwo %in% c("Argentyna", "Chile") ~ "Ameryka_Pd",
    Państwo %in% c("Kanada", "USA") ~ "Ameryka_Pl",
    Państwo %in% c("Australia", "Nowa Zelandia") ~ "Australia i Oceania"))

# Średnia oraz mediana wskaźnika HDI dla każdego kontynentu
kontynenty %>%
  group_by(Kontynent) %>%
  summarise(średnia_HDI = mean(Wskaźnik_HDI), 
            mediana_HDI = median(Wskaźnik_HDI))

# Wyświetlenie państwa z największym wskaźnikiem HDI na każdym kontynencie
top1_z_kontynentu <- kontynenty %>%
  group_by(Kontynent) %>%
  top_n(1, Wskaźnik_HDI)

# Wykres punktowy dla państw z największym HDI z każdego kontynentu
ggplot(top1_z_kontynentu, aes(x = Kontynent, y = Wskaźnik_HDI, label = Państwo)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(box.padding = 0.5) +  # Użycie geom_text_repel z pakietu ggrepel
  labs(title = "Wskaźnik HDI dla jednego państwa z każdego kontynentu", x = "Kontynent", y = "Wskaźnik HDI") +
  theme_minimal()

