#Amelia Madej
set.seed(123)
#Zadanie1
x1 <- sample(1:4000, 100, replace = F)

minmaxK <- function(x, K = 5) {
  sorted_x <- sort(x)
  min_values <- sorted_x[1:K]
  max_values <- sorted_x[(length(x) - K + 1):length(x)]
  return(list(min_values = min_values, max_values = max_values))
}

# test1
print(minmaxK(x1))

# Zadanie2
lDsknl <- function(x) {
  dziel <- which(x %% 1:(x/2) == 0)
  return(sum(dziel) == x && x > 1)
}

system.time({
  print(which(sapply(c(1:10000), lDsknl)))
})


# Zadanie3
myNorm <- function(x){
  return(x = (x - min(x))/(max(x)-min(x)))
}

# Zadanie4
x4 <- runif(100, 0, 5)
e <- rnorm(100)
y <- x4 + e

myCorr <- function(x, y){ 
  
  if (length(x) != length(y)) {
    stop("Wektory są różnej długości")
  }
  
  pearson <- cor(x, y, method = "pearson")
  kendall <- cor(x, y, method = "kendall")
  spearman <- cor(x, y, method = "spearman")
  
  return(list(Pearson = pearson, Kendall = kendall, Spearman = spearman))
}

# test4
myCorr(x4, y)

# Zadanie5
myStats <- function(x, p){
  if(p == 0){
    
    średnia = mean(x)
    odch_std = sd(x)
    
    return(list(średnia, odch_std))
    
  } else if(p==1) {
    
    mediana = median(x)
    medianowe_odch_bezwzgledne = mad(x)
    
    return(list(mediana, medianowe_odch_bezwzgledne))
  }
}

#test5
myStats(x1, 0)
myStats(x4, 1)

# Zadanie6
myFun <- function(x) 10*sin(1.5*x)* cos(.5*x^3) + (1/2)*sqrt(abs(x))

# a
# uniroot(myFun, c(6, 7))[1]
# powyższy przykład jest zakomentowany, ponieważ nie ma miejsca zerowego na tym przedziale
# program wyrzuca błąd i przerywana wykonywanie dalszych poleceń 

uniroot(myFun, c(1, 2))[1]
uniroot(myFun, c(-5,5))[1]

# b
install.packages("rootSolve")
library(rootSolve)
uniroot.all(myFun, c(-3, 3))


# c
plot(seq(-5,5,by=.1),myFun(seq(-5,5,by=.1)),
     type="l",main='f(x)=10*sin(1.5*x)* cos(.5*x^3) + (1/2)*sqrt(abs(x))',
     cex.main =0.8,xlab='x',ylab='f(x)')
wynik <- uniroot.all(myFun, c(-5, 5))
points(wynik,myFun(wynik),col = "red", pch=19)

#Zadanie7
myLin <- function(x){
  f1 = sum(c(2, 1, -2)*x) + 2
  f2 = sum(c(1, 2, -2)*x) - 1
  f3 = sum(c(2, 1, -1)*x) + 3
  c(f1=f1,f2=f2,f3=f3)
}

multiroot(myLin, c(0, 0, 0))[1]

#Zadanie8
myNonLin <- function(x){
  f1 <- 2 * x[1] + x[2]^2 - 2 * x[3] - 2
  f2 <- x[1]^2 + 2 * x[2] - 2 * x[3] - 3
  f3 <- 2 * x[1] + x[2] - x[3] - 3
  
  c(f1=f1, f2=f2, f3=f3)
}

install.packages("nleqslv")
library(nleqslv)

nleqslv(c(0,0,0), myNonLin)[1]

#Zadanie9
install.packages("rvest")
install.packages("readr")
install.packages("dplyr")
library(stringr)
library(dplyr)
library(httr)
library(rvest)
library(readr)

# Funkcja do usuwania outlierow
remove_outliers <- function(column) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  return(between(column, lower_bound, upper_bound))
}

# Funkcja do pobierania danych z tabeli na stronie Wikipedii
myDane <- function(url) {
  resp <- GET(url)
  page <- read_html(url)
  
  # Wybór pierwszej tabeli na stronie
  tab1 <- page %>% 
    html_nodes(css = "table.wikitable")
  
  # Konwersja tabeli do ramki danych
  tabela <- tab1[[1]] %>% 
    html_table(fill=T)
  
  return(tabela)
}

# Pobranie danych z podanego URL
url <- 'https://pl.wikipedia.org/wiki/Najwi%C4%99ksze_przedsi%C4%99biorstwa_%C5%9Bwiata'
tab <- myDane(url)

# Wyświetlenie struktury danych
str(tab)

# Zamiana spacji w nazwach kolumn na podkreślenia
colnames(tab) <- gsub(" ", "_", colnames(tab))

# Usunięcie wszystkich znaków niebędących cyframi z kolumny 4
tab[[4]] <- as.numeric(gsub("[^0-9]", "", tab[[4]]))

# Przemianowanie kolumny "Przychód(mln $)" na "Przychód_mln_dol"
tab <- tab %>%
  rename(Przychód_mln_dol = `Przychód(mln $)`) %>%
  # Zamiana pustych wartości na NA
  mutate_all(~ ifelse(. == "", NA, .))

# Usunięcie wierszy z brakującymi danymi
tab <- na.omit(tab)

# Usunięcie outlierów z kolumny 4
tab <- tab %>%
  filter(remove_outliers(Przychód_mln_dol))

# Unitaryzacja danych w kolumnie 4
tab[[4]] <- myNorm(tab[[4]])

# Usunięcie podwójnych spacji w kolumnie 7
tab[[7]] <- gsub(",\\s*(.+)", ",\\1", tab[[7]], perl = TRUE)

# Wyświetlenie wybranych kolumn
View(tab[,c(4,6,7)])
