#Amelia Madej

#Zadanie1
set.seed(11)
n <- sample(seq(5, 29, 2), 1)
B <- matrix(0, nrow = n, ncol = n)
diag(B) <- 1:n
diag(B[, n:1]) <- 1:n
print(B)

#Zadanie2
set.seed(41)
C <- matrix(runif(50, 2, 5), 5, 10)
D <- matrix(runif(50, 2, 5), 5, 10)
#a
print(t(C)%*%D)
#b
print(crossprod(t(C), t(D)))

#Zadanie3
x <- c(as.vector(C), as.vector(D))
set.seed(40)
x[sample(length(x), 20)] <- NA
#a
print(mean(x, na.rm = TRUE))
#b
print(sd(x, na.rm = TRUE))

#Zadanie4
#a
m <- 10
print(outer(1:m-1, 1:m-1, "+") %% m)
#b
n <- 9
print(outer(1:n-1, n:1, "+") %% n)

#Zadanie5
set.seed(31)
G <- matrix(sample(-20:20, 200, replace = TRUE), nrow = 20, ncol = 10)
print(G)
print(t(apply(G, 1, sort))[,9:10])

#Zadanie6
set.seed(31)
E <- matrix(sample(1:10, 60, replace = TRUE), nrow = 6, ncol = 10)
#a
pary <- which(outer(colSums(E), colSums(E), '+') > 75, arr.ind = TRUE, useNames = FALSE)
print(pary)
#b
print(pary[pary[, 1] < pary[, 2], ])

#Zadanie7
set.seed(27)
#a
x <- sample(20:27, 200, replace = TRUE)
print(x)

#b
y <- character(length(x))
y[x >= 20 & x <= 22] <- sample(c("lic", "inż."), sum(x >= 20 & x <= 22), replace = TRUE, prob = c(0.4, 0.6))
y[x < 20 | x > 22] <- sample(c("mgr", "mgr inż."), sum(x < 20 | x > 22), replace = TRUE, prob = c(0.3, 0.7))
print(y)

#c
z <- sample(c("Kraków", "Warszawa", "Katowice", "Rzeszów", "Częstochowa"), 200, replace = TRUE)
print(z)

#d
dane.stud <- data.frame("wiek" = x,
                        "wykształcenie" = y,
                        "adres" = z)
print(dane.stud)

#Zadanie8
#a
print(nrow(na.omit(dane.stud)))
#b
print(sum(!duplicated(dane.stud)))
#c
print(table(dane.stud$wiek))

#Zadanie9
#a
print(subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa"), select = c("wiek", "adres")))
#b
print(subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa")))

#Zadanie10
library(gridExtra)
wyniki <- round(tapply(dane.stud$wiek, list(dane.stud$wykształcenie, dane.stud$adres), mean), 2)
wyniki_df <- as.data.frame(wyniki)
tabela <- tableGrob(wyniki_df)
#grid.arrange(tabela)

#Zadanie11
set.seed(23)
lista1 <- lapply(1:6, runif, min = 2, max = 8)
print(lista1)

#Zadanie12
lista2 <- lapply(1:6, runif, min = 2, max = 8)
print(lista2)
print(mapply('+', lista1, lista2, SIMPLIFY = FALSE))

