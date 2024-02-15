#Amelia Madej

#Zadanie 1
#a
print(c(1:20, 19:1))
#b
print(rep(c(4,6,3), length = 50))
#c
print(rep(c(4, 6, 3), times = c(10, 20, 30)))
#d
print(seq(100, 4, by = -8))
#e
print((0.1^seq(3, 36, by = 3)) * (0.2^seq(1, 34, by = 3)))

#Zadanie 2
print(paste0(c("A_", "X_"), 1:30, c(".B", ".D", ".F")))

#Zadanie3
set.seed(50)
#a
print(sample(5:15, 100, replace = T))
#b
print(sample(c(LETTERS, letters), 100, replace = T))

#Zadanie 4
set.seed(30)
x <- sample(0:999, 250)
y <- sample(0:999, 250)
#a
print(x[1:248] + 2 * x[2:249] - y[3:250])
#b
print(sum(exp((-1)*x[2:250])/(x[1:249]+10)))
#c
print(sum(((1:20)^4)*sum(1/(3 + (1:5)))))

#Zadanie 5
set.seed(50)
x <- sample(0:999, 500)
#a
print(sum(x %% 2 == 0))
#b
print(sum((x %% 2 == 0) & (x %% 3 == 0)))
#c
print(sum((x < 30) | (x > 70)))
      
#Zadanie 6
napis <- c("Katedra", "Informatyki", "Biznesowej", "i", "Inżynierii", "Zarządzania", "WZ", "AGH", 2022)
#a
a <- table(unlist(lapply(napis, strsplit, split = NULL)))
print(a)
#b
print(names(which(a == max(a))))