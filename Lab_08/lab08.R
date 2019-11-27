path ='/Users/aleksandra/Projects/R/Lab_08/';
filename = "wifi.csv"
data <- read.csv(paste(path, filename, sep=''), sep=',')

data.omitted <- na.omit(data)

# Zad 1
# wylicz kowariancję zmiennych wifi.1 i wifi.2.
cov(data$wifi.1, data$wifi.2, use = "na.or.complete")

# Wylicz kowariancję tylko dla pełnych danych w tych kolumnach.
# Luckner mowi ze powinno byc 115 i 116 te wariancje a mi wychodzi inaczej ?? o co cho
# cov(data$wifi.1, data$wifi.2, use = "pairwise.complete.obs")
cov(data.omitted$wifi.1, data.omitted$wifi.2)

# Następnie wylicz kowariancję obliczając średnią korzystając ze wszystkich dostępnych danych w kolumnie.
# TODO

# Zad 2
# Dla danych z pliku wifi.csv zbuduj drzewo CART pozwalające na estymację zmiennej y na podstawie siły sygnałów Wi-Fi.
library(rpart)
cart.tree.2 <- rpart(y~., data=data)
summary(cart.tree.2)

cart.tree.pred.2 <- predict(cart.tree.2, data[,-1])
cart.tree.pred.2

# to jest mse, ktorego nie chcemy
cart.tree.pred.mse <- mean((data$y - cart.tree.pred)^2)
cart.tree.pred.mse 

# Wylicz średni błąd regresji, używając danych uczących jako dane testowe
cart.tree.pred.me.2 <- mean(data$y - cart.tree.pred.2)
cart.tree.pred.me.2

# Zad 3
# Dla danych z pliku wifi.csv zbuduj drzewo CART pozwalające na estymację zmiennej y na podstawie siły 
# sygnałów Wi-Fi.
# Zastąp brakujące sygnały wartością średnią dla danego źródła
data.replaced <- data
for(i in 1:ncol(data)){
  data.replaced[is.na(data.replaced[,i]), i] <- mean(data.replaced[,i], na.rm = TRUE)
}

cart.tree.3 <- rpart(y~., data=data.replaced)
cart.tree.pred.3 <- predict(cart.tree.3, data.replaced[,-1])
# Wylicz średni błąd regresji, używając danych uczących jako dane testowe.
cart.tree.pred.me.3 <- mean(data.replaced$y - cart.tree.pred.3)
cart.tree.pred.me.3

# Zad 4
# Dla danych z pliku wifi.csv zbuduj drzewo CART pozwalające
# na estymację zmiennej y na podstawie siły sygnałów Wi-Fi.
# Zastąp brakujące sygnały stosując metodę MICE z pięcioma iteracjami.

install.packages("mice")
require(mice)
miceAlg4 <- mice(data, m=5)
data.mice.4 <- complete(miceAlg4)
mice.cart.tree.4 <- rpart(y~., data=data.mice.4)
mice.cart.tree.pred.4 <- predict(mice.cart.tree.4, data.mice.4[,-1])
# Wylicz średni błąd regresji, używając danych uczących jako dane testowe.
mice.cart.tree.pred.me.4 <- mean(data.mice.4$y - mice.cart.tree.pred.4)
mice.cart.tree.pred.me.4

# Zad5
# Dla danych z pliku wifi.csv zbuduj drzewo CART pozwalające
# na estymację zmiennej y na podstawie siły sygnałów Wi-Fi.
# Zastąp brakujące sygnały stosując metodę MICE z pięcioma iteracjami.
# Zastosuj inputację prostą metodą CART i sekwencję monotonną.

miceAlg5 <- mice(data, m=5, method="cart", visitSequence="monotone")
data.mice.5 <- complete(miceAlg5)
mice.cart.tree.5 <- rpart(y~., data=data.mice.5)
mice.cart.tree.pred.5 <- predict(mice.cart.tree.5, data.mice.5[,-1])
# Wylicz średni błąd regresji, używając danych uczących jako dane testowe.
mice.cart.tree.pred.me.5 <- mean(data.mice.5$y - mice.cart.tree.pred.5)
mice.cart.tree.pred.me.5

# Zad 6
# Porównaj wyniki uzyskane w zadaniach 2-5.
# Wykreśl dystrybucję błędów metod.
# Korzystając z funkcji xlim i ylim ogranic

require(ggplot2)
errTree <- cart.tree.pred.2 - data$y
errImp <-  cart.tree.pred.3 - data$y
errMice <-  mice.cart.tree.pred.4 - data$y
errMiceCart <- mice.cart.tree.pred.5 - data$y

df <- data.frame(error = errTree, method="cart tree")
df<-rbind(df, data.frame(error = errImp, method="imp"))
df<-rbind(df, data.frame(error = errMice, method="mice"))
df<-rbind(df, data.frame(error = errMiceCart, method="mice cart"))

ggplot(df, aes(error, color=method))+stat_ecdf() + xlim(0,15)



