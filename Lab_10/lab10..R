path ='/Users/aleksandra/Projects/R/Lab_10/';
filename = "FEB15.csv"
data <- read.csv(paste(path, filename, sep=''), sep=',')


# Zad 1 wielkosc probki
Z <- 0.05
E <- 0.05

s <- sd(data$AverageSpeed)
n <- Z^2 * s^2 / E^2
n
# 292.6933

E <- 0.01
n <- Z^2 * s^2 / E^2
n
# 7317.333

# Zad 2
require(dplyr)

data.pop <- data %>% filter(DataQuality == 1)
wspl_odp = nrow(data.pop) / nrow(data)
wspl_odp
# 0.7794894

s <- sd(data.pop$AverageSpeed)
n <- Z^2 * s^2 / (E^2 * wspl_odp)
n
# [1] 9791.659


# Zad 3
data$Date <- as.Date(data$Date)
summary(data)
med <- median(data$Date)
med
train <- data %>% filter(Date <= med)
test <- data %>% filter(Date > med)


set.seed(100)
indeksy <- sample(nrow(train), as.integer(n), replace=FALSE, prob=NULL)
samplee <- train[indeksy,]

samplee.pop <- samplee %>% filter(DataQuality==1)
test.pop <- test %>% filter(DataQuality==1)

#Zad 4

library(rpart)
cart.tree.2 <- rpart(AverageSpeed~Date+TimePeriod+LinkLength, data=samplee.pop)
cart.tree.pred.2 <- predict(cart.tree.2, test.pop)
cart.tree.pred.me <- mean(abs(test.pop$AverageSpeed - cart.tree.pred.2))
cart.tree.pred.me
# 10.38131


# Zad 5
to <- nrow(train)
from <-1
samplee5 <- seq(from = from , to = to, by =((to-from)/(n-1) ) )
samplee5 <- train[samplee5,]

samplee5.pop <- samplee5 %>% filter(DataQuality==1)
test.pop <- test %>% filter(DataQuality==1)

library(rpart)
cart.tree.2 <- rpart(AverageSpeed~Date+TimePeriod+LinkLength, data=samplee5.pop)
cart.tree.pred.2 <- predict(cart.tree.2, test.pop)
cart.tree.pred.me <- mean(abs(test.pop$AverageSpeed - cart.tree.pred.2))
cart.tree.pred.me
# 10.53277

# Zad 6

