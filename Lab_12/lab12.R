path ='/Users/aleksandra/Projects/R/Lab_12/';
filename = "LAB_P04b_2.csv"
data <- read.csv(paste(path, filename, sep=''), sep=',')

# Zad 1 a
Z <- 0.05
E <- 0.05

s <- sd(data$Flow)
n <- Z^2 * s^2 / E^2
n
# 84196.15

# Zad 1 b
require(dplyr)

data.pop <- data %>% filter(DataQuality == 1)
wspl_odp = nrow(data.pop) / nrow(data)
wspl_odp

sb <- sd(data.pop$Flow)
# nb <- Z^2 * sb^2 / (E^2)
# nb
# [1] 84180.37 <- czy to jest poprawne - nie
nb <- Z^2 * sb^2 / (E^2 * wspl_odp)
nb
# [1] 85258.65 <- czy to? - tak

# Zad 2
data.median <- median(data$TimePeriod)
data.median 
data.train <- data %>% filter(data$TimePeriod < data.median)
data.test <- data %>% filter(data$TimePeriod >= data.median)

# losowe
set.seed(100)
means <- c()
for (i in 1:100) {
  indeksy <- sample(nrow(data.train), as.integer(90), replace=FALSE, prob=NULL)
  sample_random <- data.train[indeksy,]
  srm <- mean(sample_random$Flow)
  means <- append(means, srm)
}
mean_random <- mean(means)
mean_random
# [1] 509.0299
sd_random<- sd(means)
sd_random
# [1] 52.58909

# sekwencyjne
set.seed(100)
means <- c()
for (i in 1:100) {
  sample_ind <- seq(from = i , to = nrow(data.train), length.out = 90 )
  sample_seq <- data.train[sample_ind,]
  srm <- mean(sample_seq$Flow)
  means <- append(means, srm)
}
mean_seq <- mean(means)
mean_seq
# [1] 494.3732
sd_seq<- sd(means)
sd_seq
# [1] 19.63342

# stratified
# install.packages("splitstackshape")
library(splitstackshape)

set.seed(100)
means <- c()
for (i in 1:100) {
  out <- stratified(data.train, c("LinkRef"), 90)
  srm <- mean(out$Flow)
  means <- append(means, srm)
}
mean_strat <- mean(means)
mean_strat
# [1] 504.0041
sd_strat <- sd(means)
sd_strat
# [1] 28.47997

# Zad 2 b
meanss<- c(mean_random, mean_seq, mean_strat)
(max(meanss) - min(meanss)) / min(meanss) * 100
# [1] 3.130589 %

# Zad 3
require(dplyr)
require(caret)
require(rpart)
data.pop.train <- data.pop %>% filter(data.pop$TimePeriod <= 20)
data.pop.test <- data.pop %>% filter(data.pop$TimePeriod > 20)

set.seed(31415)
train.control <- trainControl(method="LOOCV")
model <- train(LinkRef~Flow+AverageSpeed, data.pop.train, method="knn", trControl=train.control)
model
# No pre-processing
# Resampling: Leave-One-Out Cross-Validation 
# Summary of sample sizes: 1673, 1673, 1673, 1673, 1673, 1673, ... 
# Resampling results across tuning parameters:
#   
#   k  Accuracy   Kappa    
# 5  0.7520908  0.6275351
# 7  0.7622461  0.6428013
# 9  0.7622461  0.6427496
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 9.
predicted <- predict(model, data.pop.test)
res <- postResample(predicted, data.pop.test$LinkRef)
res
#   Accuracy      Kappa 
# 0.37530188 0.05686655 

# Zad 4
data.zad4.train <- data.pop %>% filter(data.pop$TimePeriod == 47)
data.zad4.test <- data.pop %>% filter(data.pop$TimePeriod != 47)
zad <- function(methodTrainControl, methodTrain) {
  train.control <- trainControl(method=methodTrainControl, search="random")
  model <- train(LinkRef~Flow+AverageSpeed, data.zad4.train, method=methodTrain, trControl=train.control)
  # print(model)
  predicted <- predict(model, data.zad4.test)
  res <- postResample(predicted, data.zad4.test$LinkRef)
  return(res)
}

accs.boot <- c()
for (i in 1:10) {
  res <- zad("boot", "rpart")
  accs.boot <- append(accs.boot, res["Accuracy"])
  
}
# mean(accs.boot)

accs.boot632 <- c()
for (i in 1:10) {
  res <- zad("boot632", "rpart")
  accs.boot632 <- append(accs.boot632, res["Accuracy"])
  
}
mean(accs.boot)
mean(accs.boot632)


