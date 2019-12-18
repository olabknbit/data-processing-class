path ='/Users/aleksandra/Projects/R/Lab_11/';
# filename = "LAB_P03b_2.csv"
filename = "mfeb15.csv"
data <- read.csv(paste(path, filename, sep=''), sep=',')

require(dplyr)
require(caret)
require(rpart)
data.train <- data %>% filter(isLearning == TRUE)
data.test <- data %>% filter(isLearning == FALSE)
y_true = data.test[,1]


zad <- function(methodTrainControl, methodTrain) {
  train.control <- trainControl(method=methodTrainControl)
  model <- train(data.train[,-1], data.train[,1], method=methodTrain, trControl=train.control)
  predicted <- predict(model, data.test)
  res <- postResample(predicted, y_true)
  return(res)
}

# Zad 1
z1 <- zad("none", "rpart")
# Zad 2 boot
z2boot <- zad("boot", "rpart")
# Zad 2 boot632
z2boot632 <- zad("boot632", "rpart")
# Zad cv
z2cv <- zad("cv", "rpart")
# Zad LOOCV 
z2LOOCV <- zad("LOOCV", "rpart")


# Zad 3 boot
z3boot <- zad("boot", "rf")
# Zad 2 boot632
z3boot632 <- zad("boot632", "rf")
# Zad cv
z3cv <- zad("cv", "rf")
# Zad LOOCV 
z3LOOCV <- zad("LOOCV", "rf")

z3boot[1]


df <- data.frame(error = z2boot[1], method="boot rpart")
df<-rbind(df, data.frame(error = z2boot632[1], method="boot632 rpart"))
df<-rbind(df, data.frame(error = z2cv[1], method="cv rpart"))
df<-rbind(df, data.frame(error = z2LOOCV[1], method="LOOCV rpart"))
df<-rbind(df, data.frame(error = z3boot[1], method="boot rf"))
df<-rbind(df, data.frame(error = z3boot632[1], method="boot632 rf"))
df<-rbind(df, data.frame(error = z3cv[1], method="cv rf"))
df<-rbind(df, data.frame(error = z3LOOCV[1], method="LOOCV rf"))

ggplot(df, aes(error, color=method)) +geom_point(aes(method,y=error))
