path ='/Users/aleksandra/Projects/R/Lab_05/';
source(paste(path,"Skrypt05.R",sep=''))
# Zad 1
data <- c(3.8, 3.5, 3.9, 3.9, 3.4, 1.8)
data.chauvenet <- Chauvenet(data)
print(data.chauvenet)

# Zad 2
data <- c(3.8, 3.5, 3.9, 3.9, 3.4, 1.8, 1.8)
data.chauvenet <- Chauvenet(data)
print(data.chauvenet)

data <- c(3.8, 3.5, 3.9, 3.9, 3.4, 1.8, 8.8)
data.chauvenet <- Chauvenet(data)
print(data.chauvenet)


# Zad 3
# install.packages("pracma")
library(pracma)

readData <- function(path, filename) {
  mydata = read.csv(paste(path,filename, sep=''),sep=';') 
  return(mydata)
}

distanceACC <- readData(path, "distanceACC.csv")
x <- distanceACC$Accuracy
print(x)
library(ggplot2)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC$Accuracy)) + geom_line()
summary(distanceACC)


distanceACC.movavg <- movavg(distanceACC$Accuracy, 3, type="s")
summary(distanceACC.movavg)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC.movavg)) + geom_line()

distanceACC.movavg <- movavg(distanceACC$Accuracy, 5, type="s")
summary(distanceACC.movavg)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC.movavg)) + geom_line()

distanceACC.movavg <- movavg(distanceACC$Accuracy, 10, type="s")
summary(distanceACC.movavg)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC.movavg)) + geom_line()

# Zad 4
# trojkatna
distanceACC.movavg <- movavg(distanceACC$Accuracy, 3, type="t")
summary(distanceACC.movavg)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC.movavg)) + geom_line()

# modyfikowana
distanceACC.movavg <- movavg(distanceACC$Accuracy, 3, type="m")
summary(distanceACC.movavg)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC.movavg)) + geom_line()

# srednia
distanceACC.movavg <- movavg(distanceACC$Accuracy, 3, type="e")
summary(distanceACC.movavg)
ggplot(distanceACC, aes(distanceACC$Distance, distanceACC.movavg)) + geom_line()

# Zad 5
library(e1071)

running <- readData(path, "runningTime.csv")
running

# ~ oznacza ze zmienna y przyblizamy i kropka oznacza ze wszystkimi innymi zmiennymi bedziemy ja przyblizac
model <- svm(running$y~N+S, type='one-classification', data=running, kernel="linear")
running.results <- predict(model, running)
# # to jest prawdziwy podzial
# ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running$class)))
# to jest moja klasyfikacja
ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running.results))) 
# tu sa pokazane jako FALSE te co zostaly zle zaklasyfikowane
# ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running.results == running$class))) 
# table(running.results, running$class, dnn=c("Prediction", "Actual"))  
# # install.packages('rfUtilities')
# library(rfUtilities)
# accuracy(running.results, running$class)


model <- svm(running$y~N+S, type='one-classification', data=running, kernel="radial")
running.results <- predict(model, running)
ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running.results))) 

# Zad 6
model <- svm(running$y~N+S, type='one-classification', data=running, kernel="radial", nu=0.5, gamma=0.5)
running.results <- predict(model, running)
ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running.results))) 

model <- svm(running$y~N+S, type='one-classification', data=running, kernel="radial", nu=0.1, gamma=0.5)
running.results <- predict(model, running)
ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running.results))) 

model <- svm(running$y~N+S, type='one-classification', data=running, kernel="radial", nu=0.5, gamma=0.1)
running.results <- predict(model, running)
ggplot(running, aes(running$N, running$S))+ geom_point(aes(colour = factor(running.results))) 
