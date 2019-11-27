Chauvenet <- function(datapoints){
  numdatapoints <- length(datapoints)
  # calculate normalized distance to mean
  dist <- abs(datapoints - mean(datapoints))/sd(datapoints)
  # calculate probability to have seen such a point assuming a normal
  # distribution
  prob <- numdatapoints*dnorm(dist)
  # select only those points which have a probability >= 0.5
  sel <-  prob>=0.5
  datapoints <- datapoints[sel]
  return(datapoints)
}

data = c(3.8, 3.5, 3.9, 3.4, 1.8, 8.8)
print(Chauvenet(data))

install.packages("pracma")
library(pracma)

library(ggplot2)
distanceACC <- read.csv('h:/Windows7/Desktop/PPD/5/distanceACC.csv', sep=';') 

size3 <- movavg(distanceACC[,2], 3, 's')
ggplot(data.frame(distanceACC[,1], size3), aes(distanceACC[,1], size3)) + geom_line()
size3.min = min(size3)
size3.max = max(size3)
size3.mean = mean(size3)

size5 <- movavg(distanceACC[,2], 5, 's')
ggplot(data.frame(distanceACC[,1], size5), aes(distanceACC[,1], size5)) + geom_line()
size5.min = min(size5)
size5.max = max(size5)
size5.mean = mean(size5)

size10 <- movavg(distanceACC[,2], 10, 's')
ggplot(data.frame(distanceACC[,1], size10), aes(distanceACC[,1], size10)) + geom_line()
size10.min = min(size10)
size10.max = max(size10)
size10.mean = mean(size10)

library(e1071)
runningTime <- read.csv('h:/Windows7/Desktop/PPD/5/runningTime.csv', sep=';')[,1:3] 

model <- svm(y~N+S, type='one-classification', data=runningTime, kernel='radial', nu=0.1, gamma=0.1)
radial <- predict(model, runningTime)
ggplot(data.frame(runningTime, radial), aes(x=runningTime[,1], y=runningTime[,2], colour = radial))+ geom_point() 

model <- svm(y~N+S, type='one-classification', data=runningTime, kernel='linear', nu=0.1, gamma=0.1)
linear <- predict(model, runningTime)
ggplot(data.frame(runningTime, linear), aes(x=runningTime[,1], y=runningTime[,2], colour = linear))+ geom_point() 

model <- svm(y~N+S, type='one-classification', data=runningTime, kernel='sigmoid', nu=0.1, gamma=0.1)
sigmoid <- predict(model, runningTime)
ggplot(data.frame(runningTime, sigmoid), aes(x=runningTime[,1], y=runningTime[,2], colour = sigmoid))+ geom_point() 


model <- svm(y~N+S, type='one-classification', data=runningTime, kernel='radial', nu=0.5, gamma=0.5)
radial <- predict(model, runningTime)
ggplot(data.frame(runningTime, radial), aes(x=runningTime[,1], y=runningTime[,2], colour = radial))+ geom_point() 

model <- svm(y~N+S, type='one-classification', data=runningTime, kernel='radial', nu=0.5, gamma=0.1)
radial <- predict(model, runningTime)
ggplot(data.frame(runningTime, radial), aes(x=runningTime[,1], y=runningTime[,2], colour = radial))+ geom_point() 

model <- svm(y~N+S, type='one-classification', data=runningTime, kernel='radial', nu=0.1, gamma=0.5)
radial <- predict(model, runningTime)
ggplot(data.frame(runningTime, radial), aes(x=runningTime[,1], y=runningTime[,2], colour = radial))+ geom_point() 
