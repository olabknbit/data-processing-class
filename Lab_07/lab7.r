path ='/Users/aleksandra/Projects/R/Lab_07/';
filename = "LAB_P02b_2.csv"
data = read.csv(paste(path, filename, sep=''), sep=',')

Chauvenet <- function(datapoints){
  numdatapoints <- length(datapoints)
  # calculate normalized distance to mean
  dist <- abs(datapoints - mean(datapoints))/sd(datapoints)
  # calculate probability to have seen such a point assuming a normal
  # distribution
  prob <- numdatapoints*dnorm(dist)
  # select only those points which have a probability >= 0.5
  sel <-  prob>=0.5
  
  # wypisujemy te co je odrzucamy
  print(datapoints[!sel])
  datapoints <- datapoints[sel]
  
  return(datapoints)
}

#Zadanie1
library(pracma)

summary(data)
data.chauvenet <- Chauvenet(data$ciężar_min)
# ile usunietych zostalo wartosci
nrow(data) - nrow(data.frame(data.chauvenet))


#jak sprawdzic ktora usunieta wartosc miala najmniejsza wartosc?

#Zad2
library(e1071)

model <- svm(data$Rodzina~długość_max+ciężar_max+ogon_max, type='one-classification', data=data, kernel="linear", nu=0.5)
data.results <- predict(model, data)
data.results
# ile zostalo odrzucownych przez liniowe = 15
as.data.frame(table(data.results))

model <- svm(data$Rodzina~data$długość_max+data$ciężar_max+data$ogon_max, type='one-classification', data=data, kernel="radial", nu=0.1, gamma=0.5)
data.results <- predict(model, data)
data.results
# ile zostalo odrzucownych przez radialne = 6
as.data.frame(table(data.results))


# Zad 3
formula <- data$ogon_min~data$Rodzina
aov.res <- aov(formula, data)
summary(aov.res)
TukeyHSD(aov.res)


formula <- data$ciężar_max~data$Rodzina
aov.res <- aov(formula, data)
summary(aov.res)
TukeyHSD(aov.res)

# Zad 4
library(glmnet)

obs <- matrix(c(data$ciężar_max, data$ciężar_min, data$długość_max, data$ogon_min, data$ogon_max), ncol=5);
reg.lasso1 <- glmnet(as.matrix(obs), as.matrix(data$długość_min), alpha = 1)
plot(reg.lasso1, xvar = "lambda", label=TRUE)
#coef(reg.lasso1)
# usuwamy sobie zmienna ogon max bo najszybciej zbiega do zera (na wykresie)
#Ostatnia zostanie usunieta zmienna 3 bo najpozniej zbiega do 0 na wykresie
reg.lasso1$beta

coef(reg.lasso1)






