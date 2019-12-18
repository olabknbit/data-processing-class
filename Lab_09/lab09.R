path ='/Users/aleksandra/Projects/R/Lab_09/';
# filename = "LAB_P03b_2.csv"
filename = "LAB_P03a_16.csv"
data <- read.csv(paste(path, filename, sep=''), sep=',')

# Zad 1
# List-wise deletion
data.omitted <- na.omit(data[c("wifi.1", "wifi.2")])
cov(data.omitted$wifi.1, data.omitted$wifi.2, use = "na.or.complete")
# [1] 8.095238

cov(data$wifi.1, data$wifi.2, use = "complete.obs")
# [1] 8.095238
# ODP 8.095238 i ?

# Zad 2

# install.packages("mice")
require(mice)
miceAlg2 <- mice(data, m=5, seed=100, meth="pmm")
miceAlg2$loggedEvents
# it im dep      meth     out
# 1  0  0     collinear  wifi.6
# 2  0  0     collinear wifi.14
# 3  0  0     collinear wifi.17
# 4  0  0     collinear wifi.10

# ODP 4 i wifi.6

# Zad 3
require(rpart)
miceAlg3a <- mice(data, m=5, seed=100, meth="norm", remove.collinear=FALSE)
data.mice.3a <- complete(miceAlg3a)
mean(colMeans(data.mice.3a[, -c(1)]))
# [1] -76.85119

miceAlg3b <- mice(data, m=5, seed=100, meth="cart", remove.collinear=FALSE)
data.mice.3b <- complete(miceAlg3b)
mean(colMeans(data.mice.3b[, -c(1)]))
# [1] -77.3764


# Zad 4
require(tidyverse)
data.filled <- data
data.filled<- data.filled %>% replace(is.na(.), -113)
data.filled$wifi.1 <- data$wifi.1

# Czemu to nie dziala?
aov.res <- aov(.~is.na(wifi.1), data = data.filled[,-1])
summary(aov.res)


