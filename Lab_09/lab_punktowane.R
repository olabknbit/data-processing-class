path ='/Users/aleksandra/Projects/R/Lab_09/';
filename = "LAB_P03c_2.csv"
data <- read.csv(paste(path, filename, sep=''), sep=',')


# Zadanie 1

data.omitted <- na.omit(data)

# wylicz kowariancję zmiennych wifi.1 i wifi.2.
cov(data$wifi.1, data$wifi.2, use = "na.or.complete")

# Wylicz kowariancję tylko dla pełnych danych w tych kolumnach.
cov(data$wifi.1, data$wifi.2, use = "pairwise.complete.obs")
cov(data.omitted$wifi.1, data.omitted$wifi.2)

data_1 <- data.frame(wifi.1 = data$wifi.1, wifi.2 = data$wifi.2)

wifi.1.mean <- mean(data$wifi.1, na.rm = TRUE)
wifi.2.mean <- mean(data$wifi.2, na.rm = TRUE)

data_a <- na.omit(data_1)
cov(data$wifi.1, data$wifi.2, use = "pairwise.complete.obs")

cov_a <- cov(data_a$wifi.1, data_a$wifi.2)
cov_b <- sum((data_a$wifi.1 - wifi.1.mean) * (data_a$wifi.2 - wifi.2.mean)) / (nrow(data_a) - 1)

cat(sprintf("List-wise covariance for full data: %f", cov_a))
cat(sprintf("List-wise covariance for all data: %f", cov_b))


# Zad 2
library(rpart)
library(mice)
library(ggplot2)

# rym
miceAlg <- mice(data, m = 5, method = 'mean', visitSequence = 'monotone', seed=100)
mice.pred_data <- complete(miceAlg)

columnsToRemove <- sum(colSums(is.na(mice.pred_data)) > 0)
firstRemovedColumn <- miceAlg$loggedEvents$out[1]

cat(sprintf("Number of columns to remove: %d", columnsToRemove))
cat(sprintf("Column suggested to be removed as first: %s", firstRemovedColumn))

# j
miceAlg2 <- mice(data, m=5, seed=100, meth="pmm")
miceAlg2$loggedEvents

# Zad 3
require(rpart)
miceAlg3a <- mice(data, m=5, seed=100, meth="norm",  visitSequence = 'monotone',remove.collinear=FALSE)
data.mice.3a <- complete(miceAlg3a)
mean(colMeans(data.mice.3a[, -c(1)]))
# [1] -79.62154

miceAlg3b <- mice(data, m=5, seed=100, meth="cart", remove.collinear=FALSE,  visitSequence = 'monotone')
data.mice.3b <- complete(miceAlg3b)
mean(colMeans(data.mice.3b[, -c(1)]))

# Zad 4

data4 <- data[,-2:-1]
data4[is.na(data4)] <- -113
data4 <- data.frame(data[2], data4)

pvalues <- rep(0, 20)
for (i in 2:20) {
  aov.formula <- sprintf('wifi.%d ~ is.na(wifi.1)', i)
  aov.res <- aov(as.formula(aov.formula), data4)
  pvalues[i] <- unlist(summary(aov.res))["Pr(>F)1"]
}

length(pvalues[pvalues <= 0.001])
min(pvalues[pvalues > 0.001])


