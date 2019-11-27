library(e1071)
library(glmnet)

data <- read.csv('distanceACC.csv', header = TRUE, sep = ';')

# Zadanie 1

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

x <- data$ciężar_min
y <- Chauvenet(x)

rmvd <- x[! x %in% y]
cat(sprintf("Number of removed observations: %d", length(rmvd)))
cat(sprintf("Minimal removed observation: %d", min(rmvd)))

# Zadanie 2

formula = Rodzina ~ długość_max + ciężar_max + ogon_max

svm.model1 <- svm(formula, type='one-classification', data = data.frame(data, y = 1), 
             kernel = 'linear', nu = 0.5)
svm.res1 <- predict(svm.model1, data)
cat(sprintf("Number of observations discarded by linear kernel SVM: %d", sum(!svm.res1)))

svm.model2 <- svm(formula, type='one-classification', data = data.frame(data, y = 1), 
             kernel = 'radial', nu = 0.1, gamma = 0.5)
svm.res2 <- predict(svm.model2, data)
cat(sprintf("Number of observations discarded by radial kernel SVM: %d", sum(!svm.res2)))

# Zadanie 3

aov.res <- aov(ogon_min ~ Rodzina, data)
tukey.res <- TukeyHSD(aov.res, conf.level = 0.95)$Rodzina

indiff_pairs = tukey.res[, "p adj"] > 0.05
pair_names <- rownames(tukey.res)

cat(sprintf("Number of class pairs inseperable by variable: %d", 
            sum(indiff_pairs)))
cat(sprintf("The least probability of test statistic for inseparable classes : %f", 
            min(tukey.res[indiff_pairs, "p adj"])))

# Zadanie 4

x <- as.matrix(data[c('ciężar_min', 'ciężar_max', 'długość_max', 'ogon_min', 'ogon_max')])
y <- as.matrix(data['długość_min'])
reg.lasso <- glmnet(x, y, alpha = 1.0)

lasso.beta <- reg.lasso$beta

any_excluded <- apply(lasso.beta, 2, (function(x) any(x == 0)))
all_excluded <- apply(lasso.beta, 2, (function(x) all(x == 0)))
n <- ncol(lasso.beta)
vars <- rownames(lasso.beta)

index_any_excl <- max(seq(1, n)[any_excluded])
index_all_excl <- max(seq(1, n)[all_excluded]) + 1

first_removed_var <- vars[lasso.beta[, index_any_excl] == 0] 
last_removed_var <- vars[lasso.beta[, index_all_excl] != 0]

searched_beta <- lasso.beta[last_removed_var, index_any_excl]

cat(sprintf("First removed variable: %s", first_removed_var))
cat(sprintf("Beta for last removed variable when first variable was removed: %f", 
            lasso.beta[last_removed_var, index_any_excl]))
