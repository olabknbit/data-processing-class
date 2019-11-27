path ="Lab_06/"
filename = "fauna.csv"

#Zadanie1
data = read.csv(paste('', filename, sep=''), sep=';') #colClasses=c("NULL", NA, NA, NA, NA, NA, NA)) 

var.test(data$ciężar_max, data$ciężar_min, ratio = 1, alternative = "two.sided", conf.level = 0.95)
var.test(data$ciężar_max, data$długość_min, ratio = 1, alternative = "two.sided", conf.level = 0.95)
var.test(data$ciężar_max, data$długość_max, ratio = 1, alternative = "two.sided", conf.level = 0.95)
var.test(data$ciężar_max, data$ogon_min, ratio = 1, alternative = "two.sided", conf.level = 0.95)
var.test(data$ciężar_max, data$ogon_max, ratio = 1, alternative = "two.sided", conf.level = 0.95)

# patrzymy tylko na p-value?
# gdyby p-value bylo bardzo male (~5%) to wtedy moglibysmy odrzucic hipoteze zerowa
# to jest podobno zalezne od conf.level?

#Zadanie2
var.test(data$ciężar_max, data$ciężar_min, ratio = 1, alternative = "less", conf.level = 0.95)
var.test(data$ciężar_max, data$długość_min, ratio = 1, alternative = "less", conf.level = 0.95)
var.test(data$ciężar_max, data$długość_max, ratio = 1, alternative = "less", conf.level = 0.95)
var.test(data$ciężar_max, data$ogon_min, ratio = 1, alternative = "less", conf.level = 0.95)
var.test(data$ciężar_max, data$ogon_max, ratio = 1, alternative = "less", conf.level = 0.95)

var.test(data$ciężar_max, data$ciężar_min, ratio = 1, alternative = "greater", conf.level = 0.95)
var.test(data$ciężar_max, data$długość_min, ratio = 1, alternative = "greater", conf.level = 0.95)
var.test(data$ciężar_max, data$długość_max, ratio = 1, alternative = "greater", conf.level = 0.95)
var.test(data$ciężar_max, data$ogon_min, ratio = 1, alternative = "greater", conf.level = 0.95)
var.test(data$ciężar_max, data$ogon_max, ratio = 1, alternative = "greater", conf.level = 0.95)

#Zmiana hipotezy alternatywnej nie zmienia uzyskane wyniki

#Zadanie3
obserwowane_zmienne <- data$ciężar_min + data$ciężar_max + data$długość_min + data$długość_max + data$ogon_min + data$ogon_max

formula <- data$ciężar_max~data$Rodzina
aov.res <- aov(formula, data)
summary(aov.res)

#patrzymy na ** i im mniejsza wartosc tym lepsza, poniewaz p-value to jest odwrotnosc tego (1-)

TukeyHSD(aov.res)

#psowate-lasicowate sa dobrze separowane przez zmienna

formula <- data$ogon_min~data$Rodzina
aov.res <- aov(formula, data)
summary(aov.res)

#zmienna ogon_min bedzie lepszym wyborem

#Zadanie4
# install.packages("glmnet")
library(glmnet)

obs <- matrix(c(data$ciężar_min, data$długość_min, data$długość_max, data$ogon_min, data$ogon_max), ncol=5);
View(obs)
reg.lasso <- glmnet(as.matrix(obs), as.matrix(data$ciężar_max), alpha = 1)

plot(reg.lasso, xvar = "lambda", label=TRUE)
# ogon_min zostanie wyeliminowana jako pierwsza

#Zadanie5

library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
obserwowane_zmienne <- data$ciężar_min + data$ciężar_max + data$długość_min + data$długość_max + data$ogon_min + data$ogon_max

formula <- data$Rodzina~data$ciężar_min + data$ciężar_max + data$długość_min + data$długość_max + data$ogon_min + data$ogon_max
decision.tree <- rpart(formula, data)

rpart.plot(decision.tree)
summary(decision.tree)




