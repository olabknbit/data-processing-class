# install.packages("devtools")
library(devtools)
# install_github("vqv/ggbiplot")

path ='/Users/aleksandra/Projects/R/Lab_03/';

# "zone","powMieszkalna","powUsługowa","powHandlowa","lMieszkańców","lDzieci","lMiejscPracy"
readWawData <- function(path, filename) {
  mydata = read.csv(paste(path,filename,sep='')) 
  # don't select zone which is the first column
  mydata = mydata[,2:ncol(mydata)]
  
  return(mydata)
}

waw = readWawData(path,'wawData.csv')
waw.pca <- prcomp (waw, center = TRUE, scale. = TRUE)
print(waw.pca)
summary(waw.pca)


require(ggbiplot)
ggbiplot(waw.pca)
ggscreeplot(waw.pca)
print(waw.pca$rotation)

# zad1 http://www.mini.pw.edu.pl/~lucknerm/userfiles/downloads/PPD/PodstawyPrzetwarzaniaDanychWykład02.pdf slajd 23
## kryterium wartosci wlasnej (eigenvalue criterion)

# Importance of components:
#                         PC1    PC2    PC3    PC4     PC5     PC6
# Standard deviation     1.6333 1.3533 0.9642 0.5190 0.42193 0.35186
# Proportion of Variance 0.4446 0.3052 0.1550 0.0449 0.02967 0.02063
# Cumulative Proportion  0.4446 0.7498 0.9048 0.9497 0.97937 1.00000

# stąd wnioskujemy ze wg kryterium wartosci wlasnej nalezy wziac pierwsze 2 componenty, 
# bo PC1 i PC2 maja wartosci powyzej 1. mozemy chyba tez wziac 3 bo jest prawie 1

# Kryterium części wyjaśnionej wariancji (90%),
# wg tego bierzemy PC1-Pc3 bo łącznie wyjasniaja 90% wariancji
print(0.4446+0.3052+0.1550)
# Kryterium wykresu osypiskowego. - odcinamy 4-6
# The maximum number of components that should be extracted is just prior to where 
# the plot first begins to straighten out into a horizontal line


# zad2 kryterium minimalnego zasobu zmienności wspólnej 
# Suppose that it is required to keep a certain set of variables in the analysis. Then
# enough components should be extracted so that the communalities for each of these
# variables exceeds a certain threshold (e.g., 50%)
kwadrat <-function(x) {
  return(x^2)
}
kwadraty = apply(waw.pca$rotation, c(1,2), kwadrat)

print(kwadraty)
print(rowSums(kwadraty[,1:2]))
print(rowSums(kwadraty[,1:3]))
print(rowSums(kwadraty[,1:4]))
print(rowSums(kwadraty[,1:5]))
# ile w koncu bierzemy?

# analiza czynnikowa
waw.fit <- factanal(waw, factors=2, rotation="none", scores="Bartlett" )

# w takiej ankiecie mamy 1 stopien swobody
#   | T | N 
# K | X |
# M |   |
#   |100|100

#Zad 3
print(waw.fit, digits=2, cutoff = .3, sort=TRUE)

firRot <- factanal ( waw , factors =2, rotation="none" ,
                     scores="Bartlett" )

print(firRot)
# Zad4
firRot <- factanal ( waw , factors =2, rotation="varimax" ,
                     scores="Bartlett" )

print(firRot)
