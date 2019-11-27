library(devtools)


path ='/Users/aleksandra/Projects/R/Lab_04/';

readLabData <- function(path, filename) {
  mydata = read.csv(paste(path,filename,sep='')) 
  return(mydata)
}

dat = readLabData(path,'LAB_P01b_2.csv')
dat <- scale(dat)

dat.pca <- prcomp (dat, center = TRUE, scale. = TRUE)
summary(dat.pca)

# zad2 kryterium minimalnego zasobu zmienności wspólnej 
# Suppose that it is required to keep a certain set of variables in the analysis. Then
# enough components should be extracted so that the communalities for each of these
# variables exceeds a certain threshold (e.g., 50%)
kwadrat <-function(x) {
  return(x^2)
}
kwadraty = apply(dat.pca$rotation, c(1,2), kwadrat)
print(rowSums(kwadraty[,1:48]))
print(rowSums(kwadraty[,1:49]))

# 49

#Zad 3
# analiza czynnikowa

l.fit <- factanal(dat, factors=12, rotation="none", scores="Bartlett" )
print(l.fit, cutoff = .3, digits=2, sort=TRUE)


#Zad 4 // 6 i 6
# analiza czynnikowa
dat.fit <- factanal(dat, factors=10, rotation="none", scores="Bartlett", lower=0.05 )
print(dat.fit, cutoff = .3, digits=2, sort=TRUE)

dat.fit <- factanal(dat, factors=10, rotation="varimax", scores="Bartlett", lower=0.05 )
print(dat.fit, cutoff = .3, digits=2, sort=TRUE)

