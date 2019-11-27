findClusters <- function(speeds, iterations, probe, columnName) {
k<-length(speeds)
clusters <- kmeans(probe[columnName], k, iterations);
classes <- clusters$cluster
ord <-sort(clusters$centers, index.return=TRUE)

for(i in 1:k)
{
  classes[classes==i]  = speeds[ord$ix ==i]
}
classes = factor(classes,levels=speeds,ordered=TRUE)


return(classes)
}