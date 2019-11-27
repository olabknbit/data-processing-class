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

