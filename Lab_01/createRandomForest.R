createRandomForest <- function(varNames, estName, nTree, probe) {
 
  varNames <- varNames[!varNames %in% c(estName)]
  form<- paste(varNames, collapse = "+");
  form <- as.formula(paste(estName, form, sep = " ~ "))
  
  probe.rf <- randomForest(form,
                           probe,
                           ntree=nTree,
                           importance=T)
  
  plot(probe.rf)
  
  # Variable Importance Plot
  #varImpPlot(probe.rf,
  #           sort = T,
  #           main="Variable Importance")
  
  return(probe.rf)
}