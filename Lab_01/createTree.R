createTree <- function(varNames, estName, method, probe) {

  varNames <- varNames[!varNames %in% c("class")]
  form<- paste(varNames, collapse = "+");
  form <- as.formula(paste(estName, form, sep = " ~ "))
  
  fit <- rpart(form,
               method=method, data=probe)
  
  printcp(fit) # display the results 
  plotcp(fit) # visualize cross-validation results 
  plot(fit, uniform=TRUE, 
       main="Tree")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  #text(fit, use.n=TRUE, all=TRUE, cex=.8)
  return(fit)
}


