readTrafficData <- function(path, filename, PL=1) {
  mydata = read.csv(paste(path,filename,sep='')) 

  
  library(plyr)
  library(dplyr)
  mydata <- mydata %>%
   filter(mydata$DataQuality == 1) %>%
   select(-LinkDescription)
  
  mydata$Hour = as.integer(mydata$TimePeriod*15/60)
  mydata$DayOfWeek = weekdays(as.Date(mydata$Date))
  if(PL==1)
  {
  dow =c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela")
  }
  else
  {
  dow  = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")
  }
  mydata$DayOfWeek = factor(mydata$DayOfWeek,levels=dow,ordered=TRUE)
  
  return(mydata)
}