#http://www.biecek.pl/R/#Pogromcy

#Installation

# install.packages('plyr')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('e1071')
# install.packages("randomForest")
# install.packages('caret')
#Open data


#LinkRef -A unique alphanumeric link id representing a junction to junction link on the Highway’s Agency managed road network.
#LinkDescription - Description of the link.
#Date - Date of travel.
#TimePeriod - One of 96 15-minute intervals in the day that the data refers to (0-95 where 0 indicates 00:00 to 00:15). 
#AverageJT -The average journey time to travel across the ‘LinkRef’ in seconds, of vehicles entering the junction to junction link within a given 15-minute time period.
#AverageSpeed - The average speed (in km/h) of vehicles entering the junction to junction link within a given 15-minute time period.
#DataQuality -Indicator showing the quality of the journey time data for the link and time period. 1 indicates the highest quality data and 5 the lowest. See below for detailed description:
#LinkLength -The length of the link (km).
#Flow - An average of the observed flow for the link, time period and day type. 


path ='/Users/aleksandra/Projects/R/Lab_01/';
source(paste(path,"readTraffic.R",sep=''))
source(paste(path,"findClusters.R",sep=''))
source(paste(path,"CreateTree.R",sep=''))
source(paste(path,"CreateRandomForest.R",sep=''))

#Data sources
#http://data.highways.gov.uk/Journey-time-data/FEB15.zip
#http://data.highways.gov.uk/Journey-time-data/MAR15.zip
#Change isPL to 0 to use English names of the days of week
Sys.setlocale("LC_TIME", "en_US")
isPL =0;
probe = readTrafficData(path,'FEB15.csv',PL=isPL)



#Selection


#http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
library(plyr)
library(dplyr)




#Statistics

statistics <- probe %>% 
  group_by(DayOfWeek)%>%
  summarise(avg_speed = mean(AverageSpeed), 
            min_speed = min(AverageSpeed),
            max_speed = max(AverageSpeed),
            total = n())

#Plot
library(ggplot2)

#http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

ggplot(probe, aes(DayOfWeek,AverageSpeed))+ geom_boxplot()
ggplot(probe, aes(AverageSpeed,fill=DayOfWeek))+geom_density(alpha=.3)
ggplot(probe, aes(Hour,AverageSpeed))+ geom_boxplot(aes(group = cut_width(probe$Hour, 1)))

dow<-unique(probe$DayOfWeek)
for(i in 1:length(dow))
{
  print(ggplot(subset(probe,DayOfWeek==dow[i]), aes(Hour,AverageSpeed))+ geom_boxplot(aes(group = cut_width(Hour, 1)))+ labs(title = dow[i]))
}



#clusterisation - find various types of
# install.packages('e1071')
library('e1071')
speeds = c('super_slow', 'low','medium','high', 'super_max')
# speeds = c('low','medium','high')

iterations <-1000
probe$class <-findClusters(speeds, iterations, probe, 'AverageSpeed')

ggplot(probe, aes(AverageSpeed,fill=(class)))+geom_density()
ggplot(probe, aes(class,AverageSpeed))+ geom_boxplot()


statistics <- probe %>% 
  group_by(class)%>%
  summarise(avg_speed = mean(AverageSpeed), 
            min_speed = min(AverageSpeed),
            max_speed = max(AverageSpeed),
            total = n())


#classification
library(rpart)
varNames <- c('DayOfWeek','Flow','TimePeriod');
part_probe<-probe %>%
  filter(LinkRef=='AL2031')


probe.tree <-createTree(varNames,'class','class',part_probe)

probe.tree.predictions <- predict(probe.tree ,part_probe, type = "class")
library(caret)
confusionMatrix(probe.tree.predictions, part_probe$class)

#testing set
#Second probe
probe2 = readTrafficData(path,'MAR15.csv',PL=isPL)

for(i in 1:length(speeds))
{
 if(i==1)
 {
   probe2$class[probe2$AverageSpeed<=statistics$max_speed[i]] = speeds[i]
 }
  else
    if(i==length(speeds))
  {
    probe2$class[ probe2$AverageSpeed>=statistics$min_speed[i]] = speeds[i]
    }
  else
  {
    probe2$class[probe2$AverageSpeed<=statistics$max_speed[i] & probe2$AverageSpeed>=statistics$min_speed[i]] = speeds[i]
  }
}

probe2$class = factor(probe2$class,levels=speeds,ordered=TRUE)
confusionMatrix(probe.tree.predictions, part_probe$class)

#tests on Tree
part_probe2<-probe2 %>%
  filter(LinkRef=='AL2031')
#filter(LinkLength>20)
probe.tree.predictions <- predict(probe.tree ,part_probe2, type = "class")

confusionMatrix(probe.tree.predictions, part_probe2$class)

#Random Forest
 # install.packages("randomForest")
 library(randomForest)
 
 varNames <- c('DayOfWeek','TimePeriod','LinkLength','AverageJT','Flow');
 
 probe.rf <-createRandomForest(varNames,'class',30,probe)
 
 probe.predictions <- predict(probe.rf ,probe)
 # install.packages('caret')

 confusionMatrix(probe.predictions, probe$class)
 
 probe2.predictions <- predict(probe.rf ,probe2)
 confusionMatrix(probe2.predictions, probe2$class)
 
 #regression
 reg_probe1<-probe %>%
   filter(LinkRef=='AL2031')
 
 reg_probe2<-probe2 %>%
   filter(LinkRef=='AL2031')
 
 probe.rf <-createRandomForest(c('DayOfWeek','TimePeriod'),'AverageSpeed',10,reg_probe1)
 probe.predictions <- predict(probe.rf ,reg_probe1)
 errors = abs(probe.predictions-reg_probe1$AverageSpeed);
 
 probe2.predictions <- predict(probe.rf ,reg_probe2)
 errors2 = abs(probe2.predictions-reg_probe2$AverageSpeed);
 

 
 df <- data.frame(
   x = c(errors, errors2),
   probe = c(rep('learning', length(errors)), rep('testing', length(errors2))) 
 )
 ggplot(df, aes(x, colour = probe)) + stat_ecdf()

 
 
