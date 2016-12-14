#-************************* Impressive Impalas *************************-#

#-************************* Question-1 *************************-#
#-How has the tornado occurrence varied over the last 10 years?
#Variable under consideration: Count of the tornadoes each year

#Importing the ggplot2 library
library(ggplot2)

#-Read the csv file by manually browsing through the file structure
#-Read he state-wise data for tornadoes and exports in d
#-Read he region-wise data for tornadoes and exports in x
d=read.csv("C:\\Users\\kavas\\Desktop\\Documents\\Courses\\600_AW\\Project\\Rplot\\Tor_Exp_StateWise_RPlot.csv")
x=read.csv("C:\\Users\\kavas\\Desktop\\Documents\\Courses\\600_AW\\Project\\Rplot\\Tor_Exp_RegionWise_RPlot.csv")

#-We divide the data set using the subset command to analyze 
#--different geographic regions in the United States
#--namely Midwest, Northeast, Southeast and West
d.MW=subset(d,d$Region=="Mid-West")
d.NE=subset(d,d$Region=="North-East")
d.SE=subset(d,d$Region=="South-East")
d.W=subset(d,d$Region=="Western")

x.MW=subset(x,x$Region=="Mid-West")
x.NE=subset(x,x$Region=="North-East")
x.SE=subset(x,x$Region=="South-East")
x.W=subset(x,x$Region=="Western")

#Plot for the number of tornadoes in United-States
ggplot(d,aes(x=d$Year,y=d$Count))+
  geom_bar(stat="identity", fill="indianred")+
  xlab("Year")+ylab("Number of Tornadoes")+
  ggtitle("United States of America: Number of Tornadoes")

#Plot for the number of tornadoes in North-East
ggplot(d.NE,aes(x=d.NE$Year,y=d.NE$Count))+
  geom_bar(stat="identity", fill="darkcyan")+
  xlab("Year")+ylab("Number of Tornadoes")+
  ggtitle("Northeast: Number of Tornadoes")

#Plot for the number of tornadoes in South-East
ggplot(d.SE,aes(x=d.SE$Year,y=d.SE$Count))+
  geom_bar(stat="identity", fill="goldenrod1")+
  xlab("Year")+ylab("Number of Tornadoes")+
  ggtitle("Southeast: Number of Tornadoes")

#Plot for the number of tornadoes in Mid-West
ggplot(d.MW,aes(x=d.MW$Year,y=d.MW$Count))+
  geom_bar(stat="identity", fill="hotpink3")+
  xlab("Year")+ylab("Number of Tornadoes")+
  ggtitle("Midwest: Number of Tornadoes")

#Plot for the number of tornadoes in West
ggplot(d.W,aes(x=d.W$Year,y=d.W$Count))+
  geom_bar(stat="identity", fill="forestgreen")+
  xlab("Year")+ylab("Number of Tornadoes")+
  ggtitle("West: Number of Tornadoes")

#-************************* Question-2 *************************-#
#-What is the relationship between tornado occurrences and Exports
#-Variable under consideration:
#-Dependent Variable: Total exports from a state/region
#-Independent Variable: Average F-scale value, Count of tornadoes

#Analyzing the data using linear regression model for each region
summary(lm(x.MW$Sum.of.Agricultural.Exports~x.MW$Sum.of.Count))
summary(lm(x.MW$Sum.of.Animal.Products~x.NE$Sum.of.Count))
summary(lm(x.MW$Sum.of.Plant.Products~x.NE$Sum.of.Count))

summary(lm(x.NE$Sum.of.Agricultural.Exports~x.NE$Sum.of.Count))
summary(lm(x.NE$Sum.of.Animal.Products~x.NE$Sum.of.Count))
summary(lm(x.NE$Sum.of.Plant.Products~x.NE$Sum.of.Count))

summary(lm(x.SE$Sum.of.Agricultural.Exports~x.SE$Sum.of.Count))
summary(lm(x.SE$Sum.of.Animal.Products~x.SE$Sum.of.Count))
summary(lm(x.SE$Sum.of.Plant.Products~x.SE$Sum.of.Count))


summary(lm(x.W$Sum.of.Agricultural.Exports~x.W$Sum.of.Count))
summary(lm(x.W$Sum.of.Animal.Products~x.W$Sum.of.Count))
summary(lm(x.W$Sum.of.Plant.Products~x.W$Sum.of.Count))

#-We divide the data set using the subset command to analyze 
#--different states in the United States

d.Texas=subset(d,d$State=="TX")
d.NeMex=subset(d,d$State=="NM")
d.Ok=subset(d,d$State=="OK")
d.Ar=subset(d,d$State=="AR")
d.La=subset(d,d$State=="LA")
d.Az=subset(d,d$State=="AZ")
d.Ks=subset(d,d$State=="KS")

#Analyzing the data using multiple regression model for states 
#--with high occurrence of tornadoes
summary(lm(d.Texas$Agricultural.Exports~d.Texas$Count+d.Texas$Average.of.F.Scale))
summary(lm(d.Texas$Animal.Products~d.Texas$Count+d.Texas$Average.of.F.Scale))
summary(lm(d.Texas$Plant.Products~d.Texas$Count+d.Texas$Average.of.F.Scale))

summary(lm(d.NeMex$Agricultural.Exports~d.NeMex$Count+d.NeMex$Average.of.F.Scale))

summary(lm(d.Ok$Agricultural.Exports~d.Ok$Count+d.Ok$Average.of.F.Scale))

summary(lm(d.Ar$Agricultural.Exports~d.Ar$Count+d.Ar$Average.of.F.Scale))

summary(lm(d.La$Agricultural.Exports~d.La$Count+d.La$Average.of.F.Scale))

summary(lm(d.Az$Agricultural.Exports~d.Az$Count+d.Az$Average.of.F.Scale))

summary(lm(d.Ks$Agricultural.Exports~d.Ks$Count+d.Ks$Average.of.F.Scale))

#-We plotted the count of tornadoes vs the year for the state of Texas
ggplot(d.Texas,aes(d.Texas$Year,d.Texas$Count))+
  geom_point() + geom_line(colour="blue")+
  xlab("Year") + ylab("Count of Tornadoes") + ggtitle("Number of Tornadoes: Texas")

#-We plotted the exports vs the year for the state of Texas
ggplot(d.Texas, aes(d.Texas$Year)) +
  geom_line(aes(y = d.Texas$Agricultural.Exports, colour = "Agricultural Products")) +
  geom_point(aes(y = d.Texas$Agricultural.Exports, colour = "Agricultural Products")) +
  geom_line(aes(y = d.Texas$Plant.Products, colour = "Plant Products")) +
  geom_point(aes(y = d.Texas$Plant.Products, colour = "Plant Products")) +
  geom_line(aes(y = d.Texas$Animal.Products, colour = "Animal Products")) +
  geom_point(aes(y = d.Texas$Animal.Products, colour = "Animal Products")) +
  xlab("Year") + ylab("State-wise Exports") + ggtitle("Exports: Texas") + labs(color='EXPORTS')

#--We plot the agricultural products and the 
#--magnified version of number of tornadoes 
#--to observe a pattern and deduce a conclusion for the state of Texas
ggplot(d.Texas, aes(d.Texas$Year)) + 
  geom_line(aes(y = d.Texas$Count*30, colour = "Count")) +
  geom_point(aes(y = d.Texas$Count*30, colour = "Count")) +
  geom_line(aes(y = d.Texas$Agricultural.Exports, colour = "Agricultural Products")) +
  geom_point(aes(y = d.Texas$Agricultural.Exports, colour = "Agricultural Products")) +
  xlab("Year") + ylab("Agricultural Products") + ggtitle("Exports vs #Tornadoes: Texas") + labs(color='EXPORTS');

#--We plot the Plant products and the 
#--magnified version of number of tornadoes 
#--to observe a pattern and deduce a conclusion for the state of Texas
ggplot(d.Texas, aes(d.Texas$Year)) + 
  geom_line(aes(y = d.Texas$Count*30, colour = "Count")) +
  geom_point(aes(y = d.Texas$Count*30, colour = "Count")) +
  geom_line(aes(y = d.Texas$Plant.Products, colour = "Plant Products")) +
  geom_point(aes(y = d.Texas$Plant.Products, colour = "Plant Products")) +
  xlab("Year") + ylab("Plant Products") + ggtitle("Exports vs #Tornadoes: Texas") + labs(color='EXPORTS');

#--We plot the Animal products and the 
#--magnified version of number of tornadoes 
#--to observe a pattern and deduce a conclusion for the state of Texas
ggplot(d.Texas, aes(d.Texas$Year)) + 
  geom_line(aes(y = d.Texas$Count*10, colour = "Count")) +
  geom_point(aes(y = d.Texas$Count*10, colour = "Count")) +
  geom_line(aes(y = d.Texas$Animal.Products, colour = "Animal Products")) +
  geom_point(aes(y = d.Texas$Animal.Products, colour = "Animal Products")) +
  xlab("Year") + ylab("Animal Products") + ggtitle("Exports: Texas") + labs(color='EXPORTS')
