#-************************* Impressive Impalas *************************-#

#-************************* Question-1 *************************-#
#-How has the tornado occurrence varied over the last 10 years?
#Variable under consideration: Count of the tornadoes each year

#-Read the csv file by manually browsing through the file structure
#-Read the entire tornadoes data set in d1.
d1=read.csv(file.choose())

#-Verify the loaded file by printing the first 6 rows using head()
head(d1)

#Descriptive statistics

#-Plotting the histogram to check the trend for the number of 
#--tornadoes over a period of 10 years.
hist(d1$Year)

#-The distribution is skewed and so the measure of the central 
#--tendencies is the median
median(d1$Year)
#-The standard deviation is calculated using sd(variable)
sd(d1$Year)

#-We divide the data set using the subset command to analyze 
#--different geographic regions in the United States
#--namely Midwest, Northeast, Southeast and West
d1.MW=subset(d1,d1$Region=="Midwest")
d1.NE=subset(d1,d1$Region=="Northeast")
d1.SE=subset(d1,d1$Region=="Southeast")
d1.W=subset(d1,d1$Region=="West")

#-------------------- Midwest region --------------------
#-Plotting the histogram to check the trend for the number of 
#--tornadoes over a period of 10 years for the Midwest region.
hist(d1.MW$Year)

#-The distribution is skewed and so the measure of the central 
#--tendencies is the median
median(d1.MW$Year)
sd(d1.MW$Year)

#-------------------- Northeast region --------------------
#-Plotting the histogram to check the trend for the number of 
#--tornadoes over a period of 10 years for the Northeast region.
hist(d1.NE$Year)

#-The distribution is skewed and so the measure of the central 
#--tendencies is the median
median(d1.NE$Year)
sd(d1.NE$Year)

#--------------------- Southeast region --------------------
#-Plotting the histogram to check the trend for the number of 
#--tornadoes over a period of 10 years for the Southeast region.
hist(d1.SE$Year)

#-The distribution is skewed and so the measure of the central 
#--tendencies is the median
median(d1.SE$Year)
sd(d1.SE$Year)

#-------------------- West Region -------------------- 
#-Plotting the histogram to check the trend for the number of 
#--tornadoes over a period of 10 years.
hist(d1.W$Year)

#-The distribution is skewed and so the measure of the central 
#--tendencies is the median
median(d1.W$Year)
sd(d1.W$Year)

#-************************* Question-2 *************************-#
#-What is the relationship between tornado occurrences and 
#-Variable under consideration:
#-Dependent Variable: Total exports from a state
#-Independent Variable: Average F-scale value, Count of tornadoes

#-Read the aggregated data set consisting of annual average intensity 
#--and export data in d2
d2=read.csv(file.choose())

#Verify the loaded file by printing the first 6 rows using head()
head(d2)

#-We divide the data set using the subset command to analyze 
#--data for the state of Texas
d2.TX=subset(d2,d2$State=="TX")

#-Descriptive statistics
#-Plotting the histograms for the Average F-scale, Count and total 
#--agricultural exports from 2000-2014
hist(d2.TX$Average.of.F.Scale)

#-The distribution is normal and so the measure of the central 
#--tendencies is the mean
mean(d2.TX$Average.of.F.Scale)
sd(d2.TX$Average.of.F.Scale)

hist(d2.TX$Count)
#-The distribution is normal and so the measure of the central 
#--tendencies is the mean
mean(d2.TX$Count)
sd(d2.TX$Count)

hist(d2.TX$Total.agricultural.exports)
#-The distribution is skewed and so the measure of the central 
#--tendencies is the median
median(d2.TX$Total.agricultural.exports)
sd(d2.TX$Total.agricultural.exports)

#-We plotted the F-scale, Count and Agricultural export vs the Year to
#--observe a pattern and decide the stastical method for analysis.
plot(d2.TX$Year,d2.TX$Average.of.F.Scale,type = "o")
plot(d2.TX$Year,d2.TX$Count,type = "o")
plot(d2.TX$Year,d2.TX$Total.agricultural.exports,type="o")
