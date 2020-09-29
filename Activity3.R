#ACTIVITY 3 CODE
#by: Matt McGraw

#installing package for Activity
install.packages(c("lubridate"))
library(lubridate)

#creating assert function
assert <- function(statement,err.message){
  if(statement==FALSE){
    print(err.message)
  }
}

#testing function
assert(1==2, "err: unequal values")
assert(2==2, "err: unequal values")

###QUESTIONS 1 and 2

#no code

###QUESTION 3

#reading in data file needed
datW <- read.csv("bewkes//bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
print(datW[1,])

#reviewing data
str(datW)

#reading in data as sensor information
sensorInfo <- read.csv("bewkes//bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)

print(sensorInfo)
colnames(datW) <- colnames(sensorInfo)
print(datW[1,])

###DATA QA QC

#convert to standardized formatting
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

##Other Conversions:

#calculate day of year
datW$doy <- yday(dates)

#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)

#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#quick preview of new date calcualtions
datW[1,]

#calculate possible missing values
length(which(is.na(datW$air.temperature)))
length(which(is.na(datW$wind.speed)))
length(which(is.na(datW$precipitation)))
length(which(is.na(datW$soil.moisture)))
length(which(is.na(datW$soil.temp)))

#plot to see where data is missing
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

###QA QC TESTS

#visual plot of air temperature
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

###QUESTION 4 CODE

#new column to check for faulty data, freezing temps in the summer
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#checks quantiles for extreme data in the dataset
quantile(datW$air.tempQ1)

#finds days with very low air temperature
datW[datW$air.tempQ1 < 8,] 

#finds days with very high air temperature
datW[datW$air.tempQ1 > 33,] 

###QUESTION 5 CODE

#plot precipitation and lightning strikes on the same plot
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

#plot precipitation points only when there is precipitation 
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#test code using assert function, shows lightscale can exist
assert(length(lightscale)==length(datW$precipitation),"err: unequal lengths" )

###QUESTION 6 CODE

#air temperature column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#wind speed column
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#check to see if same length
assert(length(datW$air.tempQ2)==length(datW$wind.speedQ1), "err: unequal lengths")

#create plot of data, points and lines
help(plot)
plot(datW$DD, datW$wind.speedQ1, type="p", main="Wind Speed", xlab="Day of Year", 
     ylab="Wind Speed")

plot(datW$DD, datW$wind.speedQ1, type="l", main="Wind Speed", xlab="Day of Year", 
     ylab="Wind Speed")

###QUESTION 7 CODE


