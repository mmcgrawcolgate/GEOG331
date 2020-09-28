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

#reading in data as sensor information
sensorInfo <- read.csv("bewkes//bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)

print(sensorInfo)
colnames(datW) <- colnames(sensorInfo)
print(datW[1,])

###QUESTION 4 (QA/QC)

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
