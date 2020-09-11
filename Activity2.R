#ACTIVITY 2 CODE

#reads data into variable datW
datW <- read.csv("noaa_weather//2011124.csv")

#displays overall stats and figures from data
str(datW)

#formatting date column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#question 2 code

#check unique site names
levels(datW$NAME)

#find mean of aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
