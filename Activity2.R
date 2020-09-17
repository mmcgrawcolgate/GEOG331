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

#find average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#find mean not only of aberdeen but across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change column names so they display name and mean annual air temp
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to numbers
datW$siteN <- as.numeric(datW$NAME)

#create histogram
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
