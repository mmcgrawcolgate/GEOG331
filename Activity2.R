#ACTIVITY 2 CODE

#reads data into variable datW
datW <- read.csv("noaa_weather//2011124.csv", stringsAsFactors = T)

#displays overall stats and figures from data
str(datW)

#formatting date column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#question 2 code

#check unique site names
unique(datW$NAME)

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

#for use in question 4 to make 2 by 2 window of histograms
par(mfrow=c(2,2))

#create histogram
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#question 3 code
help("hist")
help("colors")

#add mean line with red color
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#adding dashed sd line above and below mean 
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#question 4, creating 3 other histograms and adding colored lines
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#mean line
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "cadetblue",
       lwd = 3)

#two sd lines
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "cadetblue", 
       lty = 3,
       lwd = 3)

abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "cadetblue", 
       lty = 3,
       lwd = 3)
