#ACTIVITY 2 CODE
#author: Matt McGraw

#reads data into variable datW
datW <- read.csv("noaa_weather//2011124.csv", stringsAsFactors = T)

###QUESTION 1 CODE
#displays overall stats and figures from data
str(datW)

#formatting date column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))

###QUESTION 2 CODE
#vector with character and integer values
vec1 <- c("word", 3, "word2", 7, "string")

#reference items in vector using numbers, or create new vector using factor
vec1[1]
vec1[2]
vec2 <- c("newword", vec1[1])
vec2[2]

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

###QUESTION 3 CODE
help("hist")
help("colors")

###QUESTION 4 CODE
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

        #livermore, ca
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

        #mean line and two sd lines
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "cadetblue",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "cadetblue", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "cadetblue", 
       lty = 3,
       lwd = 3)

        #mandan experiment station, nd
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

        #mean line and two sd lines
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "darkgoldenrod1",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "darkgoldenrod1", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "darkgoldenrod1", 
       lty = 3,
       lwd = 3)

        #mormon flat, az
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

        #mean line and two sd lines
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "blue2",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "blue2", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "blue2", 
       lty = 3,
       lwd = 3)

#create vector for histogram 1
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#plot range of values
x.plot <- seq(-10,30, length.out = 100)

#creates probability density
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#create density that is scaled to the plot itself
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#adds line to the graph
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#find p of normal dist for site 1
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#finds area of the code below 5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#finds probability of area from 0 to 5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#finds probability of area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#gives 95th percentile of the data, by temp
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#QUESTION 6 CODE

#creates a reference for the new mean of 4 degrees higher
newmean <- mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4

#finds 95th percentile
1-pnorm(newmean,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#QUESTION 7 CODE

#histogram of daily average precip in aberdeen
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipitation (cm)", 
     ylab="Relative frequency",
     col="grey50",
     border="black")

#QUESTION 8 CODE

#average daily precip across sites
averagePrecip <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averagePrecip

#yearly precip function
yearPrecip <- aggregate(datW$PRCP, by=list(datW$year, datW$siteN), FUN="sum", na.rm=TRUE)
yearPrecip

#creates a histogram for yearly precip at Livermore, CA
hist(yearPrecip$x[yearPrecip$Group.2==2],
        freq=FALSE, 
        main = paste(levels(datW$NAME)[2]),
        xlab = "Yearly Precipitation", 
        ylab = "Relative frequency",
        col = "grey50",
        border = "black")

###QUESTION 9 CODE

#finds yearly precip averages across sites
allprcp <- aggregate(yearPrecip, by=list(yearPrecip$Group.2), FUN="mean", na.rm=TRUE)
allprcp

###QUESTION 10 CODE
paste(levels(datW$NAME)[5])
#None, see attached sheet for GitHub Link