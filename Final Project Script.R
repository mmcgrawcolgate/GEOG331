###GEOG331 Final Project Script###
#by: Matt McGraw

###############################################################################

#read in for data management
library(lubridate)

#read in data
datAllData <- read.csv("snow_and_temp_data.csv", header=TRUE, sep=',', 
                       blank.lines.skip = TRUE)
str(datAllData)
unique(datAllData$NAME)

mean(datAllData$SNOW, na.rm = TRUE)

#begin to sort out vars for time
datDates <- as.Date(datAllData$DATE, "%Y-%m-%d")
datAllData$DOY <- yday(datDates)
datAllData$YEAR <- year(datDates)
datAllData$MONTH <- month(datDates)

unique(datAllData$YEAR)
unique(datAllData$MONTH)

#plot out snow against year to see highest days of snow
plot(datAllData$YEAR, datAllData$SNOW, 
     type = "p", 
     xlab="Year", 
     ylab = "Daily Snowfall (in)")
title("Variation of Days of Snowfall Across Stations (2005-2020)")

#plot out snow against month to find months with highest snowfall
plot(datAllData$MONTH, datAllData$SNOW,
     type = "p",
     xlab="Month",
     ylab="Daily Snowfall Measures (in)")
axis(1, seq(1,12, by=1))
title("Variation of Days of Snowfall Across Stations (2005-2020, by month)")

#water year formatting
datAllData$WATERMONTH <- ifelse(datAllData$MONTH > 9, 
                               datAllData$MONTH-9, 
                               datAllData$MONTH+3)

datAllData$WATERYEAR <- ifelse(datAllData$MONTH > 9,
                               datAllData$YEAR +1, 
                               datAllData$YEAR)

unique(datAllData$WATERYEAR)
unique(datAllData$WATERMONTH)

#start aggregation process by year to sum snowfall each winter and by month
sumSnowYear <- aggregate(datAllData$SNOW, by=list(datAllData$WATERYEAR), FUN='sum', na.rm=TRUE)
colnames(sumSnowYear) <- c("Water Year", "Sum of Snowfall")

str(sumSnowYear)

meanTempYear <- aggregate(datAllData$TAVG, by=list(datAllData$WATERYEAR), FUN='mean', na.rm=TRUE)
colnames(meanTempYear) <- c("Water Year", "Average Yearly Temp")

str(meanTempYear)

#start aggregation process by month
sumSnowMonth <- aggregate(datAllData$SNOW, by=list(datAllData$WATERMONTH), FUN='sum', na.rm=TRUE)
colnames(sumSnowMonth) <- c("Water Month", "Sum of Snowfall")

meanSnowMonth <- aggregate(datAllData$SNOW, by=list(datAllData$WATERMONTH), FUN='mean', na.rm=TRUE)
colnames(meanSnowMonth) <- c("Water Month", "Mean Snowfall")
str(sumSnowMonth)

meanTempMonth <- aggregate(datAllData$TAVG, by=list(datAllData$WATERMONTH), FUN='mean', na.rm=TRUE)
colnames(meanTempMonth) <- c("Water Month", "Average Monthly Temp")

str(meanTempMonth)

#aggregating plots of snow and temp by month and year
par(mai=c(1,1,1,1))

plot(sumSnowMonth$`Water Month`, sumSnowMonth$`Sum of Snowfall`,
     type="h",
     xlab="Water Month",
     ylab="Sum of Snowfall (in)",
     lwd=4,
     col="blue",
     axes=FALSE)
axis(1, seq(1,12, by=1), lab=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
                               "May", "Jun", "Jul", "Aug", "Sep"))
axis(2, seq(0,14000, by=2000), lab=c("0", "2k", "4k", "6k", "8k", "10k", "12k", "14k"))
title(main="Snowfall by Water Month Across Stations")

plot(meanTempMonth$`Water Month`, meanTempMonth$`Average Monthly Temp`,
     type='h',
     xlab="Water Month",
     ylab="Average Monthly Temp (F)",
     lwd=4,
     ylim=c(20,80),
     col="maroon",
     axes=FALSE)
axis(1, seq(1,12, by=1), lab=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
                               "May", "Jun", "Jul", "Aug", "Sep"))
axis(2, seq(20,80, by=10))
title(main="Average Temp by Water Month")

plot(sumSnowYear$`Water Year`, sumSnowYear$`Sum of Snowfall`,
     type='h', 
     xlab="Water Year",
     ylab="Yearly Snowfall Totals (in)",
     lwd=4,
     ylim=c(0,4000),
     col="light blue",
     axes=TRUE)
title(main="Sum of Snowfall by Water Year")

plot(meanTempYear$`Water Year`, meanTempYear$`Average Yearly Temp`,
     type='h', 
     xlab="Water Year",
     ylab="Average Yearly Temp (F)",
     lwd=4,
     xlim=c(2005, 2019),
     col="pink",
     axes=TRUE)
title(main="Average Yearly Temp by Water Year (discluding 2020)")

#plot yearly sum snowfall against yearly average temp
plot(meanTempYear$`Average Yearly Temp`, sumSnowYear$`Sum of Snowfall`,
     type='p',
     xlab="Average Yearly Temp (F)",
     ylab="Yearly Sum of Snowfall (in)",
     col="black",
     axes=TRUE)
title(main="Yearly Sum of Snowfall vs Average Yearly Temp")

#plot monthly sum snowfall against monthly average temp
plot(meanTempMonth$`Average Monthly Temp`, sumSnowMonth$`Sum of Snowfall`,
     type="p",
     xlab="Average Monthly Temp (F)",
     ylab="Monthly Sum of Snowfall (in)",
     axes=TRUE)
title(main="Monthly Sum of Snowfall vs Average Monthly Temp")

#start to focus on winters as months of study
datAllData$SEASON <- ifelse(datAllData$WATERMONTH>2 & datAllData$WATERMONTH<7,
                                  "W", "S")

seasonTemp <- aggregate(datAllData$TAVG, by=list(datAllData$SEASON, datAllData$WATERYEAR), 
                        FUN='mean', na.rm=TRUE)
seasonSnow <- aggregate(datAllData$SNOW, by=list(datAllData$SEASON, datAllData$WATERYEAR), 
                        FUN='sum', na.rm=TRUE)

colnames(seasonTemp) <- c("Season", "Year", "Temp")
colnames(seasonSnow) <- c("Season", "Year", "Snowfall")

winterTemp <- subset(seasonTemp, seasonTemp$Season=="W")
winterSnow <- subset(seasonSnow, seasonSnow$Season=="W")

#create plots for winter and snow and temps for each
plot(winterTemp$Temp, winterSnow$Snowfall,
     type='p',
     xlab="Average Winter Temp (F)",
     ylab="Winter Sum of Snowfall (in)",
     axes=TRUE,
     col="blue")
title(main="Winter Sum of Snowfall vs Average Winter Temp")

plot(winterTemp$Year, winterTemp$Temp,
     type="h",
     lwd=4,
     xlab="Year",
     ylab="Average Winter Temp (F)",
     axes=TRUE)
title(main="Average Winter Temp (F) by Year")

plot(winterSnow$Year, winterSnow$Snowfall,
     type="h",
     lwd=4,
     xlab="Year",
     ylab="Winter Sum of Snowfall (in)",
     col="light blue")
title(main="Average Winter Snowfall (in) by Year")

#create subset for Oneida, affected by lake effect snowfall
Oneida <- subset(datAllData, datAllData$NAME == "ONEIDA 0.4 NW, NY US")
OneidaSnow <- aggregate(Oneida$SNOW, by=list(Oneida$WATERYEAR), FUN='sum', na.rm=TRUE)
OneidaTemp <- aggregate(Oneida$TAVG, by=list(Oneida$WATERYEAR), FUN='mean', na.rm=TRUE)

plot(OneidaSnow$Group.1, OneidaSnow$x,
     type='p',
     xlab="Year",
     ylab="Sum of Snowfall (in)",
     xlim=c(2014,2020))
title(main="Variation of Winter Snowfall in Oneida")

plot(OneidaTemp$Group.1, OneidaTemp$x,
     type='p',
     xlab="Year",
     ylab="Average Temperature (F)",
     xlim=c(2008, 2019),
     ylim=c(20, 40))
title(main="Variation of Winter Temperatures in Oneida")
