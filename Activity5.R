#ACTIVITY 5 CODE
#by: Matt McGraw


##READING IN DATA AND MAKING DATAFRAMES##
library(lubridate)

#read in streamflow data file
datH <- read.csv("stream_flow_data.csv",
                 na.strings = c("Eqp"))

#read in precip data file
datP <- read.csv("2049867.csv")                            

#create new dataframe using approved data
datD <- datH[datH$discharge.flag == "A",]

###QUESTION 1###
#no code

###QUESTION 2 CODE###

#defining time for streamflow
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#defining time for precipitation
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#decimal formats for datD
#convert time from a string to a more usable format with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))

#decimal formats for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 

###QUESTION 3 CODE###

#find lengths of both dataframes, check out how frequent data appears
length(datP$STATION)
length(datD$site_no)
head(datP, 15)
head(datD, 15)

###QUESTION 4 CODE###

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

help("expression")
help("paste")

###QUESTION 5 CODE###

#start to formatting plot
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#using 2017 as year to add line
d2017 <- datD[datD$year==2017,]
average2017 <- aggregate(d2017$discharge, by=list(d2017$doy), FUN="mean")
colnames(average2017) <- c("doy", "dailyAve")

#format of plot, extra line added
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)), #x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)), #ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA #no border
)  

#line added based on 2017 averages, made purple for contrast
lines(average2017$doy, average2017$dailyAve, col="purple")

axis(1, seq(1,365, by=31), #tick intervals
     lab = c("jan","feb","mar","apr","may","jun","jul","aug",
             "sep","oct","nov","dec")) #tick labels for month
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2) #show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017 mean"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"purple"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

###QUESTION 6 CODE###

#no code

###QUESTION 7 CODE###

library(dplyr)

#create code to adjust for days of precip with all 24 hours
total_hours <- datP %>% group_by(year, doy) %>%
  count()
fulldays <- total_hours[total_hours$n == 24,]
datP <- datP %>% mutate(doy_year = paste(doy, year, sep= "_"))
fulldays <- fulldays %>% mutate(doy_year = paste(doy, year, sep= "_"))
datP$complete <- ifelse(datP$doy_year %in% fulldays$doy_year, 1, 0)

#create plot with days with 24 hours added
par(mai=c(1,1,1,1))

plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,400),
     xaxs="i", yaxs ="i") 

#add in days with 24 hours of precipitation
for (i in 1:nrow(fulldays)){
  good <- datP[datP$complete == 1,]
  m <- i + 24*(i-1) + 12
  points(good[m,"decYear"], 350, pch=20, col="maroon", cex=0.7)
}
title("Yearly Discharge with Days with 24 Hours of Precip")

###QUESTION 8 CODE###

##FIRST HYDROGRAPH CODE##
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1

#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1

#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5

#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))

#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

###QUESTION 9 CODE###

#load in ggplot
library(ggplot2)

#mark days when seasons change
spring <- 60
summer <- 152
fall <- 244
winter <- 335

#define years as dataframes
datD2k16 <- datD[datD$year==2016,]
datD2k17 <- datD[datD$year==2017,]

#define seasons within years
datD2k16$seasons <- ifelse(datD2k16$decDay >= spring & datD2k16$decDay < summer, "Spring", 
                    ifelse(datD2k16$decDay >= summer & datD2k16$decDay < fall, "Summer",
                    ifelse(datD2k16$decDay >= fall & datD2k16$decDay < winter, "Fall",
                    "Winter")))

datD2k17$seasons <- ifelse(datD2k17$decDay >= spring & datD2k17$decDay < summer, "Spring", 
                    ifelse(datD2k17$decDay >= summer & datD2k17$decDay < fall, "Summer",
                    ifelse(datD2k17$decDay >= fall & datD2k17$decDay < winter, "Fall",
                    "Winter")))

#create violin plots
plot2k16 <- ggplot(data = datD2k16, aes(seasons, discharge)) +geom_violin()
plot2k17 <- ggplot(data = datD2k17, aes(seasons, discharge)) +geom_violin()

###QUESTION 10 CODE###

#no code, see doc
