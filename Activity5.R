#ACTIVITY 5 CODE
#by: Matt McGraw

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

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

str(datP$STATION)
str(datD)

###QUESTION 4 CODE###

help("expression")

###QUESTION 5 CODE###

#start to formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#using 2017 as year to add line
d2017 <- datD[datD$year==2017,]
average2017 <- aggregate(d2017$discharge, by=list(d2017$doy), FUN="mean")
colnames(average2017) <- c("doy", "dailyAve")

#format of plot, no extra line added
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
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)  
lines(average2017$doy, average2017$dailyAve, col="purple") #add line based on 2017 averages
axis(1, seq(1,365, by=31), #tick intervals
     lab = c("jan","feb","mar","apr","may","jun","jul","aug",
             "sep","oct","nov","dec")) #tick labels for month
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017 mean"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"purple"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

###QUESTION 6 CODE###

#no code