#ACTIVITY 6 CODE
#by: Matt McGraw

#install packages
install.packages(c("raster","sp","rgdal","rgeos","plyr"))

#load in packages
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in glacier data files
g1966 <- readOGR("data//GNPglaciers//GNPglaciers_1966.shp", stringsAsFactors = T)
g1998 <- readOGR("data//GNPglaciers//GNPglaciers_1998.shp", stringsAsFactors = T)
g2005 <- readOGR("data//GNPglaciers//GNPglaciers_2005.shp", stringsAsFactors = T)
g2015 <- readOGR("data//GNPglaciers//GNPglaciers_2015.shp", stringsAsFactors = T)

###QUESTION 1 CODE###

#look at type of projection
g1966@proj4string

###QUESTION 2 CODE###

spplot(g1966, "GLACNAME")

#adjust glacier names to match across time periods
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#reading in rgb files
redL <- raster("data//glacier_09_05_14//l08_red.tif")
greenL <- raster("data//glacier_09_05_14//l08_green.tif")
blueL <- raster("data//glacier_09_05_14//l08_blue.tif")

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("data//NDVI//NDVI_",ndviYear[i],".tif"))
}

###QUESTION 3 CODE###

par(mfrow=c(1, 2))
#plot the 1966 glacier
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
#this plots 2013
plot(NDVIraster[[1]])

###QUESTION 4 CODE###

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

vector_maxes<-c(0.9999395,0.999779,0.999081,0.9999021,0.9999728,0.9997418,0.9999769,
             0.9994358,0.9998951,0.9999688,0.9997269,0.999855,0.9997797,
             0.9997877)

max(vector_maxes)

#after checking the max of the vectors, 2010 has the maximum NDVI 
par(new=TRUE)
plot(NDVIraster[[8]],axes=FALSE, frame.plot=TRUE)

# plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g2015p, axes=FALSE,bg=NA, border='black')


###QUESTION 5 CODE###

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

spplot(g1966p, "GLACNAME")
spplot(g2015p, "GLACNAME")

gAll$a1966m.sq[1]
gAll$a2015m.sq[1]

total_percent_change <-(((sum(gAll$a1966m.sq)-sum(gAll$a2015m.sq))
                  /sum(gAll$a1966m.sq)))*100

#calculate percent change in area and create a dataframe for area
gAll$PercentChangefrom2015to1966 <- (((gAll$a1966m.sq - gAll$a2015m.sq)
                                      /gAll$a1966m.sq)*100)

#create new dataframe
gArea <- data.frame(GLACNAME=gAll$GLACNAME, 
                    areaDiff = gAll$PercentChangefrom2015to1966)

#join percent change data to the 2015 glacier data
g2015p@data <- join(g2015p@data, gArea, by="GLACNAME", type="left")
spplot(g2015p, "areaDiff", ylab="% Change in Area", axes=TRUE)

###QUESTION 6 CODE###

diffPoly <- gDifference(g1966p, g2015p, checkValidity = 2L)
plot(diffPoly)

plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#check to see which glacier has the largest % loss
gAll$PercentChangefrom2015to1966
gAll$GLACNAME[5]

#create var for boulder glacier
boulder_glacier<-g2015p[g2015p$GLACNAME=="Boulder Glacier",]

#make graph for boulder glacier
spplot(boulder_glacier, "areaDiff",xlab="Boulder Glacier", axes=TRUE)


###QUESTION 7 CODE###

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

###QUESTION 8 CODE###

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize glaciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

###QUESTION 9 CODE###

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

g2015p@data$meanChange <- meanChange[2:40, "mean"]
spplot(g2015, "meanChange")

###QUESTION 10 CODE###

#no code

###QUESTION 11 CODE###



###QUESTION 12 CODE###

#no code

###QUESTION 13 CODE###

#no code
