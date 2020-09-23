#ACTIVITY 3 CODE
#by: Matt McGraw

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
