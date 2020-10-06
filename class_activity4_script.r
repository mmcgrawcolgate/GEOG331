#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

x <- c(1,3,1)
y <- c(2,4,3)
for (i in 1:3){
  fit <- lm(iris[,x[i]] ~ iris[,y[i]])
  print(summary(fit))
}


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

iris_2 <- left_join(height,iris)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
plot <- ggplot(iris,aes(iris$Sepal.Length,iris$Sepal.Width))+
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
plot2 <- ggplot(iris,aes(iris$Sepal.Length,iris$Sepal.Width))+
  geom_point()+
  theme(panel.grid = element_blank())
  

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size

plot3 <- ggplot(iris,aes(Sepal.Length,Sepal.Width))+
  geom_point(aes(color=Species), size=5)+
  theme(panel.grid = element_blank())

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

#In plot, the arguments needed are the variables that you are looking to plot
#against each other as well as axis labels, the type of plot, and specifying
#colors and sizes all in a list. In ggplot, you must also confirm what data
#frame the data is coming from, actually put in an argument to plot the points,
#as well as changing the theme or 'type' of the plot in the third listed 
#argument above. Arguments in ggplot seem to have more room for alterations
#to the design and look of the plot than in just plot.