###---------------------------------------------------------###
###---------------------------------------------------------###
###       CODE TO CALCULATE ANDREW'S BASEBALL STAT
###---------------------------------------------------------###
###---------------------------------------------------------###

library(dplyr)
setwd("~/GitHub/stat/")
source("stat_lib.R")


# Read in data
data<-readRDS("alldata.RDS")
dim(data) # only 170K events. That seems a little light
tail(data$timestamp)


# Set what region each BIP lands in
data$region <- apply(data[,c("our.x", "our.y")], 1, FUN=function(x) ball_in_play(x[1], x[2]))

# check that we have observations for the RSOI
dataR1 <- data %>% filter(region== "RIGHT SIDE OF INFIELD")
dim(dataR1)


###-------------------------------------------------------------###
### Figure out BIP outcome
###-------------------------------------------------------------###

# base_value("Single")
# base_value("Double")

# data_LEFT_SIDE_OF_INFIELD<-data%>%filter(region== "LEFT SIDE OF INFIELD")
# LEFT_SIDE_OF_INFIELD<-mean(data_LEFT_SIDE_OF_INFIELD$base_value)

# create colum that will use the base value function to find the mean
data$base_value<- sapply(data$Event, FUN=function(x) base_value(x))
summary(data$base_value)
library(ggplot2)
qplot(data$base_value)

###-------------------------------------------------------------###
### Get average weights per region from data
###-------------------------------------------------------------###

# Get the average SLG weights per region
weights_slg<-data %>% group_by(region) %>% summarize(m=mean(base_value))
weights_slg

###-------------------------------------------------------------###
### Functions for location-adjusted offense
###-------------------------------------------------------------###

# Function for location-adjusted slugging
loc_slg <- function(X){
  # Takes:X (vector of numbers) of hits to each region 
  # Returns: location-adjusted average (numeric)
  
  # avg base value f LEFT SIDE OF INFIELD, LEFT SIDE OF OUTFIELD, NOT IN INFIELD,  RIGHT SIDE OF INFIELD, NOT IN INFIELD  
  b <- c(0.10524700, 0.90796180, 0.23114447, 0.05024712, 0.82243020)
  return(b %*% X / sum(X))
}

