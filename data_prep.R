library(openWAR)
library(dplyr)

setwd("GitHub/stat/")

data <- getData(start="2015-05-01", end="2015-05-02")
names(data)
unique(data$event)

###---PSEUDO CODE NOTES FOR ANDREW---###
library(openWAR)
library(ggplot2)
library(dplyr)

head(data$Event)
# set our date range for the data
data.start.date<-'2005-04-01'
data.end.date<-'2015-11-15'

#use openWar to get event data
data<-getData(start=data.start.date,end=data.end.date)

#--- Albert's data cleaning
#code Event colum for hits and outs 
data$event<-as.character(data$event)
data$Event <- with(data, ifelse(event=="Double" | event=="Home Run" |
                                  event=="Single" | event=="Triple", 
                                event, "Out"))
data$Event <- factor(data$Event, 
                     levels=c("Home Run", "Triple", "Double", "Single", "Out"),
                     ordered = TRUE)


write.csv(data,"alldata.csv",row.names=FALSE)
saveRDS(data, "alldata.RDS")

# data$stand <- ifelse(data$stand=="R", "Right-Handed Batter",
# "Left-Handed Batter")

head(data$event)

# spray plot


head(data$event)

data$event



