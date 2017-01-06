library(openWAR)
library(dplyr)
data <- getData(start="2015-05-01", end="2015-05-02")
names(data)
unique(data$event)

###---PSEUDO CODE NOTES FOR ANDREW---###
# We want to read in data for several seasons starting in 2005.
# We want to re-code the event (Event)
# We want to associate each Event with a value
##    a. Bases
##    b. Run value
# We want to save this data as an RDS file.
# saveRDS(data, "filename.RDS")



