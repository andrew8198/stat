###---------------------------------------------------------###
###---------------------------------------------------------###
###      FUNCTION LIBRARY FOR ANDREW'S BASEBALL STAT
###---------------------------------------------------------###
###---------------------------------------------------------###

# Libraries
library(openWAR)
library(dplyr)

y_intercept <- function(x1, y1, x2, y2){
  # Takes four numbers corresponding to two coordinate pairs (x1, y1) & (x2, y2)
  # Returns the y-intercept of the line running through this pair of points.
  
  m = slope(x1, y1, x2, y2)
  return(y1 - m*x1)
}

slope <- function(x1, y1, x2, y2){
  # Takes four numbers corresponding to two coordinate pairs (x1, y1) & (x2, y2)
  # Returns slope of the line running through this pair of points.
  
  return((y2 - y1)/(x2 - x1))
}

# Determine what region a ball in play lands in
ball_in_play<-function(x,y) {
  # Takes: x, y (numeric) coordinates of BIP
  # Returns: region (character) that BIP lands in
  
  if(is.na(x) | is.na(y)){
    return(NA)
    break
  }
  if((slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)>=0-90
     & ( (slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)<=0+90) & 
     (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5)>=60.5-90 & 
     (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5) <=60.5+90 |
     (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))<= 95)& abs(y/x)>=1 & x<0)
    
  {return("LEFT SIDE OF INFIELD")}
  else if((slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)>=0-90
          & ( (slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)<=0+90) & 
          (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5)>=60.5-90 & 
          (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5) <=60.5+90 |
          (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))<= 95)& abs(y/x)>=1 & (x>0))
  {return("RIGHT SIDE OF INFIELD")}
  else if (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))>= 95 &abs(y)>=x &x>=0)
  {return("RIGHT SIDE OF OUTFIELD")}
  else if (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))>= 95 &abs(y)>=x &x<=0)
  {return("LEFT SIDE OF OUTFIELD")}
  else {
    return("NOT IN INFIELD")
  }
}

inRegionSQ <- function(x, y, region){
  # Takes: x, y coordinates of ball in play
  #         region=list(c(x1, x2), ..., c(x4, y4)) the coordinates of the vertices
  # Returns: TRUE if (x, y) in region
  
  inRange = function(x, list){
    return(x >= min(list) & x <= max(list))
  }
  xcoords = sapply(region, function(x) x[1])
  ycoords = sapply(region, function(x) x[2])
  if(inRange(x, xcoords) & inRange(y, ycoords)){
    return(TRUE)
  } else { 
    return(FALSE)
  }
}

RegionSQ = function(x, y, grid){
  # grid is a NAMED list of regions
  # each region is a list of vertices as above
  # return the name of the region (x, y) is in.
}

# Function to get average weights per region
base_value <- function(Event){
  # Takes:
  # Returns: 
  
  if(Event == "Single"){
    return(1)
  } else if(Event == "Double") {
    return(2)
  } else if(Event == "Triple") {
    return(3)
  } else if(Event == "Home Run") {
    return(4)
  } else {
    return(0)
  }
}

# wts = list(1.409, 1.063, 0.764, 0.474, 0.546, 0.385, 0.33, 0.102, -0.001, -0.299, -0.31, 0.285, 0.284, 0.237, 0.195, -0.456, -0.255, -0.953, -0.572, 0.429, 0.026, 0.063)
# names(wts) = c("HR", "Triple", "Double", "Single", "RBOE", "HBP", "NIBB", "IBB", "Bunt", "Out", "K", "PB", "WP", "BK", "SB", "CS", "PO", "POE", "OA", "INT", "FoulE", "DefInd")
# event_df = data.frame(event=unique(data$event))
# wtorders = c(10, 11, 10, 10, 3, 10, 1, 4, 10, 7, 10, 10, 10, 2, 10, 6, 5, 10, )