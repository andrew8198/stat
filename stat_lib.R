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
