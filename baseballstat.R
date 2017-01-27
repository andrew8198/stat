library(dplyr)
setwd("~/GitHub/stat/")

# Read in data
data<-readRDS("alldata.RDS")


###-------------------------------------------------------------###
### Figure out where a BIP lands
###-------------------------------------------------------------###

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

# Check ball in play
ball_in_play(4,5)

# Set what region each BIP lands in
data$region <- apply(data[,c("our.x", "our.y")], 1, FUN=function(x) ball_in_play(x[1], x[2]))
# check that we have observations for the RSOI
dataR1 <- data %>% filter(region== "RIGHT SIDE OF INFIELD")
dim(dataR1)


###-------------------------------------------------------------###
### Figure out BIP outcome
###-------------------------------------------------------------###

# Function to get average weights per region
base_value <- function(Event){
  # Takes:
  # Returns: 
  
  if(Event == "Single"){
    return(1)
  }else if(Event == "Double") {
    return(2)
  } else if(Event == "Triple") {
    return(3)
  }else if(Event == "Home Run") {
    return(4)
  }else {
   return(0)}
}

base_value("Single")
base_value("Double")

# data_LEFT_SIDE_OF_INFIELD<-data%>%filter(region== "LEFT SIDE OF INFIELD")
# LEFT_SIDE_OF_INFIELD<-mean(data_LEFT_SIDE_OF_INFIELD$base_value)

# create colum that will use the base value function to find the mean
data$base_value<- sapply(data$Event, FUN=function(x)base_value(x))
summary(data$base_value)

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

# #find player values 
# A.S.S.<-function(x,y) {
#   if(is.na(x) | is.na(y)){
#     return(NA)
#     break
#   }
#   if((slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)>=0-90
#      & ( (slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)<=0+90) & 
#      (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5)>=60.5-90 & 
#      (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5) <=60.5+90 |
#      (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))<= 95)& abs(y/x)>=1 & x<0)
#     
#   {return("# LEFT SIDE OF INFIELD")}
#   else if((slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)>=0-90
#           & ( (slope(x,y,0,60.5)*x)+y_intercept(x,y,0,60.5)<=0+90) & 
#           (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5)>=60.5-90 & 
#           (y-(y_intercept(x,y,0,60.5)))/slope(x,y,0,60.5) <=60.5+90 |
#           (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))<= 95)& abs(y/x)>=1 & (x>0))
#   {return(" # RIGHT SIDE OF INFIELD")}
#   else if (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))>= 95 &abs(y)>=x &x>=0)
#   {return(" # RIGHT SIDE OF OUTFIELD")}
#   else if (abs(sqrt(0^2+60.5^2)-sqrt(x^2+y^2))>= 95 &abs(y)>=x &x<=0)
#   {return(" # LEFT SIDE OF OUTFIELD")}
#   else {
#     return(" #NOT IN PLAY")
#   }
# } 

###-------------------------------------------------------------###
### Test loc_slg on players
###-------------------------------------------------------------###

#filter players 10 (Cano, Jeter , Drew, J , Ortiz, D  Heyward, Prado  Suzuki, I Votto Bruce Loney )
Cano<-filter(data,batterName=="Cano") # get Cano's data
can <- tally(group_by(Cano,region)) # Get Cano's BIPs by region
loc_slg(can$n[1:5])

testguys <- c("Cano", "Jeter", "Drew, J", "Ortiz, D", "Heyward", "Prado", "Huff", "Uribe", "Glaus", "Votto")
test_locslg <- data.frame(name=NULL, locslg=NULL)
for(guy in testguys){
  # Filter out the data for that guy and store it as tmp
  tmp<-filter(data, batterName==guy) 
  
  # Tally the number of BIPs for that guy and store it as bips
  bips<-tally(group_by(tmp, region)) 
  
  # Take bips and
      # Reorder it so that it follows the order defined on line 101
      # If there is a region missing, add in a row for that region and an n=0 for that row
  
  loc_slg(bips$n[1:5])
  
  
  BIPS

  # Calculate loc_slg for 
  
  # Store the player's name and loc_slg value
  test_locslg$name <- c(test_locslg$name, guy)
  test_locslg$locslg <- c(test_locslg$locslg, lslg)
  
}


###-------------------------------------------------------------###
### Create stat based on run value (UNDER CONSTRUCTION)
###-------------------------------------------------------------###
value_stat<-function(x1,x2,x3,x4,ab)
  (((x1*.10524700)+(x3*0.90796180)+(x4*.82243020)*(x2*0.23114447))/ab)
value_stat(x1,x2,x3,x4,ab)

#create code for b 

ball_in_play(3,4)


value_stat(3,3,3,3,55)


value_stat(Cano)
head(Cano)
head(tally)
head(data$region)
