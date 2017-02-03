###---------------------------------------------------------###
###---------------------------------------------------------###
###      ANALYSIS FOR ANDREW'S BASEBALL STAT
###---------------------------------------------------------###
###---------------------------------------------------------###

source("baseballstat.R")

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
