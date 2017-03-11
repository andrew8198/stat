library(ggplot2)
library(dplyr)

data = readRDS("alldata.RDS")
data$bases = sapply(data$Event, function(x) if(x == "Out"){
  return(0)
} else if (x == "Single"){
  return(1)
} else if (x == "Double"){
  return(2)
} else if (x == "Triple"){
  return(3)
} else if (x == "Home Run"){
  return(4)
} else {
  return(NA)
})

stadium_lines <- list(generic=list(
          diamond = data.frame(x=c(0, -127.28 / 2, 0, 127.28 / 2, 0),
                                y=c(0, 127.28 / 2, 127.28, 127.28 / 2, 0)),
          fouline1 = data.frame(x=c(-127.28 / 2, -300),
                                 y=c(127.28 / 2, 300)),
          fouline2 = data.frame(x=c(127.28 / 2, 300),
                                 y=c(127.28 / 2, 300)),
          fence = data.frame(x=seq(-300, 300, length.out=100),
                              y=400 - .0011 * 
                                seq(-300, 300, length.out=100) ^ 2)
))

locationSlgDF <- function(nx=50, ny=50, conditions=NULL, data){ 

  xgrid = seq(-315, 315, length.out=nx)
  ygrid = seq(-125, 500, length.out=ny)
  
  dat = data %>% filter(!is.na(our.x) & !is.na(our.y))
  if(!is.null(conditions)){
    dat = dat %>% filter_(conditions)
  }
  
  dat$grid_x = sapply(dat$our.x, FUN=function(x) 
    unlist(sapply(1:(nx-1), FUN=function(i) if(xgrid[i] <= x & x <= xgrid[i+1]){return(i)})))
  dat$grid_y = sapply(dat$our.y, FUN=function(y) 
    unlist(sapply(1:(ny-1), FUN=function(i) if(ygrid[i] <= y & y <= ygrid[i+1]){return(i)})))
  dgrd = dat %>% group_by(grid_x, grid_y) %>% summarize(mnbase = mean(bases)) %>% left_join(dat)
  
  return(dgrd)
}
  
heatSprayChart <- function(var, data, stadium="generic", conditions=NULL){
  
  if(!is.null(conditions)){
    data = data %>% filter_(conditions)
  }
  
  diamond = stadium_lines[[stadium]]$diamond
  fouline1 = stadium_lines[[stadium]]$fouline1
  fouline2 = stadium_lines[[stadium]]$fouline2
  fence = stadium_lines[[stadium]]$fence
  
  
  pp = ggplot(data, aes(our.x, our.y)) + geom_point(aes_(var), alpha=.5) +
    geom_path(aes(x=x, y=y), data=diamond, lwd=.5, col="gray") +
    geom_path(aes(x, y), data=fouline1, lwd=.5, col="gray") +
    geom_path(aes(x, y), data=fouline2, lwd=.5, col="gray") +
    geom_path(aes(x, y), data=fence, lwd=.5, col="gray") +
    coord_fixed(ratio=1) +
    scale_colour_gradient(low="blue", high="red")
  
  return(pp)
}


  dat1 = data %>% filter(!is.na(our.x) & !is.na(our.y), stand=="R", throws=="R")
  dat1$grid1 = sapply(dat1$our.x, FUN=function(x) 
            unlist(sapply(1:(nx-1), FUN=function(i) if(xgrid[i] <= x & x <= xgrid[i+1]){return(i)})))
  dat1$grid2 = sapply(dat1$our.y, FUN=function(y) 
            unlist(sapply(1:(ny-1), FUN=function(i) if(ygrid[i] <= y & y <= ygrid[i+1]){return(i)})))
  dgrd1 = dat1 %>% group_by(grid1, grid2) %>% summarize(mnbase = mean(bases)) %>% left_join(dat1)

dat2 = data %>% filter(!is.na(our.x) & !is.na(our.y), stand=="R", throws=="L")
dat2$grid1 = sapply(dat2$our.x, FUN=function(x) 
  unlist(sapply(1:(nx-1), FUN=function(i) if(xgrid[i] <= x & x <= xgrid[i+1]){return(i)})))
dat2$grid2 = sapply(dat2$our.y, FUN=function(y) 
  unlist(sapply(1:(ny-1), FUN=function(i) if(ygrid[i] <= y & y <= ygrid[i+1]){return(i)})))
dgrd = dat2 %>% group_by(grid1, grid2) %>% summarize(mnbase = mean(bases)) %>% left_join(dat2) %>% rbind(dgrd1)


ggplot(filter(dgrd, throws=="L"), aes(our.x, our.y, color=mnbase)) + geom_point(alpha=.5) +
  geom_path(aes(x=x, y=y), data=diamond, lwd=.5, col="gray") +
  geom_path(aes(x, y), data=fouline1, lwd=.5, col="gray") +
  geom_path(aes(x, y), data=fouline2, lwd=.5, col="gray") +
  geom_path(aes(x, y), data=fence, lwd=.5, col="gray") +
  coord_fixed(ratio=1) +
  scale_colour_gradient(low="blue", high="red")

ggplot(filter(dgrd, throws=="R"), aes(our.x, our.y, color=mnbase)) + geom_point(alpha=.5) +
  geom_path(aes(x=x, y=y), data=diamond, lwd=.5, col="gray") +
  geom_path(aes(x, y), data=fouline1, lwd=.5, col="gray") +
  geom_path(aes(x, y), data=fouline2, lwd=.5, col="gray") +
  geom_path(aes(x, y), data=fence, lwd=.5, col="gray") +
  coord_fixed(ratio=1) +
  scale_colour_gradient(low="blue", high="red")

dd = dgrd %>% group_by(batterId) %>% summarize(locslg = mean(mnbase)) %>% ungroup() %>% left_join(dgrd)
dd %>% distinct(batterId, batterName, locslg) %>% arrange(desc(locslg)) %>% head()

  
  
