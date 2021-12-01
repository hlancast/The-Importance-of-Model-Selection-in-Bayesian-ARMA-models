# Correctly specified models 
setwd("C://Users//weeth//Documents//Stats Project//MA correct")

load(file = "ma2_t2.rda")

setwd("C://Users//weeth//Documents//Stats Project//MA Missspecified")
load(file = "ma1_mis2_500_t2.rda")
load(file = "ma2_mis3_500.rda")

save(ma2_t2, file = "ma2_t2.rda")


###############################
### MA[2] data to MA[1] model forecast
par(mfrow = c(2,2))

### t+1 ###

#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nMA(2) data MA(2) Model")

#set up colors
colors<- rep(gray(0.6), 100)



for(j in 1:100){
  lines(c(ma2_t2[j,7], ma2_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_t2[j,1],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-4,4),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nMA(2) data MA(1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ma1_mis2_500_t2[j,14], ma1_mis2_500_t2[j,16]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma1_mis2_500_t2[j,8], j , pch = 15)
}

### t+2 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nMA(2) data MA(2) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ma2_t2[j,8], ma2_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_t2[j,2],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-4,4),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nMA(2) data MA(1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ma1_mis2_500_t2[j,15], ma1_mis2_500_t2[j,17]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma1_mis2_500_t2[j,9], j , pch = 15)
}



###############################
### MA[2] data to MA[3] forecast
par(mfrow = c(2,2))

### t+1 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nMA(2) data MA(2) Model")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ma2_t2[j,7], ma2_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_t2[j,1],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-4,4),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nMA(2) data MA(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ma2_mis3_500[j,28], ma2_mis3_500[j,30]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_mis3_500[j,22], j , pch = 15)
}
### t+2 ###

#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nMA(2) data MA(2) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ma2_t2[j,8], ma2_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_t2[j,2],j, pch =15)
}


#forecast incorrect
plot(0,
     xlim = c(-4,4),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nMA(2) data MA(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ma2_mis3_500[j,29], ma2_mis3_500[j,31]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_mis3_500[j,23], j , pch = 15)
}
