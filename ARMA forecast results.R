setwd()

load(file = "arma3_500_t2.rda")

setwd()

load(file = "arma3_mis1_500_t2.rda")
load(file = "arma3_mis2_500_t2.rda")

load(file = "arma_misar3_500.rda")
load(file = "arma2_misar3_500.rda")



###############################
### ARMA(1,1) mis to ARMA(1,2) model forecast
par(mfrow = c(2,2))
### t+1 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nARMA(1,2) data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_500_t2[j,7], arma3_500_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_500_t2[j,1],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nARMA(1,2) data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_mis1_500_t2[j,7], arma3_mis1_500_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_mis1_500_t2[j,1], j, pch = 15)
}

### t=2 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nARMA(1,2) data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_500_t2[j,8], arma3_500_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_500_t2[j,2],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nARMA(1,2) data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_mis1_500_t2[j,8], arma3_mis1_500_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_mis1_500_t2[j,2], j, pch = 15)
}




###############################
### ARMA(1,2) mis to ARMA(1,2) model forecast
par(mfrow = c(2,2))

### t+1 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nARMA(1,2) data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_500_t2[j,7], arma3_500_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_500_t2[j,1],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nARMA(1,2) data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_mis2_500_t2[j,7], arma3_mis2_500_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_mis2_500_t2[j,1], j, pch = 15)
}


### t+2 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nARMA(1,2) data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_500_t2[j,8], arma3_500_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_500_t2[j,2],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nARMA(1,2) data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma3_mis2_500_t2[j,8], arma3_mis2_500_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma3_mis2_500_t2[j,2], j, pch = 15)
}




###############################
### AR(3) data to ARMA(1,1) model forecast
par(mfrow = c(2,2))
### t+1 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nAR(3) data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ar3_t2[j,7], ar3_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_t2[j,1],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-6,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nAR(3) data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma_misar3_500[j,21], arma_misar3_500[j,23]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma_misar3_500[j,15],j, pch =15)
}

### t+2 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ar3_t2[j,8], ar3_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_t2[j,2],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-6,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+1 Forecast \nAR(3) data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma_misar3_500[j,22], arma_misar3_500[j,24]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma_misar3_500[j,16],j, pch =15)
}



###############################
### ARM(3) data to ARMA(2,1) model forecast
par(mfrow = c(2,2))

### t+1 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nAR(3) data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ar3_t2[j,7], ar3_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_t2[j,1],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-6,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+1]), 
     main = "credible Intervals t+1 Forecast \nAR(3) data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma2_misar3_500_t2[j,7], arma2_misar3_500_t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma2_misar3_500_t2[j,1],j, pch =15)
}

### t+2 ###
#forecast correct
plot(0,
     xlim = c(-5,5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ar3_t2[j,8], ar3_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_t2[j,2],j, pch =15)
}

#forecast incorrect
plot(0,
     xlim = c(-6,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(arma2_misar3_500_t2[j,8], arma2_misar3_500_t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(arma2_misar3_500_t2[j,2],j, pch =15)
}
