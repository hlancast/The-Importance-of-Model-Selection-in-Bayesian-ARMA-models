# Correctly specified models 
setwd()

load(file = "ma500.rda")
load(file = "ma2_500.rda")
load(file = "ma3_500.rda")

setwd()
load(file = "ma_mis1_500.rda")
load(file = "ma_mis2_500.rda")


################ first MA plot ################
#ma 1 plot
par(mfrow = c(1,2))
#ma
mean(ma500[,4] <= 0.3 & 0.3<= ma500[,5])

ID<- which(!(ma500[,4] <= 0.3 & 0.3<= ma500[,5]))

plot(0,
     xlim = c(0,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA Parameter \nMA[1] Model MA[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(ma500[j,4], ma500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ma underfit 
mean(ma_mis2_500[,4] <= 0.3 & 0.3<= ma_mis2_500[,5])

ID<- which(!(ma_mis2_500[,4] <= 0.3 & 0.3<= ma_mis2_500[,5]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA Parameter \nMA[1] Model MA[2] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(ma_mis2_500[j,4], ma_mis2_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

### second MA plot ###
par(mfrow = c(2,2))
#ma
mean(ma500[,4] <= 0.3 & 0.3<= ma500[,5])

ID<- which(!(ma500[,4] <= 0.3 & 0.3<= ma500[,5]))

plot(0,
     xlim = c(0,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA Parameter \nMA[1] Model MA[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(ma500[j,4], ma500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ma1 overfit
mean(ma_mis1_500[,4] <= 0.3 & 0.3<= ma_mis1_500[,5])

ID<- which(!(ma_mis1_500[,4] <= 0.3 & 0.3<= ma_mis1_500[,5]))

plot(0,
     xlim = c(0,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA Parameter \nMA[2] Model MA[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(ma_mis1_500[j,4], ma_mis1_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}


#ma2 overfit
mean(ma_mis1_500[,11] <= 0 & 0<= ma_mis1_500[,12])

ID<- which(!(ma_mis1_500[,11] <= 0 & 0 <= ma_mis1_500[,12]))

plot(0,
     xlim = c(-0.5,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA Parameter \nMA[2] Model MA[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(ma_mis1_500[j,11], ma_mis1_500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#forecast
plot(0,
     xlim = c(-4,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = "t+1 Forecast", 
     main = "MA(1) Forecast t+1")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ma500[j,9], ma500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ma 2 plot
par(mfrow = c(2,1))
#ma 1
mean(ma2_500[,7] <= 0.3 & 0.3<= ma2_500[,9])

ID<- which(!(ma2_500[,7] <= 0.3 & 0.3<= ma2_500[,9]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[1] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(ma2_500[j,7], ma2_500[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_500[j,1], j, pch = 15)
}

#ma
mean(ma2_500[,8] <= -0.3 & -0.3<= ma2_500[,10])

ID<- which(!(ma2_500[,8] <= -0.3 & -0.3<= ma2_500[,10]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[2] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.3, lty = 2)

for(j in 1:100){
  lines(c(ma2_500[j,8], ma2_500[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma2_500[j,2], j, pch = 15)
}


#forecast
plot(0,
     xlim = c(-4,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = "t+1 Forecast", 
     main = "MA(2) Forecast t+1")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ma2_500[j,16], ma2_500[j,19]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#Mvoing avergae 3 plot
par(mfrow = c(1,1))
#ma 1
mean(ma3_500[,10] <= 0.3 & 0.3<= ma3_500[,13])

ID<- which(!(ma3_500[,10] <= 0.3 & 0.3 <= ma3_500[,13]))

plot(0,
     xlim = c(0,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[1] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(ma3_500[j,10], ma3_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma3_500[j,1], j, pch = 15)
}

#ma 2
mean(ma3_500[,11] <= -0.3 & -0.3<= ma3_500[,14])

ID<- which(!(ma3_500[,11] <= -0.3 & -0.3<= ma3_500[,14]))

plot(0,
     xlim = c(-0.5,0),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[2] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.3, lty = 2)

for(j in 1:100){
  lines(c(ma3_500[j,11], ma3_500[j,14]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma3_500[j,2], j, pch = 15)
}

#ma 3
mean(ma3_500[,12] <= -0.1 & -0.1 <= ma3_500[,15])

ID<- which(!(ma3_500[,12] <= -0.1 & -0.1<= ma3_500[,15]))

plot(0,
     xlim = c(-0.5,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[3] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.1, lty = 2)

for(j in 1:100){
  lines(c(ma3_500[j,12], ma3_500[j,15]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ma3_500[j,3], j, pch = 15)
}

#forecast
plot(0,
     xlim = c(-1,3),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = "t+1 Forecast", 
     main = "MA(3) Forecast t+1")

#set up colors
colors<- rep(gray(0.6), 100)

for(j in 1:100){
  lines(c(ma3_500[j,23], ma3_500[j,26]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

