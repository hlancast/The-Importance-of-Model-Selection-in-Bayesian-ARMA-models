
# Correctly specified models 
setwd()

load(file = "arma_500.rda")
load(file = "arma2_500.rda")
load(file = "arma3_500.rda")

setwd()
load(file = "arma_mis2_500.rda")
load(file = "arma_mis3_500.rda")
load(file = "arma2_mis1_500.rda")
load(file = "arma2_mis3_500.rda")
load(file = "arma3_mis2_500.rda")
load(file = "arma3_mis1_500.rda")


### ARMA figue 1 ARMA(1,1) Data ARMA(1,2) Model ####
#arma 1 plot
par(mfrow = c(3,2))
#ar incorrect
mean(arma_500[,7] <= 0.5 & 0.5<= arma_500[,9])

ID<- which(!(arma_500[,7] <= 0.5 & 0.5<= arma_500[,9]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n ARMA(1,1) Data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.5, lty = 2)

for(j in 1:100){
  lines(c(arma_500[j,7], arma_500[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar - incorrect
mean(arma_mis3_500[,10] <= 0.5 & 0.5<= arma_mis3_500[,13])

ID<- which(!(arma_mis3_500[,10] <= 0.5 & 0.5<= arma_mis3_500[,13]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n ARMA(1,1) Data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.5, lty = 2)

for(j in 1:100){
  lines(c(arma_mis3_500[j,10], arma_mis3_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

# MA correct
mean(arma_500[,8] <= 0.3 & 0.3<= arma_500[,10])

ID<- which(!(arma_500[,8] <= 0.3 & 0.3<= arma_500[,10]))

plot(0,
     xlim = c(-0,0.7),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[1] Parameter \n ARMA(1,1) Data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(arma_500[j,8], arma_500[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ma1 - incorrect
mean(arma_mis3_500[,11] <= 0.3 & 0.3<= arma_mis3_500[,14])

ID<- which(!(arma_mis3_500[,11] <= 0.3 & 0.3<= arma_mis3_500[,14]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals AR Parameter \n ARMA(1,1) Data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(arma_mis3_500[j,11], arma_mis3_500[j,14]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}
plot.new()

#ma2 - incorrect
mean(arma_mis3_500[,12] <= 0 & 0<= arma_mis3_500[,15])

ID<- which(!(arma_mis3_500[,12] <= 0 & 0<= arma_mis3_500[,15]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(theta), 
     main = "credible Intervals MA[2] Parameter \n ARMA(1,1) Data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(arma_mis3_500[j,12], arma_mis3_500[j,15]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

##### 2nd ARMA plot figure ARMA(1,2) data ARMA (1,1) model #####
par(mfrow= c(2,2))
mean(arma3_500[,10] <= 0.5 & 0.5<= arma3_500[,13])

ID<- which(!(arma3_500[,10] <= 0.5 & 0.5<= arma3_500[,13]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \nARMA(1,2) Data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.5, ity = 2)

for(j in 1:100){
  lines(c(arma3_500[j,10], arma3_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar incorrect
mean(arma3_mis1_500[,7] <= 0.5 & 0.5<= arma3_mis1_500[,9])

ID<- which(!(arma3_mis1_500[,7] <= 0.5 & 0.5<= arma3_mis1_500[,9]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \nARMA(1,2) Data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.5, ity = 2)

for(j in 1:100){
  lines(c(arma3_mis1_500[j,7], arma3_mis1_500[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}
# MA parameter
mean(arma3_500[,11] <= 0.3 & 0.3<= arma3_500[,14])

ID<- which(!(arma3_500[,11] <= 0.3 & 0.3<= arma3_500[,14]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals MA Parameter \nARMA(1,2) Data ARMA(1,2) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.5, lty = 2)

for(j in 1:100){
  lines(c(arma3_500[j,11], arma3_500[j,14]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ma incorrect
mean(arma3_mis1_500[,8] <= 0.3 & 0.3<= arma3_mis1_500[,10])

ID<- which(!(arma3_mis1_500[,8] <= 0.3 & 0.3<= arma3_mis1_500[,10]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \nARMA(1,2) Data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.3, lty = 2)

for(j in 1:100){
  lines(c(arma3_mis1_500[j,8], arma3_mis1_500[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}


### ARMA figue 1 ARMA(1,1) Data ARMA(1,2) Model ####
#arma 1 plot
par(mfrow = c(3,2))
#ar1 incorrect
mean(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5])

ID<- which(!(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n AR(3) Data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,4], ar3_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar - incorrect
mean(arma2_misar3_500[,10] <= 0.8 & 0.8<= arma2_misar3_500[,13])

ID<- which(!(arma2_misar3_500[,10] <= 0.8 & 0.8<= arma2_misar3_500[,13]))

plot(0,
     xlim = c(0.5,1.1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n AR(3) Data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(arma2_misar3_500[j,10], arma2_misar3_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar2 correct
mean(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12])

ID<- which(!(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12]))

plot(0,
     xlim = c(-0.9,0),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \n AR(3) Data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.5, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,11], ar3_500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar - incorrect
mean(arma2_misar3_500[,11] <= -0.5 & -0.5<= arma2_misar3_500[,14])

ID<- which(!(arma2_misar3_500[,11] <= -0.5 & -0.5<= arma2_misar3_500[,14]))

plot(0,
     xlim = c(-1,0),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \n AR(3) Data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.5, lty = 2)

for(j in 1:100){
  lines(c(arma2_misar3_500[j,11], arma2_misar3_500[j,14]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}


plot.new()


#ma - incorrect
mean(arma2_misar3_500[,12] <= 0 & 0<= arma2_misar3_500[,15])

ID<- which(!(arma2_misar3_500[,12] <= 0 & 0<= arma2_misar3_500[,15]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals MA Parameter \n AR(3) Data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(arma2_misar3_500[j,12], arma2_misar3_500[j,15]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}


################################################
### ARMA figue 1 AR{3) Data ARMA(2,1) Model ####
#arma 1 plot
par(mfrow = c(3,2))
#ar1 incorrect
mean(arma_500[,4] <= 0.8 & 0.8<= ar3_500[,5])

ID<- which(!(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n AR(3) Data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,4], ar3_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar - incorrect
mean(arma2_misar3_500[,10] <= 0.8 & 0.8<= arma2_misar3_500[,13])

ID<- which(!(arma2_misar3_500[,10] <= 0.8 & 0.8<= arma2_misar3_500[,13]))

plot(0,
     xlim = c(0.5,1.1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n AR(3) Data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(arma2_misar3_500[j,10], arma2_misar3_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar2 correct
mean(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12])

ID<- which(!(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12]))

plot(0,
     xlim = c(-0.9,0),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \n AR(3) Data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.5, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,11], ar3_500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar2 - incorrect
mean(arma2_misar3_500[,11] <= -0.5 & -0.5<= arma2_misar3_500[,14])

ID<- which(!(arma2_misar3_500[,11] <= -0.5 & -0.5<= arma2_misar3_500[,14]))

plot(0,
     xlim = c(-1,0),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \n AR(3) Data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.5, lty = 2)

for(j in 1:100){
  lines(c(arma2_misar3_500[j,11], arma2_misar3_500[j,14]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}


plot.new()


#ma - incorrect
mean(arma2_misar3_500[,12] <= 0 & 0<= arma2_misar3_500[,15])

ID<- which(!(arma2_misar3_500[,12] <= 0 & 0<= arma2_misar3_500[,15]))

plot(0,
     xlim = c(-1,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals MA Parameter \n AR(3) Data ARMA(2,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(arma2_misar3_500[j,12], arma2_misar3_500[j,15]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}



################################################
### ARMA figue 1 AR{3) Data ARMA(1,1) Model ####
par(mfrow = c(2,2))
#AR correct 
mean(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5])

ID<- which(!(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5]))

plot(0,
     xlim = c(0.4,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n AR(3) Data AR(3) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,4], ar3_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#AAr incorrect
mean(arma_misar3_500[,7] <= 0.8 & 0.8<= arma_misar3_500[,9])

ID<- which(!(arma_misar3_500[,7] <= 0.8 & 0.8<= arma_misar3_500[,9]))

plot(0,
     xlim = c(0,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter \n AR(3) Data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(arma_misar3_500[j,7], arma_misar3_500[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

plot.new()

#MA incorrect
mean(arma_misar3_500[,8] <= 0 & 0<= arma_misar3_500[,10])

ID<- which(!(arma_misar3_500[,8] <= 0 & 0<= arma_misar3_500[,10]))

plot(0,
     xlim = c(-0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals MA Parameter \n AR(3) Data ARMA(1,1) Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(arma_misar3_500[j,8], arma_misar3_500[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}
