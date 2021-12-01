setwd("C://Users//weeth//Documents//Stats Project//AR correct")

load(file = "ar1_500.rda")
load(file = "ar3_500.rda")
load(file = "ar4_500.rda")

#############################
#### Ar1 underfit AR1 ######
par(mfrow = c(1,2))
#ar
mean(ar1_500[,4] <= 0.8 & 0.8<= ar1_500[,5])

ID<- which(!(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "Credible Intervals AR Parameter \n AR[3] Model AR[3] Data")

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

#fraction of 100 simulated credible intervals should be close to 95%
#ar 3 data to ar 1 model plot
#ar1
mean(ar_mis3_500[,4] <= 0.8 & 0.8<= ar_mis3_500[,5])

ID<- which(!(ar_mis3_500[,4] <= 0.8 & 0.8<= ar_mis3_500[,5]))

plot(0,
     xlim = c(0.2,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi),
     main = "Credible Intervals AR Parameter \n AR[1] Model AR[3] Data")


#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar_mis3_500[j,4], ar_mis3_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}




#ar 4 data to ar 1 model plot
par(mfrow = c(1,1))
#ar
mean(ar_mis4_500[,4] <= 0.8 & 0.8<= ar_mis4_500[,5])

ID<- which(!(ar_mis4_500[,4] <= 0.8 & 0.8<= ar_mis4_500[,5]))

plot(0,
     xlim = c(0.2,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar_mis4_500[j,4], ar_mis4_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#forecast
plot(0,
     xlim = c(-4,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar1_500[j,9], ar1_500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar 3 plot - side by side comparisons
par(mfrow = c(1,2))
#ar1 - corret
mean(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5])

ID<- which(!(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "Credible Intervals AR[1] Parameter \nAR[3] data AR[3] Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,4], ar3_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_500[j,1], j, pch = 15)
}

#ar 1 data to ar 3 model plot
mean(ar3_overfit1_500[,10] <= 0.8 & 0.8<= ar3_overfit1_500[,13])

ID<- which(!(ar3_overfit1_500[,10] <= 0.8 & 0.8<= ar3_overfit1_500[,13]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "Credible Intervals AR[1] Parameter \n AR[3] Model AR[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar3_overfit1_500[j,10], ar3_overfit1_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_overfit1_500[j,1], j, pch = 15)
}


#ar2
mean(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12])

ID<- which(!(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12]))

plot(0,
     xlim = c(-0.7,-0.2),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter")

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

#ar3
mean(ar3_500[,18] <= -0.1 & -0.1<= ar3_500[,19])

ID<- which(!(ar3_500[,18] <= -0.1 & -0.1<= ar3_500[,19]))

plot(0,
     xlim = c(-0.7,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[3] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.1, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,18], ar3_500[j,19]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#forecast
plot(0,
     xlim = c(-4,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar1_500[j,9], ar1_500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar 4 plots
par(mfrow = c(2,2))
#ar
mean(ar4_500[,4] <= 0.8 & 0.8<= ar4_500[,5])

ID<- which(!(ar4_500[,4] <= 0.8 & 0.8<= ar4_500[,5]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[1] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar4_500[j,4], ar4_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar2
mean(ar4_500[,11] <= -0.5 & -0.5<= ar4_500[,12])

ID<- which(!(ar4_500[,11] <= -0.5 & -0.5<= ar4_500[,12]))

plot(0,
     xlim = c(-0.7,-0.2),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.5, lty = 2)

for(j in 1:100){
  lines(c(ar4_500[j,11], ar4_500[j,12]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar3
mean(ar4_500[,18] <= -0.1 & -0.1<= ar4_500[,19])

ID<- which(!(ar4_500[,18] <= -0.1 & -0.1<= ar4_500[,19]))

plot(0,
     xlim = c(-0.7,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[3] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.1, lty = 2)

for(j in 1:100){
  lines(c(ar4_500[j,18], ar4_500[j,19]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar4
mean(ar4_500[,25] <= 0.2 & 0.2<= ar4_500[,26])

ID<- which(!(ar4_500[,25] <= 0.2 & 0.2<= ar4_500[,26]))

plot(0,
     xlim = c(-0.1,0.4),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[4] Parameter")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.2, lty = 2)

for(j in 1:100){
  lines(c(ar4_500[j,25], ar4_500[j,26]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

### Figure X AR3 overfit AR(4)  ### 
par(mfrow = c(3,2))
#AR1 correct
mean(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5])

ID<- which(!(ar3_500[,4] <= 0.8 & 0.8<= ar3_500[,5]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "Credible Intervals AR[1] Parameter \nAR[3] data AR[3] Model")

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


#ar1 - incorrect
mean(ar4_overfit3_500[,13] <= 0.8 & 0.8<= ar4_overfit3_500[,17])

ID<- which(!(ar4_overfit3_500[,13] <= 0.8 & 0.8<= ar4_overfit3_500[,17]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[1] Parameter \nAR[3] Data AR[4] Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar4_overfit3_500[j,13], ar4_overfit3_500[j,17]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar2 - correct
mean(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12])

ID<- which(!(ar3_500[,11] <= -0.5 & -0.5<= ar3_500[,12]))

plot(0,
     xlim = c(-0.7,-0.2),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \nAR[3] Data AR[3] Model")

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
mean(ar4_overfit3_500[,14] <= -0.5 & -0.5<= ar4_overfit3_500[,18])

ID<- which(!(ar4_overfit3_500[,14] <= -0.5 & -0.5<= ar4_overfit3_500[,18]))

plot(0,
     xlim = c(-1,0),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \nAR[3] Data AR[4] Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.5, lty = 2)

for(j in 1:100){
  lines(c(ar4_overfit3_500[j,14], ar4_overfit3_500[j,18]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

par(mfrow= c(1,2))
#ar3 - correct
mean(ar3_500[,18] <= -0.1 & -0.1<= ar3_500[,19])

ID<- which(!(ar3_500[,18] <= -0.1 & -0.1<= ar3_500[,19]))

plot(0,
     xlim = c(-0.7,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[3] Parameter \n AR[3] Data AR[3] Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.1, lty = 2)

for(j in 1:100){
  lines(c(ar3_500[j,18], ar3_500[j,19]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar3 incorrect
mean(ar4_overfit3_500[,15] <= -0.1 & -0.1 <= ar4_overfit3_500[,19])

ID<- which(!(ar4_overfit3_500[,15] <= -0.1 & -0.1 <= ar4_overfit3_500[,19]))

plot(0,
     xlim = c(-0.5,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[3] Parameter \nAR[4] Data AR[4] Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = -0.1, lty = 2)

for(j in 1:100){
  lines(c(ar4_overfit3_500[j,15], ar4_overfit3_500[j,19]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

par(mfrow=c(1,2))
plot.new()

#ar4 incorrect
mean(ar4_overfit3_500[,16] <= 0 & 0 <= ar4_overfit3_500[,20])

ID<- which(!(ar4_overfit3_500[,16] <= 0 & 0 <= ar4_overfit3_500[,20]))

plot(0,
     xlim = c(-0.5,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[4] Parameter \nAR[4] Data AR[4] Model")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(ar4_overfit3_500[j,16], ar4_overfit3_500[j,20]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

################################
### AR1 overfit AR3 #########
par(mfrow = c(2,2))
#ar
mean(ar1_500[,4] <= 0.8 & 0.8<= ar1_500[,5])

ID<- which(!(ar1_500[,4] <= 0.8 & 0.8<= ar1_500[,5]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "Credible Intervals AR Parameter \n AR[1] Model AR[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar1_500[j,4], ar1_500[j,5]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar1
mean(ar3_overfit1_500[,10] <= 0.8 & 0.8<= ar3_overfit1_500[,13])

ID<- which(!(ar3_overfit1_500[,10] <= 0.8 & 0.8<= ar3_overfit1_500[,13]))

plot(0,
     xlim = c(0.5,1),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[1] Parameter \n AR[3] Model AR[1] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0.8, lty = 2)

for(j in 1:100){
  lines(c(ar3_overfit1_500[j,10], ar3_overfit1_500[j,13]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar2
mean(ar3_overfit1_500[,11] <= 0 & 0<= ar3_overfit1_500[,14])

ID<- which(!(ar3_overfit1_500[,11] <= 0 & 0<= ar3_overfit1_500[,14]))

plot(0,
     xlim = c(-0.4,0.4),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[2] Parameter \n AR[3] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(ar3_overfit1_500[j,11], ar3_overfit1_500[j,14]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}

#ar3
mean(ar3_overfit1_500[,12] <= 0 & 0 <= ar3_overfit1_500[,15])

ID<- which(!(ar3_overfit1_500[,12] <= 0 & 0<= ar3_overfit1_500[,15]))

plot(0,
     xlim = c(-0.5,0.5),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression(phi), 
     main = "credible Intervals AR[3] Parameter \nAR[3] Data")

#set up colors
colors<- rep(gray(0.6), 100)
colors[ID]<- "red"

abline(v = 0, lty = 2)

for(j in 1:100){
  lines(c(ar3_overfit1_500[j,12], ar3_overfit1_500[j,15]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
}
