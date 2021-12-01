
load(file = "ar3_t2.rda")


load(file = "ar1_ar3_500t2.rda")
load(file = "ar3_ar4_500t2.rda")

###############################
### AR[3] data to AR[1] model forecast
par(mfrow = c(2,2))

#forecast correct
plot(0,
     xlim = c(-6,6),
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
     main = "credible Intervals t+1 Forecast \nAR(3) data AR(1) Model")

#set up colors
colors<- rep(gray(0.6), 100)

abline(v = ar2.true[501], lty = 2)

for(j in 1:100){
  lines(c(ar1_ar3_500t2[j,7], ar1_ar3_500t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar1_ar3_500t2[j,1], j , pch = 15)
}

#forecast correct
plot(0,
     xlim = c(-6,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data AR(3) Model")

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
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data AR(1) Model")

#set up colors
colors<- rep(gray(0.6), 100)



for(j in 1:100){
  lines(c(ar1_ar3_500t2[j,8], ar1_ar3_500t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar1_ar3_500t2[j,2], j , pch = 15)
}



###############################
### AR[3] data to AR[4] model forecast
par(mfrow = c(2,2))

#forecast correct
plot(0,
     xlim = c(-6,6),
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
     main = "credible Intervals t+1 Forecast \nAR(3) data AR(4) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ar3_ar4_500t2[j,7], ar3_ar4_500t2[j,9]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_ar4_500t2[j,1], j , pch = 15)
}

#forecast correct
plot(0,
     xlim = c(-6,6),
     ylim = c(1,100),
     ylab = "Model", 
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data AR(3) Model")

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
     xlab = expression('y'[t+2]), 
     main = "credible Intervals t+2 Forecast \nAR(3) data AR(4) Model")

#set up colors
colors<- rep(gray(0.6), 100)


for(j in 1:100){
  lines(c(ar3_ar4_500t2[j,8], ar3_ar4_500t2[j,10]), 
        c(j,j), 
        col = colors[j],
        lwd = 2)
  points(ar3_ar4_500t2[j,2], j , pch = 15)
}
