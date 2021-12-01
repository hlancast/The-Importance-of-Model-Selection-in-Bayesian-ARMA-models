install.packages("future.apply")
install.packages("parallel")

# load packages 
library(rstan)
library(dplyr)
library(brms)
library(bayesforecast)
library(parallel)

## different n sizes were experimented with but not reported
setwd("")

#Simulate ARMA Processes
# arma[1,1]

arma_mod1<- list(ar = c(0.5),ma = c(0.3), order = c(1, 0, 1))
set.seed(123)
arma1.sim1<- replicate(100,arima.sim(n = 50, model = arma_mod1, mean = 0, sd = 1))
set.seed(123)
arma1.sim2<- replicate(100,arima.sim(n = 150, model = arma_mod1, mean = 0, sd = 1))
set.seed(123)
arma1.sim3<- replicate(100,arima.sim(n = 500, model = arma_mod1, mean = 0, sd = 1))

#arma[2,1]
arma_mod2<- list(ar = c(0.5, 0.1),ma = c(0.3), order = c(2, 0, 1))
set.seed(42)
arma2.sim1<- replicate(100,arima.sim(n = 50, model = arma_mod2, mean = 0, sd = 1))
set.seed(43)
arma2.sim2<- replicate(100,arima.sim(n = 150, model = arma_mod2, mean = 0, sd = 1))
set.seed(44)
arma2.sim3<- replicate(100,arima.sim(n = 500, model = arma_mod2, mean = 0, sd = 1))

#ma[3]
arma_mod3<- list(ar = c(0.5),ma = c(0.3, -0.1), order = c(1, 0, 2))
set.seed(7)
arma3.sim1<- replicate(100,arima.sim(n = 50, model = arma_mod3, mean = 0, sd = 1))
set.seed(71)
arma3.sim2<- replicate(100,arima.sim(n = 150, model = arma_mod3, mean = 0, sd = 1))
set.seed(712)
arma3.sim3<- replicate(100,arima.sim(n = 500, model = arma_mod3, mean = 0, sd = 1))


# Fitting properly specified models
# The general process here is to fit the models, put them in a list
# and then extract posterior estimates into a data frame
# Then repeat to make a forecast

#ARMA(1,1) 50
# initialize matrix to store credible intervals
arma_50 = matrix(nrow = 100, ncol = 19)

l = lapply(1:100, function(x) stan_sarima(arma1.sim1[,x], order = c(1,0,1), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))
#extract credible summaries
l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
#extract forecast
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma_50[1:100,1:14] = l1
arma_50[1:100, 15:19] = l2

colnames(arma_50) = c("ar_mean", "ma_mean", "ar_se_mean", "ma_se_mean", "ar_sd", "ma_sd",
                          "ar_2.5%", "ma_2.5%", "ar_97.5%", "ma_97.5%", "ar_n_eff", "ma2_n_eff",
                          "ar_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma_50 = as.data.frame(arma_50)
save(arma_50, file = "arma_50.rda")

rm(l,l1,l2)



#ARMA(1,1) 150
arma_150 = matrix(nrow = 100, ncol = 19)
l = lapply(51:100, function(x) stan_sarima(arma1.sim2[,x], order = c(1,0,1), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma_150[51:100,1:14] = l1
arma_150[51:100, 15:19] = l2

colnames(arma_150) = c("ar_mean", "ma_mean", "ar_se_mean", "ma_se_mean", "ar_sd", "ma_sd",
                          "ar_2.5%", "ma_2.5%", "ar_97.5%", "ma_97.5%", "ar_n_eff", "ma2_n_eff",
                          "ar_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma_150 = as.data.frame(arma_150)
save(arma_150, file = "arma_150.rda")
load(file = "arma_150.rda")
rm(l,l1,l2)

### Here I began Parallelizing the Code 
#parallelize it to save some time
#ARMA(1,1) 500
arma_500f = matrix(nrow = 100, ncol = 10)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma1.sim3'))
clusterSetRNGStream(cl, 123)

# fit the models in Stan
l = parLapply( cl, 1:100, function(x) stan_sarima(arma1.sim3[,x], order = c(1,0,1), seasonal = c(0,0,0),
                                                 iter = 2000, chains = 2, seed = 123)) 

stopCluster(cl)

#Make a forecast 

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)


arma_500f[100, 15:19] = l2

colnames(arma_500) = c("ar_mean", "ma_mean", "ar_se_mean", "ma_se_mean", "ar_sd", "ma_sd",
                          "ar_2.5%", "ma_2.5%", "ar_97.5%", "ma_97.5%", "ar_n_eff", "ma2_n_eff",
                          "ar_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma_500 = as.data.frame(arma_500)
save(arma_500, file = "arma_500.rda")
load(file = "arma_500.rda")
rm(l,l1,l2)

#ARMA(2,1) 50 
arma2_50 = matrix(nrow = 100, ncol = 26)
l = lapply(1:100, function(x) stan_sarima(arma2.sim1[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma2_50[1:100,1:21] = l1
arma2_50[1:100, 22:26] = l2

colnames(arma2_50) = c("ar1_mean", "ar2_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                          "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                          "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_50 = as.data.frame(arma2_50)
save(arma2_50, file = "arma2_50.rda")
load(file = "ma_mis3_50.rda")
rm(l,l1,l2)

#ARMA(2,1) 150 
arma2_150 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma2.sim2'))
clusterSetRNGStream(cl, 123)

l = parLapply(cl, 51:100, function(x) stan_sarima(arma2.sim2[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))
stopCluster(cl)
l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma2_150[51:100,1:21] = l1
arma2_150[51:100, 22:26] = l2

colnames(arma2_150) = c("ar1_mean", "ar1_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                          "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                          "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_150 = as.data.frame(arma2_150)
save(arma2_150, file = "arma2_150.rda")
load(file = "ma_mis3_50.rda")
rm(l,l1,l2)

#parallelize it to save some time
#ARMA(2,1) 500
arma2_500 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma2.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply( cl, 91:100, function(x) stan_sarima(arma2.sim3[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                                   iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)


l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))

l2<- t(l2)


arma2_500[91:100,1:21] = 2
arma2_500[91:100, 22:26] = l2

colnames(arma2_500) = c("ar1_mean", "ar1_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                       "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                       "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_500 = as.data.frame(arma2_500)
save(arma2_500, file = "arma2_500.rda")
load(file = "arma2_500.rda")
rm(l,l1,l2)

#ARMA(2,1) 50 
arma2_50 = matrix(nrow = 100, ncol = 26)
l = lapply(1:100, function(x) stan_sarima(arma2.sim1[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma2_50[1:100,1:21] = l1
arma2_50[1:100, 22:26] = l2

colnames(arma2_50) = c("ar1_mean", "ar1_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                       "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                       "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_50 = as.data.frame(arma2_50)
save(arma2_50, file = "arma2_50.rda")
load(file = "ma_mis3_50.rda")
rm(l,l1,l2)

#ARMA(2,1) 150 
arma2_150 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma2.sim2'))
clusterSetRNGStream(cl, 123)

l = parLapply(cl, 51:100, function(x) stan_sarima(arma2.sim2[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                                  iter = 5000, chains = 4, seed = 123))
stopCluster(cl)
l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma2_150[51:100,1:21] = l1
arma2_150[51:100, 22:26] = l2

colnames(arma2_150) = c("ar1_mean", "ar1_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                        "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                        "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_150 = as.data.frame(arma2_150)
save(arma2_150, file = "arma2_150.rda")
load(file = "ma_mis3_50.rda")
rm(l,l1,l2)

#parallelize it to save some time
#ARMA(1,2) 500
arma3_500 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma3.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply( cl, 91:100, function(x) stan_sarima(arma3.sim3[,x], order = c(1,0,2), seasonal = c(0,0,0),
                                                   iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]", "ma[2]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)


l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))

l2<- t(l2)


arma3_500[91:100,1:21] = l1
arma3_500[91:100, 22:26] = l2

colnames(arma3_500) = c("ar_mean", "ma1_mean","ma2_mean", "ar_se_mean","ma1_se_mean", "ma2_se_mean", "ar_sd","ma1_sd", "ma2_sd",
                        "ar_2.5%","ma1_2.5%", "ma2_2.5%", "ar_97.5%","ma1_97.5%", "ma2_97.5%", "ar_n_eff",  "ma1_n_eff", "ma2_n_eff",
                        "ar_Rhat", "ma1_Rhat", "ma2_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma3_500 = as.data.frame(arma3_500)
save(arma3_500, file = "arma3_500.rda")
load(file = "arma3_500.rda")
rm(l,l1,l2)

# Misspecify
setwd("C://Users//weeth//Documents//Stats Project//ARMA Misspecified")
#ARMA(1,2) data ARMA(1,1) Model
arma3_mis1_500 = matrix(nrow = 100, ncol = 19)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma3.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply( cl, 91:100, function(x) stan_sarima(arma3.sim3[,x], order = c(1,0,1), seasonal = c(0,0,0),
                                                  iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma3_mis1_500[91:100,1:14] = l1
arma3_mis1_500[91:100, 15:19] = l2

colnames(arma3_mis1_500) = c("ar_mean", "ma_mean", "ar_se_mean", "ma_se_mean", "ar_sd", "ma_sd",
                       "ar_2.5%", "ma_2.5%", "ar_97.5%", "ma_97.5%", "ar_n_eff", "ma2_n_eff",
                       "ar_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma3_mis1_500 = as.data.frame(arma3_mis1_500)
save(arma3_mis1_500, file = "arma3_mis1_500.rda")
load(file = "arma3_mis1_500.rda")
rm(l,l1,l2)

#ARMA(1,2) data ARMA(2,1) Model
arma3_mis2_500 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma3.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply( cl, 97:100, function(x) stan_sarima(arma3.sim3[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                                  iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                    probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma3_mis2_500[97:100,1:21] = l1
arma3_mis2_500[97:100, 22:26] = l2

colnames(arma3_mis2_500) = c("ar1_mean", "ar2_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                             "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                             "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma3_mis2_500 = as.data.frame(arma3_mis2_500)
save(arma3_mis2_500, file = "arma3_mis2_500.rda")
load(file = "arma3_mis2_500.rda")
rm(l,l1,l2)

#ARMA(2,1) data ARMA(1,1) Model
arma2_mis1_500 = matrix(nrow = 100, ncol = 19)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma2.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply( cl,1, function(x) stan_sarima(arma2.sim3[,x], order = c(1,0,1), seasonal = c(0,0,0),
                                                  iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

                                 
arma2_mis1_500[100,1:14] = l1
arma2_mis1_500[100, 15:19] = l2

colnames(arma2_mis1_500) = c("ar_mean", "ma_mean", "ar_se_mean", "ma_se_mean", "ar_sd", "ma_sd",
                             "ar_2.5%", "ma_2.5%", "ar_97.5%", "ma_97.5%", "ar_n_eff", "ma2_n_eff",
                             "ar_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_mis1_500 = as.data.frame(arma2_mis1_500)
save(arma2_mis1_500, file = "arma2_mis1_500.rda")
load(file = "arma2_mis1_500.rda")
rm(l,l1,l2)

#ARMA(2,1) data ARMA(1,2) Model
arma2_mis3_500 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma2.sim3'))
clusterSetRNGStream(cl, 123)

l = lapply( 99:100, function(x) stan_sarima(arma2.sim3[,x], order = c(1,0,2), seasonal = c(0,0,0),
                                                  iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]", "ma[2]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma2_mis3_500[99:100,1:21] = l1
arma2_mis3_500[99:100, 22:26] = l2

colnames(arma2_mis3_500) =  c("ar_mean", "ma1_mean","ma2_mean", "ar_se_mean","ma1_se_mean", "ma2_se_mean", "ar_sd","ma1_sd", "ma2_sd",
                              "ar_2.5%","ma1_2.5%", "ma2_2.5%", "ar_97.5%","ma1_97.5%", "ma2_97.5%", "ar_n_eff",  "ma1_n_eff", "ma2_n_eff",
                              "ar_Rhat", "ma1_Rhat", "ma2_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma2_mis3_500 = as.data.frame(arma2_mis3_500)
save(arma2_mis3_500, file = "arma2_mis3_500.rda")
load(file = "arma2_mis3_500.rda")
rm(l,l1,l2)

#parallelize it to save some time
#ARMA(1,1) 500
arma_mis3_500 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma1.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply(cl, 99:100, function(x) stan_sarima(arma1.sim3[,x], order = c(1,0,2), seasonal = c(0,0,0),
                                                  iter = 2000, chains = 2, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ma[1]", "ma[2]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma_mis3_500[99:100,1:21] = l1
arma_mis3_500[99:100, 22:26] = l2

colnames(arma_mis3_500) = c("ar_mean", "ma1_mean","ma2_mean", "ar_se_mean","ma1_se_mean", "ma2_se_mean", "ar_sd","ma1_sd", "ma2_sd",
                       "ar_2.5%","ma1_2.5%", "ma2_2.5%", "ar_97.5%","ma1_97.5%", "ma2_97.5%", "ar_n_eff",  "ma1_n_eff", "ma2_n_eff",
                       "ar_Rhat", "ma1_Rhat", "ma2_Rhat", "mean forecast 1", "mean forecast 2", "low 0.025 1","low 0.025 2", "high 0.025 1",
                       "high 0.025 2", "low 0.975 1",  "low 0.975 2", "high 0.975 1", "high 0.975 2")

arma_mis3_500 = as.data.frame(arma_mis3_500)
save(arma_mis3_500, file = "arma_mis3_500.rda")
load(file = "arma_500.rda")
rm(l,l1,l2)

#parallelize it to save some time
#ARMA(1,1)  arma(2,1) model 500
arma_mis2_500 = matrix(nrow = 100, ncol = 26)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('arma1.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply(cl, 97:100, function(x) stan_sarima(arma1.sim3[,x], order = c(2,0,1), seasonal = c(0,0,0),
                                                 iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ma[1]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 2, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

arma_mis2_500[97:100,1:21] = l1
arma_mis2_500[97:100, 22:26] = l2

colnames(arma_mis2_500) = c("ar1_mean", "ar2_mean","ma_mean", "ar1_se_mean","ar2_se_mean", "ma_se_mean", "ar1_sd","ar2_sd", "ma_sd",
                           "ar1_2.5%","ar2_2.5%", "ma_2.5%", "ar1_97.5%","ar2_97.5%", "ma_97.5%", "ar1_n_eff",  "ar2_n_eff", "ma2_n_eff",
                           "ar1_Rhat", "ar2_Rhat", "ma_Rhat", "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma_mis2_500 = as.data.frame(arma_mis2_500)
save(arma_mis2_500, file = "arma_mis2_500.rda")
load(file = "arma_500.rda")
rm(l,l1,l2)

n = 90
stan_105 = stan_sarima(arma3.sim3[,n], order = c(1,0,2), seasonal = c(0,0,0),
                       iter = 1000, chains = 2, seed = 123)

f = as.matrix(as.data.frame(forecast(object = stan_105, h = 2, probs = c(0.025, 0.975), 
                                     seed = 123)))
f = t(f)

arma3_500_t2[n, 1:2] = f[1,]
arma3_500_t2[n, 3:4] = f[2,]
arma3_500_t2[n, 5:6] = f[3,]
arma3_500_t2[n, 7:8] = f[4,]
arma3_500_t2[n, 9:10] = f[5,]

rm(stan_105, f)

summary(as.stan(stan_105), pars = c("ar[1]", "ar[2]", "ma[1]"),
        probs = c(0.025, 0.975))$summary
stan_505 = stan_sarima(arma2.sim3[,1], order = c(2,0,1), seasonal = c(0,0,0),
                       iter = 5000, chains = 4, seed = 123)
summary(as.stan(stan_505), pars = c("ar[1]", "ar[2]", "ma[1]"),
        probs = c(0.025, 0.975))$summary
arma2_df = data.frame(y = arma2.sim2)
brm_fit<- brm(y.1 ~arma(p =2, q = 1), data = arma2_df, iter = 5000, seed = 123)
summary(brm_fit)
