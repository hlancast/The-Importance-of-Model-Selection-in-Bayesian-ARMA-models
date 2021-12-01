

#Simulate AR Processes
# AR[1]

ar_mod1<- list(ar = c(0.8), order = c(1, 0, 0))
set.seed(123)
ar1.sim1<- replicate(100,arima.sim(n = 50, model = ar_mod1, mean = 0, sd = 1))
set.seed(123)
ar1.sim2<- replicate(100,arima.sim(n = 150, model = ar_mod1, mean = 0, sd = 1))
set.seed(123)
ar1.sim3<- replicate(100,arima.sim(n = 500, model = ar_mod1, mean = 0, sd = 1))

#AR[3]
ar_mod2<- list(ar = c(0.8, -0.5, -0.1), order = c(3, 0, 0))
set.seed(123)
ar2.sim1<- replicate(100,arima.sim(n = 50, model = ar_mod2, mean = 0, sd = 1))
set.seed(43)
ar2.sim2<- replicate(100,arima.sim(n = 150, model = ar_mod2, mean = 0, sd = 1))
set.seed(44)
ar2.sim3<- replicate(100,arima.sim(n = 500, model = ar_mod2, mean = 0, sd = 1))

#AR[4]
ar_mod3<- list(ar = c(0.8, -0.5, -0.1, 0.2), order = c(4, 0, 0))
set.seed(7)
ar3.sim1<- replicate(100,arima.sim(n = 50, model = ar_mod3, mean = 0, sd = 1))
set.seed(71)
ar3.sim2<- replicate(100,arima.sim(n = 150, model = ar_mod3, mean = 0, sd = 1))
set.seed(712)
ar3.sim3<- replicate(100,arima.sim(n = 500, model = ar_mod3, mean = 0, sd = 1))

#fit process to true model 
#ar[1]
ar1_fit<- function(df, x){
  
  results_ar1.sim1 = matrix(nrow = x, ncol = 12)
  colnames(results_ar1.sim1) = c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat", 
                                 "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")
  
  for(i in 1:x){
    y = df[,i]
    
    sfit =  stan_sarima(y, order = c(1,0,0), seasonal = c(0,0,0),
                        iter = 5000, chains = 4, seed = 123)
    results_ar1.sim1[i,1:7] = summary(as.stan(sfit), pars = c("ar[1]"), probs = c(0.025, 0.975))$summary
    
    results_ar1.sim1[i,8:12] = as.matrix(as.data.frame(forecast(object = sfit, h = 1, probs = c(0.025, 0.975), 
                                                                seed = 123)))
    
    rm(y, sfit)
    gc()
  
  }
  
  return(results_ar1.sim1)

}

a<- ar1_fit(df = ar1.sim1, x = 100)
b<- ar1_fit(df = ar1.sim2, x = 100)
ar1_500<- ar1_fit(df = ar1.sim3, x = 100)

stan_mod = stan_sarima(ar1.sim3[,92], order = c(1,0,0), seasonal = c(0,0,0),
            iter = 7000, chains = 2, seed = 123, prior_ar = normal(1,0.08), prior_sigma0 = gamma(0.1,0.1))
m = summary(as.stan(stan_mod), pars = c("ar[1]"), probs = c(0.025, 0.975))$summary

ar1_500[92,1:7] = m[1,1:7]
rm(stan_mod,m)
gc()

#AR[3]
ar3_fit<- function(df, x){
  
  results_ar1.sim1 = matrix(nrow = x, ncol = 26)
  colnames(results_ar1.sim1) = c("ar1_mean", "ar1_se_mean", "ar1_sd", "ar1_2.5%", "ar1_97.5%", "ar1_n_eff", "ar1_Rhat",
                                 "ar2_mean", "ar2_se_mean", "ar2_sd", "ar2_2.5%", "a2_97.5%", "ar2_n_eff", "ar2_Rhat",
                                 "ar3_mean", "ar3_se_mean", "ar3_sd", "ar3_2.5%", "ar3_97.5%", "ar3_n_eff", "ar3_Rhat",
                                 "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")
  
  for(i in 1:x){
    y = df[,i]
    
    sfit =  stan_sarima(y, order = c(3,0,0), seasonal = c(0,0,0),
                        iter = 5000, chains = 4, seed = 123)
    m = summary(as.stan(sfit), pars = c("ar[1]","ar[2]","ar[3]"),
                probs = c(0.025, 0.975))$summary
    results_ar1.sim1[i,1:7] = m[1, 1:7]
    results_ar1.sim1[i,8:14] = m[2, 1:7]
    results_ar1.sim1[i,15:21] = m[3, 1:7]
    
    results_ar1.sim1[i,22:26] = as.matrix(as.data.frame(forecast(object = sfit, h = 1, probs = c(0.025, 0.975), 
                                                                seed = 123)))
    
    rm(y, sfit,m)
    gc()
    
  }
  
  return(results_ar1.sim1)
  
}

#AR[4]
ar4_fit<- function(df, x){
  
  results_ar1.sim1 = matrix(nrow = x, ncol = 33)
  colnames(results_ar1.sim1) = c("ar1_mean", "ar1_se_mean", "ar1_sd", "ar1_2.5%", "ar1_197.5%", "ar1_n_eff", "ar1_Rhat",
                                 "ar2_mean", "ar2_se_mean", "ar2_sd", "ar2_2.5%", "a2_197.5%", "ar2_n_eff", "ar2_Rhat",
                                 "ar3_mean", "ar3_se_mean", "ar3_sd", "ar3_2.5%", "ar3_197.5%", "ar3_n_eff", "ar3_Rhat",
                                 "ar4_mean", "ar4_se_mean", "ar4_sd", "ar4_2.5%", "ar4_197.5%", "ar4_n_eff", "ar4_Rhat",
                                 "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")
  
  for(i in 1:x){
    y = df[,i]
    
    sfit =  stan_sarima(y, order = c(4,0,0), seasonal = c(0,0,0),
                        iter = 5000, chains = 4, seed = 123)
    m = summary(as.stan(sfit), pars = c("ar[1]","ar[2]","ar[3]", "ar[4]"),
                probs = c(0.025, 0.975))$summary
    results_ar1.sim1[i,1:7] = m[1, 1:7]
    results_ar1.sim1[i,8:14] = m[2, 1:7]
    results_ar1.sim1[i,15:21] = m[3, 1:7]
    results_ar1.sim1[i,22:28] = m[4, 1:7]
    
    results_ar1.sim1[i,29:33] = as.matrix(as.data.frame(forecast(object = sfit, h = 1, probs = c(0.025, 0.975), 
                                                                seed = 123)))
    
    rm(y, sfit, m)
    gc()
    
  }
  
  return(results_ar1.sim1)
  
}


d<- ar3_fit(df = ar2.sim1, x = 100)
e<- ar3_fit(df = ar2.sim2, x = 100)
f<- ar3_fit(df = ar2.sim3, x = 100)
 
g<- ar4_fit(df = ar3.sim1, x = 100)
h<- ar4_fit(df = ar3.sim2, x = 100)
i<- ar4_fit(df = ar3.sim3, x = 100)

a = as.data.frame(a)
b = as.data.frame(b)
c = as.data.frame(c)

d = as.data.frame(d)
e = as.data.frame(e)
f = as.data.frame(f)

g = as.data.frame(g)
h = as.data.frame(h)
i = as.data.frame(i)

save(a, file = "ar1.rda")
save(b, file = "ar3.rda")
save(c, file = "ar4.rda")

save(d, file = "ar41.rda")
save(e, file = "ar5.rda")
save(f, file = "ar6.rda")

save(g, file = "ar7.rda")
save(h, file = "ar8.rda")
save(i, file = "ar9.rda")

load(file = "ar4.rda")

hist(d$ar1_mean)
hist(e$ar1_mean)
hist(f$ar1_mean)

# fit misspecified processes 

#fit AR[3] data to AR[1] Model

j<- ar1_fit(df = ar2.sim1, x = 100)
k<- ar1_fit(df = ar2.sim2, x = 100)
l<- ar1_fit(df = ar2.sim3, x = 100)

j = as.data.frame(j)
k = as.data.frame(k)
l = as.data.frame(l)


save(j, file = "arm1_50.rda")
save(k, file = "arm1_150.rda")
save(l, file = "arm1.rda")

#fit Ar[3] Data to Ar[4] Model

m<- ar4_fit(df = ar2.sim1, x = 100)
n<- ar4_fit(df = ar2.sim2, x = 100)
o<- ar4_fit(df = ar2.sim3, x = 100)

m = as.data.frame(m)
n = as.data.frame(n)
o = as.data.frame(o)

save(m, file = "arm2_50.rda")
save(n, file = "arm2_150.rda")
save(o, file = "arm2_5001.rda")

hist(n$ar1_mean)
hist(o$ar1_mean)

forc_val = matrix(nrow = 100, ncol = 5)


l =  lapply( 1:10, function(n)  stan_sarima(ar1.sim3[,n], order = c(1,0,0), seasonal = c(0,0,0),
                    iter = 3000, chains = 2, seed = 123))
f = sapply(l, function(x)  as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                      seed = 123))))


f<-t(f)
ar1_500[1:10, 8:12] = f
#AR(4) to AR(1) Model 50

setwd("C://Users//weeth//Documents//Stats Project//AR Misspecified")
ar_mis4_50 = matrix(nrow = 10, ncol = 12)
l = lapply(11:100, function(x) stan_sarima(ar3.sim1[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_mis4_50[11:100,1:7] = l1
ar_mis4_50[11:100, 8:12] = l2

colnames(ar_mis4_50) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                           "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar_mis4_50, file = "ar_mis4_50.rda")
load(file = "ar_mis4_150.rda")
rm(l,l1,l2)
rm(ar_mis4_50)

#AR(4) to AR(1) model - 150 
ar_mis4_150 = matrix(nrow = 100, ncol = 12)
l = lapply(76:100, function(x) stan_sarima(ar3.sim2[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_mis4_150[76:100,1:7] = l1
ar_mis4_150[76:100, 8:12] = l2

colnames(ar_mis4_150) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                         "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar_mis4_150, file = "ar_mis4_150.rda")
load(file = "ar_mis4_150.rda")
rm(l,l1,l2)

#AR(4) to AR(1) model 500
ar_mis4_500 = matrix(nrow = 10, ncol = 12)
l = lapply(97:98, function(x) stan_sarima(ar3.sim3[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_mis4_500[97:98,1:7] = l1
ar_mis4_500[97:98, 8:12] = l2

colnames(ar_mis4_500) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                         "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar_mis4_500, file = "ar_mis4_500.rda")
load(file = "ar_mis4_500.rda")
rm(l,l1,l2)

## AR(3) to AR(4) data ##
ar3_mis4_50 = matrix(nrow = 100, ncol = 26)
l = lapply(1:100, function(x) stan_sarima(ar3.sim1[,x], order = c(3,0,0), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar3_mis4_50[1:100,1:21] = l1
ar3_mis4_50[1:100, 22:26] = l2

colnames(ar3_mis4_50) = c("ar1_mean","ar2_mean","ar3_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean", "ar1_sd",
                          "ar2_sd", "ar3_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%", "ar1_97.5%","ar2_97.5%", "ar3_97.5%",
                          "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat", "mean forecast", 
                          "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar3_mis4_50, file = "ar3_mis4_50.rda")
load(file = "ar3_mis4_50.rda")
rm(l,l1,l2)

ar3_mis4_150 = matrix(nrow = 100, ncol = 26)
l = lapply(51:100, function(x) stan_sarima(ar3.sim2[,x], order = c(3,0,0), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar3_mis4_150[51:100,1:21] = l1
ar3_mis4_150[51:100, 22:26] = l2

colnames(ar3_mis4_150) = c("ar1_mean","ar2_mean","ar3_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean", "ar1_sd",
                          "ar2_sd", "ar3_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%", "ar1_97.5%","ar2_97.5%", "ar3_97.5%",
                          "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat", "mean forecast", 
                          "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar3_mis4_150, file = "ar3_mis4_150.rda")
load(file = "ar3_mis3_150.rda")
rm(l,l1,l2)

ar3_mis4_500 = matrix(nrow = 100, ncol = 26)
l = lapply(65:70, function(x) stan_sarima(ar3.sim3[,x], order = c(3,0,0), seasonal = c(0,0,0),
                                         iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar3_mis4_500[41:55,1:21] = l1
ar3_mis4_500[41:55, 22:26] = l2

colnames(ar3_mis4_500) = c("ar1_mean","ar2_mean","ar3_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean", "ar1_sd",
                           "ar2_sd", "ar3_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%", "ar1_97.5%","ar2_97.5%", "ar3_97.5%",
                           "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat", "mean forecast", 
                           "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar3_mis4_500, file = "ar3_mis4_500.rda")
load(file = "ar3_mis4_500.rda")
rm(l,l1,l2)

## AR(1) to AR(3) data ##
ar_mis3_50 = matrix(nrow = 100, ncol = 12)
l = lapply(1:100, function(x) stan_sarima(ar2.sim1[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_mis3_50[1:100,1:7] = l1
ar_mis3_50[1:100, 8:12] = l2

colnames(ar_mis3_50) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                          "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar_mis3_50 = as.data.frame(ar_mis3_50)
save(ar_mis3_50, file = "ar_mis3_50.rda")
load(file = "ar_mis3_50.rda")
rm(l,l1,l2)

## AR(1) to AR(3) data 150 ##
ar_mis3_150 = matrix(nrow = 100, ncol = 12)
l = lapply(51:100, function(x) stan_sarima(ar2.sim2[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_mis3_150[51:100,1:7] = l1
ar_mis3_150[51:100, 8:12] = l2

colnames(ar_mis3_150) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                         "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar_mis3_150 = as.data.frame(ar_mis3_150)
save(ar_mis3_150, file = "ar_mis3_150.rda")
load(file = "ar_mis3_150.rda")
rm(l,l1,l2)

## AR(1) to AR(3) data 500 ##
ar_mis3_500 = matrix(nrow = 100, ncol = 12)
l = lapply(95:100, function(x) stan_sarima(ar2.sim3[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)
l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_mis3_500[95:100,1:7] = l1
ar_mis3_500[95:100, 8:12] = l2

colnames(ar_mis3_500) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                          "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar_mis3_500 = as.data.frame(ar_mis3_500)
save(ar_mis3_500, file = "ar_mis3_500.rda")
load(file = "ar_mis3_500.rda")
rm(l,l1,l2)

## over fitting
#AR(3) model AR(1) data

ar3_overfit1_50 = matrix(nrow = 100, ncol = 26)
l = lapply(1:100, function(x) stan_sarima(ar1.sim1[,x], order = c(3,0,0), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar3_overfit1_50[1:100,1:21] = l1
ar3_overfit1_50[1:100, 22:26] = l2

colnames(ar3_overfit1_50) = c("ar1_mean","ar2_mean","ar3_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean", "ar1_sd",
                           "ar2_sd", "ar3_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%", "ar1_97.5%","ar2_97.5%", "ar3_97.5%",
                           "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat", "mean forecast", 
                           "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar3_overfit1_50, file = "ar3_overfit1_50.rda")
load(file = "ar3_overfit1_50.rda")
rm(l,l1,l2)

#AR(3) model AR(1) data 150

ar3_overfit1_150 = matrix(nrow = 100, ncol = 26)
l = lapply(51:100, function(x) stan_sarima(ar1.sim2[,x], order = c(3,0,0), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar3_overfit1_150[51:100,1:21] = l1
ar3_overfit1_150[51:100, 22:26] = l2

colnames(ar3_overfit1_150) = c("ar1_mean","ar2_mean","ar3_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean", "ar1_sd",
                              "ar2_sd", "ar3_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%", "ar1_97.5%","ar2_97.5%", "ar3_97.5%",
                              "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat", "mean forecast", 
                              "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar3_overfit1_150, file = "ar3_overfit1_150.rda")
load(file = "ar3_overfit1_50.rda")
rm(l,l1,l2)

#AR(3) model AR(1) data 500

ar3_overfit1_500 = matrix(nrow = 100, ncol = 26)
l = lapply(91:100, function(x) stan_sarima(ar1.sim[,x], order = c(3,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar3_overfit1_500[91:100,1:21] = l1
ar3_overfit1_500[91:100, 22:26] = l2

colnames(ar3_overfit1_500) = c("ar1_mean","ar2_mean","ar3_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean", "ar1_sd",
                               "ar2_sd", "ar3_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%", "ar1_97.5%","ar2_97.5%", "ar3_97.5%",
                               "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat", "mean forecast", 
                               "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar3_overfit1_500 = as.data.frame(ar3_overfit1_500)
save(ar3_overfit1_500, file = "ar3_overfit1_500.rda")
load(file = "ar3_overfit1_500.rda")
rm(l,l1,l2)

#AR(4) model AR(1) data 50

ar4_overfit1_50 = matrix(nrow = 100, ncol = 32)
l = lapply(1:100, function(x) stan_sarima(ar1.sim1[,x], order = c(4,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]","ar[4]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar4_overfit1_50[1:100,1:28] = l1
ar4_overfit1_50[1:100, 29:32] = l2

colnames(ar4_overfit1_50) = c("ar1_mean","ar2_mean","ar3_mean","ar4_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean","ar4_se_mean",
                               "ar1_sd", "ar2_sd", "ar3_sd", "ar4_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%","ar4_2.5", "ar1_97.5%","ar2_97.5%",
                               "ar3_97.5%", "ar4_97.5", "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar4_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat",
                               "ar4_Rhat","mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar4_overfit1_50, file = "ar4_overfit1_50.rda")
load(file = "ar4_overfit1_50.rda")
rm(l,l1,l2)
ar4_overfit1_50 =ar4_overfit1_150


#AR(4) model AR(1) data 150

ar4_overfit1_150 = matrix(nrow = 100, ncol = 33)
l = lapply(1:50, function(x) stan_sarima(ar1.sim2[,x], order = c(4,0,0), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]","ar[4]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar4_overfit1_150[1:50,1:28] = l1
ar4_overfit1_150[1:50, 29:33] = l2

colnames(ar4_overfit1_150) = c("ar1_mean","ar2_mean","ar3_mean","ar4_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean","ar4_se_mean",
                              "ar1_sd", "ar2_sd", "ar3_sd", "ar4_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%","ar4_2.5", "ar1_97.5%","ar2_97.5%",
                              "ar3_97.5%", "ar4_97.5", "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar4_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat",
                              "ar4_Rhat","mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

save(ar4_overfit1_150, file = "ar4_overfit1_150.rda")
load(file = "ar4_overfit1_150.rda")
rm(l,l1,l2)

#AR(4) model AR(1) data 500

ar4_overfit1_500 = matrix(nrow = 100, ncol = 33)
l = lapply(91:100, function(x) stan_sarima(ar1.sim3[,x], order = c(4,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))

l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]","ar[4]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar4_overfit1_500[91:100,1:28] = l1
ar4_overfit1_500[91:100, 29:33] = l2

colnames(ar4_overfit1_500) = c("ar1_mean","ar2_mean","ar3_mean","ar4_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean","ar4_se_mean",
                               "ar1_sd", "ar2_sd", "ar3_sd", "ar4_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%","ar4_2.5", "ar1_97.5%","ar2_97.5%",
                               "ar3_97.5%", "ar4_97.5", "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar4_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat",
                               "ar4_Rhat","mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar4_overfit1_500 = as.data.frame(ar4_overfit1_500)
save(ar4_overfit1_500, file = "ar4_overfit1_500.rda")
load(file = "ar4_overfit1_500.rda")
rm(l,l1,l2)

#AR(4) model AR(3) data 50

ar4_overfit3_50 = matrix(nrow = 100, ncol = 33)
l = lapply(1:100, function(x) stan_sarima(ar2.sim1[,x], order = c(4,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))


l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]","ar[4]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar4_overfit3_50[1:100,1:28] = l1
ar4_overfit3_50[1:100, 29:33] = l2

colnames(ar4_overfit3_50) = c("ar1_mean","ar2_mean","ar3_mean","ar4_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean","ar4_se_mean",
                               "ar1_sd", "ar2_sd", "ar3_sd", "ar4_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%","ar4_2.5", "ar1_97.5%","ar2_97.5%",
                               "ar3_97.5%", "ar4_97.5", "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar4_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat",
                               "ar4_Rhat","mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar4_overfit3_50 = as.data.frame(ar4_overfit3_50)
save(ar4_overfit3_50, file = "ar4_overfit3_50.rda")
load(file = "ar4_overfit1_150.rda")
rm(l,l1,l2)

#AR(4) model AR(3) data 150

ar4_overfit3_150 = matrix(nrow = 100, ncol = 33)
l = lapply(51:100, function(x) stan_sarima(ar2.sim2[,x], order = c(4,0,0), seasonal = c(0,0,0),
                                          iter = 5000, chains = 4, seed = 123))


l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]","ar[4]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar4_overfit3_150[51:100,1:28] = l1
ar4_overfit3_150[51:100, 29:33] = l2

colnames(ar4_overfit3_150) = c("ar1_mean","ar2_mean","ar3_mean","ar4_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean","ar4_se_mean",
                              "ar1_sd", "ar2_sd", "ar3_sd", "ar4_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%","ar4_2.5", "ar1_97.5%","ar2_97.5%",
                              "ar3_97.5%", "ar4_97.5", "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar4_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat",
                              "ar4_Rhat","mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar4_overfit3_150 = as.data.frame(ar4_overfit3_150)
save(ar4_overfit3_150, file = "ar4_overfit3_150.rda")
load(file = "ar4_overfit1_150.rda")
rm(l,l1,l2)

#AR(4) model AR(3) data 500

ar4_overfit3_500 = matrix(nrow = 100, ncol = 33)
l = lapply(91:100, function(x) stan_sarima(ar2.sim3[,x], order = c(4,0,0), seasonal = c(0,0,0),
                                           iter = 5000, chains = 4, seed = 123))


l1<- sapply(l, function (x) summary(as.stan(x), pars = c("ar[1]", "ar[2]", "ar[3]","ar[4]"),
                                    probs = c(0.025, 0.975))$summary)
l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar4_overfit3_500[91:100,1:28] = l1
ar4_overfit3_500[91:100, 29:33] = l2

colnames(ar4_overfit3_500) = c("ar1_mean","ar2_mean","ar3_mean","ar4_mean",  "ar1_se_mean","ar2_se_mean","ar3_se_mean","ar4_se_mean",
                               "ar1_sd", "ar2_sd", "ar3_sd", "ar4_sd", "ar1_2.5%","ar2_2.5%", "ar3_2.5%","ar4_2.5", "ar1_97.5%","ar2_97.5%",
                               "ar3_97.5%", "ar4_97.5", "ar1_n_eff", "ar2_n_eff", "ar3_n_eff","ar4_n_eff","ar1_Rhat", "ar2_Rhat", "ar3_Rhat",
                               "ar4_Rhat","mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

ar4_overfit3_500 = as.data.frame(ar4_overfit3_500)
save(ar4_overfit3_500, file = "ar4_overfit3_500.rda")
load(file = "ar4_overfit3_500.rda")
rm(l,l1,l2)

#parallelize it to save some time
#ARMA(1,1)  arma(2,1) model 500
ar_500 = matrix(nrow = 100, ncol = 12)

ncpus = parallel::detectCores()-1
cl = makeCluster(ncpus, type="PSOCK")
clusterEvalQ(cl, library(bayesforecast))
clusterEvalQ(cl, library(rstan))
clusterExport(cl = cl , varlist = c('ar1.sim3'))
clusterSetRNGStream(cl, 123)

l = parLapply(cl, 46:60, function(x) stan_sarima(ar1.sim3[,x], order = c(1,0,0), seasonal = c(0,0,0),
                                                  iter = 5000, chains = 4, seed = 123)) 

l1<- parSapply(cl,l, function (x) summary(as.stan(x), pars = c("ar[1]"),
                                          probs = c(0.025, 0.975))$summary)

stopCluster(cl)

l1<- t(l1)

l2<- sapply(l, function(x) as.matrix(as.data.frame(forecast(object = x, h = 1, probs = c(0.025, 0.975), 
                                                            seed = 123))))
l2<- t(l2)

ar_500[46:60,1:7] = l1
ar_500[46:60, 8:12] = l2

colnames(ar_500) = c("ar_mean", "ar_se_mean", "ar_sd", "ar_2.5%", "ar_97.5%", "ar_n_eff", "ar_Rhat",
                            "mean forecast", "low 0.025", "high 0.025", "low 0.975", "high 0.975")

arma_mis2_500 = as.data.frame(arma_mis2_500)
save(ar_500, file = "ar_500.rda")
load(file = "ar_500.rda")
rm(l,l1,l2)

n = 95 
ar_fit = stan_sarima(ar1.sim3[,n], order = c(1,0,0), seasonal = c(0,0,0),
                     iter = 2000, chains = 2, seed = 123)
summary(ar_fit)
ar1_500[95,1] = 0.7592
