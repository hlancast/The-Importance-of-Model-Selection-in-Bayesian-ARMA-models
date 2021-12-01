## R Code for the plots at the begining of the Paper

library(ggplot2)

setwd(#Working Directory remvoved in Public Docs)
  
#Simulated AR for plots  
ar_mod1<- list(ar = c(0.8), order = c(1, 0, 0))
set.seed(123)
ar1<-arima.sim(n = 200, model = ar_mod1, mean = 0, sd = 1)

ar_mod2<- list(ar = c(0.8, -0.1), order = c(2, 0, 0))
set.seed(123)
ar2<- arima.sim(n = 50, model = ar_mod2, mean = 0, sd = 1)


par(mfrow = c(1,1))
plot(ar1, type = "l")
abline(h = 0, col ="red", lty = 2)
title(main = "AR[1] Time Series")
plot(ar2, type = "l")
abline(h = 0, col ="red", lty = 2)
title(main = "AR[2] Time Series")


#Simulated MAs for Plots
ma_mod1<- list(ma = c(0.3), order = c(0, 0, 1))
set.seed(123)
ma1<- arima.sim(n = 100, model = ma_mod1, mean = 0, sd = 1)

ma_mod2<- list(ma = c(0.3, -0.3), order = c(0, 0, 2))
set.seed(123)
ma2<- arima.sim(n = 100, model = ma_mod2, mean = 0, sd = 1)



par(mfrow = c(2,1))
plot(ma1, type = "l")
abline(h = 0, col ="red", lty = 2)
title(main = "MA[1] Time Series")
plot(ma2, type = "l")
abline(h = 0, col ="red", lty = 2)
title(main = "MA[2] Time Series")


#put into data frame for ggplots
df.plot<- data.frame(Y = as.matrix(ar1))
time<- seq(1,50,1)

df.plot<- cbind(df.plot,time)

p<- ggplot(df.plot, aes(x = time, y= Y)) +
  geom_line() +
  theme_minimal() + 
  ylab("Y_t") +
  ggtitle("AR[1] Process") +
  theme(plot.title = element_text(hjust = 0.5))

p

#fit AR1 in stan
stanar = "
data{
  int<lower=0>  p;        // order of ar process
  int<lower=0> N;       // Number of Observations 
  vector[N] y;            // obervations
  int<lower=0> n_new;    // forecast
  
}

transformed data{
  int<lower=0> x = N + n_new;
}


parameters{
  real alpha;               // constant
  vector[p] beta;           // coefficients
  real<lower=0> sigma;    // std dev of noise
}

transformed parameters{
  vector[N] mu;           // mean of dist
  
  mu[1:p] = y[1:p];       // first p points are known
  
  for(t in (p + 1):N){
    mu[t] = alpha;
    for(c in 1:p){
      mu[t] += beta[c] * y[t - c];  // generate the mean
    }
  }
  
}

model{
  alpha ~ normal(0,sqrt(10^4));     // prior for constant
  beta ~ normal(0,sqrt(10^4));      // prior for coeffs
  sigma ~ normal(0, sqrt(10));    // prior for noise
  
// data dist  
  for(i in 1:N){
    y[i] ~ normal(mu[i], sigma);
  }
}

generated quantities{
  vector[x] y_hat;                // posterior forecast
  vector[x] predict;              // posterior fitted values

  for(i in 1:x){
    if(i <= N){
    predict[i] = normal_rng(mu[i], sigma);        // generate fitted
    y_hat[i] = mu[i];
    } 
    else{
      y_hat[i] = alpha + beta[p] * predict[i - 1];   // generate posterior forecast
      predict[i] = normal_rng(y_hat[i], sigma); 
    }
  }
}
"


model_ar = stan_model(model_code = stanar)
#save(model_ar, file = "model_ar.rda", compress = "xz")
load(file = "model_ar.rda")


#fit stan model
y = ar1
n = length(y)
time_fit_data = list(y = y, N = n, n_new = 1, p = 1)
ar_fit = sampling(model_ar, data = time_fit_data, iter = 10000, chains = 4, seed = 123)

summary(ar_fit, pars = c("alpha","beta","sigma","y_hat[201]"), probs = c(0.025, 0.975))$summary

print(ar_fit, pars = c("alpha", "beta","sigma"))

stan_dens(ar_fit, par = c("alpha","beta", "sigma"), separate_chains = TRUE)

stan_trace(ar_fit, par = c("beta"))


ar_df = as.data.frame(ar1)
ar_fit_bf = stan_sarima(ar1, order = c(1,0,0), seasonal = c(0,0,0),
            iter = 5000, chains = 4, seed = 123) 

summary(as.stan(ar_fit_bf), pars =  c("mu0","ar","sigma0"), probs = c(0.025, 0.975))$summary
summary(ar_fit_bf)

fit1_df<- cbind(as.data.frame(ar_fit), model = "stan_fit")

## Desnity Plots in Paper
par(mfrow = c(1,3))
muplot<- ggplot(fit1_df, aes(x = alpha)) +
  geom_vline(xintercept = -0.07929008) +
  geom_density(aes(fill = model), alpha = 0.4) + theme_bw() +
  scale_fill_manual(values = "blue") +
  xlab(expression(mu)) +
  ggtitle(expression(paste("Estimated Posterior Density of ", mu))) +
  theme(plot.title = element_text(hjust = 0.5)) 

muplot+theme(legend.position="none")

phiplot<- ggplot(fit1_df, aes(x = fit1_df$`beta[1]`)) +
  geom_vline(xintercept = 0.78685) +
  geom_density(aes(fill = model), alpha = 0.4) + theme_bw() +
  scale_fill_manual(values = "blue") +
  xlab(expression(phi)) +
  ggtitle(expression(paste("Estimated Posterior Density of ", phi))) +
  theme(plot.title = element_text(hjust = 0.5))

phiplot+theme(legend.position="none")

sigmaplot<- ggplot(fit1_df, aes(x = sigma)) +
  geom_vline(xintercept = 1.068) +
  geom_density(aes(fill = model), alpha = 0.4) + theme_bw() +
  scale_fill_manual(values = "blue") +
  xlab(expression(mu)) +
  ggtitle(expression(paste("Estimated Posterior Density of ", sigma))) +
  theme(plot.title = element_text(hjust = 0.5))

sigmaplot+theme(legend.position="none")


forecastplot<- ggplot(fit1_df, aes(x = `y_hat[201]`)) +
  geom_vline(xintercept = -0.2515) +
  geom_density(aes(fill = model), alpha = 0.4) + theme_bw() +
  scale_fill_manual(values = "blue") +
  xlab("t+1") +
  ggtitle("Estimated Posterior Density of t+1 Forecast") +
  theme(plot.title = element_text(hjust = 0.5))

forecastplot+theme(legend.position="none")

ypred = extract(ar_fit, pars = "y_hat")[[1]]

hist(ypred[201])

#identify mean, median, and tails of posterior predictive distribution
post_mean<- apply(ypred, 2, mean)
post_median<- apply(ypred, 2, median)
post_lower<- apply(ypred, 2, quantile, probs = 0.025)
post_upper<- apply(ypred, 2, quantile, probs = 0.975)


#put into data frame for ggplots
df.plot<- data.frame(Y = as.matrix(ar1))
df.plot[nrow(df.plot)+1, ] <- NA


time<- seq(1,201,1)
plot(time, post_mean)

df.plot<- cbind(df.plot,time)

df.plot2<- data.frame(df.plot$time, post_mean, post_median, post_lower, post_upper)

#plot of time series v replicated values
p<- ggplot() + geom_ribbon(data = df.plot2, 
                           aes(x = df.plot.time, ymin = post_lower, ymax = post_upper, col = "Credible Interval"), fill = "skyblue") + 
  geom_line(data = df.plot2, aes(x = df.plot.time, y = post_mean, col = "post mean")) +
  geom_point(data = df.plot, 
             aes(x = time, y = Y, col = "Data")) + 
  scale_fill_manual(name = "", values=c("Credible Interval" = "skyblue", "post mean" = "NA", "Data" = NA)) +
  scale_colour_manual(name="", values=c("Credible Interval" = "skyblue", "post mean" = "purple", "Data" = "black")) +
  ggtitle("Forecast of AR(1) Process")
p

library(forecast)
freq_fit<- arima(ar1, order = c(1,0,0))
fitted<- fitted(freq_fit)
plot(ar1, col = "black")
lines(fitted, col = "red")
lines(post_mean, col = "blue")

par(mfrow = c(2,1))
plot(ar1)
lines(fitted, col = "red")
title(main = " AR series v fitted values")
plot(ar1)
lines(post_mean, col = "blue")

#Posterior Predictive check
yrep = extract(ar_fit, pars = "y_hat")[[1]]
yrep2 = yrep[701:1400,1:200]

ppc_hist(ar1, yrep[,1:200]
y = as.matrix(ar1)
ppc_dens_overlay(ar1, yrep2)

ar1_df = data.frame(y = ar1)
brm_fit<- brm(y ~arma(p =1, q = 0), data = ar1_df, iter = 10000, seed = 123)

summary(brm_fit)
pp_check(brm_fit, ndraws = 100)
stancode(brm_fit)


## posterior distribution checks  ##
#fit AR1 in stan
stanar_alt1 = "
data{
  int<lower=0>  p;        // order of ar process
  int<lower=0> N;       // Number of Observations 
  vector[N] y;            // obervations
  int<lower=0> n_new;    // forecast
  
}

transformed data{
  int<lower=0> x = N + n_new;
}


parameters{
  real alpha;               // constant
  vector[p] beta;           // coefficients
  real<lower=0> sigma;    // std dev of noise
}

transformed parameters{
  vector[N] mu;           // mean of dist
  
  mu[1:p] = y[1:p];       // first p points are known
  
  for(t in (p + 1):N){
    mu[t] = alpha;
    for(c in 1:p){
      mu[t] += beta[c] * y[t - c];  // generate the mean
    }
  }
  
}

model{

  
// data dist  
  for(i in 1:N){
    y[i] ~ normal(mu[i], sigma);
  }
}

generated quantities{
  vector[x] y_hat;                // posterior forecast
  vector[x] predict;              // posterior fitted values

  for(i in 1:x){
    if(i <= N){
    predict[i] = normal_rng(mu[i], sigma);        // generate fitted
    y_hat[i] = mu[i];
    } 
    else{
      y_hat[i] = alpha + beta[p] * predict[i - 1];   // generate posterior forecast
      predict[i] = normal_rng(y_hat[i], sigma); 
    }
  }
}
"


model_ar_alt1 = stan_model(model_code = stanar_alt1)
save(model_ar_alt1, file = "model_ar_alt1.rda", compress = "xz")
load(file = "model_ar_alt1.rda")


#fit stan model
y = ar1
n = length(y)
time_fit_data = list(y = y, N = n, n_new = 1, p = 1)
ar_fit = sampling(model_ar_alt1, data = time_fit_data, iter = 10000, chains = 4, seed = 123)

summary(ar_fit, pars = c("alpha","beta","sigma","y_hat[201]"), probs = c(0.025, 0.975))$summary

print(ar_fit, pars = c("alpha", "beta","sigma"))

stan_dens(ar_fit, par = c("alpha","beta", "sigma"), separate_chains = TRUE)

stan_trace(ar_fit, par = c("beta"))

