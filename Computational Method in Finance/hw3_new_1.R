# hW3 new
# Dong WOo Kim
# FE 621

rm(list = ls())

# 1
# a)
bs<-function(S,T,K,r,sigma,Greek)
{
  if(Greek=="Price" && T>0)
  {
    d1<-(log(S/K)+(r+sigma^2/2)*T) / (sigma*sqrt(T))
    d2<-d1 - sigma * sqrt(T)
    return(S * pnorm(d1, mean=0, sd=1) - K * exp(-r*T) * pnorm(d2, mean=0, sd=1))
  }
  if(Greek=="Price" && T==0){
    return(pmax(S-K,0))
    }
  if(Greek=="Delta" && T>0)
  {
    d1<-(log(S/K)+(r+sigma^2/2)*T) / (sigma*sqrt(T))
    return(pnorm(d1, mean=0, sd=1))
  }
  if(Greek=="Delta" && T==0){
    return(0)
  }
}

# Parameter
S_0<-50
T<-0.25
K<-50
r<-0.05
mu<-0.1
sigma<-0.3

N<-100000
steps_daily<-63 # daily rebalancing

dt<-T/steps_daily
set.seed(123456)

# Results
S_t<-rep(0, times=N)
Payoff<-rep(0, times=N)
Hedge<-rep(0, times=N)
Error<-rep(0, times=N)


for(j in 1:N) 
{
  S<-S_0
  
  # Delta Hedge
  V<-bs(S,T,K,r,sigma,"Price") # initial investment
  a<-bs(S,T,K,r,sigma,"Delta") # delta
  b<-V-a*S # money in bank
  
  
  for(i in 1:steps_daily) 
  {
    Z <- rnorm(1) # normal random
    S<-S*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*Z) # simulation for stock price
    
    # Delta Hedge
    V<-a*S+b*exp(r*dt)
    a<-bs(S,T-i*dt,K,r,sigma,"Delta")
    b<-V-a*S
  }
  
  S_t[j]<-S
  Hedge[j]<-V
  Payoff[j]<-bs(S,0,K,r,sigma,"Price")
}

# Hedging error
Error<-Payoff-Hedge
mean(Error)
sd(Error)


# Plot Hist Error
hist(Error, breaks=nclass.FD(Error), freq=F, col="gray", border="black", main='daily rebalancing', xlab='Heding Error', ylab='frequencies') 

# RMSE

RMSE <- sqrt(mean(Error)^2+sd(Error)^2)
x <- c(1/252, 1/52)
y <- c(0.325406, 0.69663)

plot(log(x),log(y),type = 'l')
slope <- (log(0.69663)-log(0.325406))/(log(1/52)-log(1/252))
