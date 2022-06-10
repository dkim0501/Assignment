# FE621
# HW 1
# Dong Woo Kim

rm(list = ls())

# 1
# a)
library(quantmod)
SPX.option <- getOptionChain("^SPX",NULL) #download option data

# b)
getSymbols("^IRX", from="2022-01-31", to="2022-02-09")

# c)
bs.call <- function(S0, K, T1, sigma, r){ # bs call option pricing
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2)
}

bs.put <- function(S0, K, T1, sigma, r){ # bs put option pricing
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  exp(-r*T1)*K*pnorm(-d2) - S0*pnorm(-d1)
}

# d)
bisection.new <- function(f,a,b,tol=0.001,N.max=100){ # Generate bisection method
  f.a <- f(a)
  f.b <- f(b)
  if(is.na(f.a*f.b)||f.a*f.b>0){
    return(NA)
  } else if(f.a==0){
    return(a)
  } else if(f.b==0){
    return(b)
  }
  
  for(n in 1:N.max){
    c<-(a+b)/2
    f.c <- f(c)
    if(f.c==0||abs(b-a)<tol){
      break
    }
    if(f.a*f.c<0){
      b<-c
      f.b <- f.c
    } else{
      a<-c
      f.a <- f.c
    }
  }
  return(c)
}

implied.vol.call <- function(S0, K, T1, r, price){ # implied vol, call option
  price.diff <- function(sigma)bs.call(S0, K, T1, sigma, r) - price
  return(bisection.new(price.diff, 0.01,5))
}

implied.vol.put <- function(S0, K, T1, r, price){ # implied vol, put option
  price.diff <- function(sigma)bs.put(S0,K,T1,sigma,r) - price
  return(bisection.new(price.diff,0.01,5))
}

# e) f)
SPX.options.all <- getOptionChain("^SPX",NULL) #download option data
SPX.expiration <- names(SPX.options.all)# all expiration dates
T.vec <- (as.Date(SPX.expiration,"%b.%d.%Y")-Sys.Date())/365
T.vec <- as.numeric(T.vec)
SPX.S0 <- getQuote("^SPX")$Last
r <- 0.275 * 0.01 # from 13 week Treasury bill

calc <- function(x, T1){ # add a column of implied vol and price for each expiration date (call option)
  # add a column of price
  x$calls$Price <- 0.5*(x$calls$Bid + x$calls$Ask)
  # add a column of implied volatility
  func <- function(K,P)implied.vol.call(SPX.S0,K,T1,r,P)
  x$calls$impliedVol <- mapply(func, x$calls$Strike, x$calls$Price)
  # delete columns
  x$calls <- x$calls[c("Bid","Ask","Strike","Price","impliedVol")]
    return(x)
}

SPX.options.all <- mapply(calc, SPX.options.all, T.vec, SIMPLIFY = FALSE) # print SPY option price and implied vol


plot(NA, xlim = c(4200,5600), ylim = c(0.1,0.3), xlab = "Strike", ylab = "ImpliedVol") # plot of volatility smile, 3 exp date
lines(SPX.options.all[[10]]$calls$Strike, SPX.options.all[[10]]$calls$impliedVol,col = "red")
lines(SPX.options.all[[18]]$calls$Strike, SPX.options.all[[18]]$calls$impliedVol,col = "green")
lines(SPX.options.all[[21]]$calls$Strike, SPX.options.all[[21]]$calls$impliedVol,col = "blue")
abline(v=4572.53,col="black",lty=2)
legend("bottomleft", SPX.expiration[c(10,18,21)], fill = c("red","green","blue"))



calc.put <- function(x,T1){ # put option implied vol
  # add a column of price
  x$puts$Price <- 0.5*(x$puts$Bid + x$puts$Ask)
  # add a column of implied volatility
  func <- function(K,P)implied.vol.put(SPX.S0,K,T1,r,P)
  x$puts$impliedVol <- mapply(func, x$puts$Strike, x$puts$Price)
  # delete columns
  x$puts <- x$puts[c("Bid","Ask","Strike","Price","impliedVol")]
  return(x)
}

SPX.options.all <- mapply(calc.put, SPX.options.all, T.vec, SIMPLIFY = FALSE) # print SPY option price and implied vol


plot(NA, xlim = c(4200,5600), ylim = c(0.1,0.3), xlab = "Strike", ylab = "ImpliedVol") # plot of volatility smile, 3 exp date
lines(SPX.options.all[[10]]$puts$Strike, SPX.options.all[[10]]$puts$impliedVol,col = "red") 
lines(SPX.options.all[[18]]$puts$Strike, SPX.options.all[[18]]$puts$impliedVol,col = "green")
lines(SPX.options.all[[21]]$puts$Strike, SPX.options.all[[21]]$puts$impliedVol,col = "blue")
abline(v=4572.53,col="black",lty=2)
legend("bottomleft", SPX.expiration[c(10,18,21)], fill = c("red","green","blue"))


# comparing with download data
plot(NA, xlim = c(4200,5600), ylim = c(0.1,0.3), xlab = "Strike", ylab = "ImpliedVol")
lines(SPX.options.all[[18]]$calls$Strike, SPX.options.all[[18]]$calls$impliedVol,col = "green")
lines(SPX.option[[18]]$calls$Strike, SPX.option[[18]]$calls$IV,col="purple")
legend("bottomleft", c("calculated_call","downloaded_call"), fill = c("green","purple"))


plot(NA, xlim = c(4200,5600), ylim = c(0,0.5), xlab = "Strike", ylab = "ImpliedVol")
lines(SPX.options.all[[18]]$puts$Strike, SPX.options.all[[18]]$puts$impliedVol,col = "red")
lines(SPX.option[[18]]$puts$Strike, SPX.option[[18]]$puts$IV,col="blue")
legend("bottomleft", c("calculated_put","downloaded_put"), fill = c("red","blue"))

# g)
# C = P + S0 - X*exp(-rt)

# exp 03
call.0302 <- NULL
SPX.strike.0302 <- SPX.option[[10]]$puts$Strike
put.0302 <- SPX.options.all[[10]]$puts$Price
t.0302 <- 22/365
call.real <- SPX.options.all[[10]]$calls$Price

for (i in 1:nrow(SPX.option[[10]]$puts)){
  call.0302[i] <- put.0302[i] + SPX.S0 - SPX.strike.0302[i]*exp(-r*t.0302)
}
plot(NA, xlim=c(0,100), ylim=c(0,1000),main="03-02 call option price",xlab="put",ylab="call")
lines(put.0302,call.0302,col="blue")
lines(put.0302[1:149],SPX.options.all[[10]]$calls$Price,col="red")
legend("bottomright",c("computed call","downloaded call"),fill=c("blue","red"))

# exp 0429
call.0429 <- NULL
SPX.strike.0429 <- SPX.option[[18]]$puts$Strike
put.0429 <- SPX.options.all[[18]]$puts$Price
t.0429 <- 80/365
call.real2 <- SPX.options.all[[18]]$calls$Price

for (i in 1:nrow(SPX.option[[18]]$puts)){
  call.0429[i] <- put.0429[i] + SPX.S0 - SPX.strike.0429[i]*exp(-r*t.0429)
}
plot(NA, xlim=c(0,100), ylim=c(0,1000),main="04-29 call option price",xlab="put",ylab="call")
lines(put.0429,call.0429,col="blue")
lines(put.0429[1:295],SPX.options.all[[18]]$calls$Price,col="red")
legend("bottomright",c("computed call","downloaded call"),fill=c("blue","red"))

# exp 0617
call.0617 <- NULL
SPX.strike.0617 <- SPX.option[[21]]$puts$Strike
put.0617 <- SPX.options.all[[21]]$puts$Price
t.0617 <- 129/365
call.real3 <- SPX.options.all[[21]]$calls$Price

for (i in 1:nrow(SPX.option[[21]]$puts)){
  call.0617[i] <- put.0617[i] + SPX.S0 - SPX.strike.0617[i]*exp(-r*t.0617)
}
plot(NA, xlim=c(0,300), ylim=c(0,3000),main="06-17 call option price",xlab="put",ylab="call")
lines(put.0617,call.0617,col="blue")
lines(put.0617[1:249],SPX.options.all[[21]]$calls$Price,col="red")
legend("bottomright",c("computed call","downloaded call"),fill=c("blue","red"))

# i)
# call delta = N(d1), Gamma = N'(d1)/S*sigam*sqrt(t), N'(d1) = exp(-d1^2/2)/sqrt(2*pi)
Nd1.call.0302 <- NULL #delta call
sigma.call.0302 <- SPX.option[[10]]$calls$IV
d1.call <- NULL

Nd1.call.0429 <- NULL
sigma.call.0429 <- SPX.option[[18]]$calls$IV
d1.call.0429 <- NULL

Nd1.call.0617 <- NULL
sigma.call.0617 <- SPX.option[[21]]$calls$IV
d1.call.0617 <- NULL

for(i in 1:nrow(SPX.option[[10]]$calls)){
  Nd1.call.0302[i] <- pnorm((log(SPX.S0/SPX.strike.0302[i]) + (r + 0.5*(sigma.call.0302[i])^2)*t.0302)/(sigma.call.0302[i]*sqrt(t.0302)))
  d1.call[i] <- (log(SPX.S0/SPX.strike.0302[i]) + (r + 0.5*(sigma.call.0302[i])^2)*t.0302)/(sigma.call.0302[i]*sqrt(t.0302))
}

for(i in 1:nrow(SPX.option[[18]]$calls)){
  d1.call.0429[i] <- (log(SPX.S0/SPX.strike.0429[i]) + (r + 0.5*(sigma.call.0429[i])^2)*t.0429)/(sigma.call.0429[i]*sqrt(t.0429))
}

Nd1.call.0429 <- pnorm(d1.call.0429)

for(i in 1:nrow(SPX.option[[21]]$calls)){
  d1.call.0617[i] <- (log(SPX.S0/SPX.strike.0617[i]) + (r + 0.5*(sigma.call.0617[i])^2)*t.0617)/(sigma.call.0617[i]*sqrt(t.0617))
}

Nd1.call.0617 <- pnorm(d1.call.0617)

Nd1.put.0302 <- Nd1.call.0302 - 1 #delta put
Nd1.put.0429 <- Nd1.call.0429 - 1
Nd1.put.0617 <- Nd1.call.0617 - 1

N_gamma_d1 <- exp(-(d1.call^2)/2)/sqrt(2*pi)
gamma <- N_gamma_d1/(SPX.S0*sigma.call.0302*sqrt(t.0302))

N_gamma_0429 <- exp(-(d1.call.0429^2)/2)/sqrt(2*pi)
gamma_0429 <- N_gamma_0429/(SPX.S0*sigma.call.0429*sqrt(t.0429))

N_gamma_0617 <- exp(-(d1.call.0617^2)/2)/sqrt(2*pi)
gamma_0617 <- N_gamma_0617/(SPX.S0*sigma.call.0617*sqrt(t.0617))

plot(NA, xlim=c(3500,5000), ylim=c(-1,1),main="call option delta",xlab="Strike",ylab="delta")
lines(SPX.strike.0302[1:149], Nd1.call.0302, col="blue")
lines(SPX.strike.0429[1:295], Nd1.call.0429, col="red")
lines(SPX.strike.0617[1:249], Nd1.call.0617, col="green")
legend("bottomright",SPX.expiration[c(10,18,21)],fill=c("blue","red","green"))


plot(NA, xlim=c(3500,5000), ylim=c(-1,1),main="put option delta",xlab="Strike",ylab="delta")
lines(SPX.strike.0302[1:149], Nd1.put.0302, col="blue")
lines(SPX.strike.0429[1:295], Nd1.put.0429, col="red")
lines(SPX.strike.0617[1:249], Nd1.put.0617, col="green")
legend("bottomleft",SPX.expiration[c(10,18,21)],fill=c("blue","red","green"))


plot(NA, xlim=c(3500,5000),ylim=c(0,0.01),main="03-02 gamma",xlab="Strike",ylab="gamma")
lines(SPX.strike.0302[1:149], gamma, col="blue")
lines(SPX.strike.0429[1:295], gamma_0429, col="red")
lines(SPX.strike.0617[1:249], gamma_0617, col="green")
legend("bottomleft",SPX.expiration[c(10,18,21)],fill=c("blue","red","green"))
