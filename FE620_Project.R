# Final Project
# FE620
# Team 2

rm(list = ls())

# Data analysis
library(quantmod)

getSymbols("MRNA", src="yahoo", from="2021-06-01", to="2021-12-07")
plot(Cl(MRNA))
R_MRNA <- periodReturn(MRNA, period="daily", type="log") 

# get log return
R_MRNA <- periodReturn(MRNA, period="daily", type="log")
plot(R_MRNA, main="MRNA daily log returns")

# check normality
qqnorm(R_MRNA, main="Q-Q plot of the MRNA log-returns")

# cumulative return
library(PerformanceAnalytics)
chart.CumReturns(R_MRNA)

# volatility
vol_MRNA <- sd(R_MRNA)*sqrt(252)

# Risk free rate ^IRX=13-week T-bill rate
getSymbols("^IRX",src="yahoo",from="2021-01-02",to="2021-12-01")
rf <- na.omit(Cl(IRX))
tail(rf)
plot(rf, main="13-week T-bill rate")

# option pricing with dummy data
library(derivmkts)
binomopt(S0,K,sigma,r,T1,0,nstep=5,american=TRUE,putopt = FALSE)
binomopt(S0,K,sigma,r,T1,0,nstep=10,american=TRUE,putopt = FALSE)
binomopt(S0,K,sigma,r,T1,0,nstep=20,american=TRUE,putopt = FALSE)
binomopt(S0,K,sigma,r,T1,0,nstep=100,american=TRUE,putopt = FALSE)
binomplot(S0, K, sigma, r, T1, 0, nstep=10, putopt=FALSE, american=TRUE,
          plotvalues=FALSE, plotarrows=FALSE, drawstrike=TRUE,
          pointsize=4, ylimval=c(0,0),
          saveplot = FALSE, saveplotfn='binomialplot.pdf',
          crr=FALSE, jarrowrudd=FALSE, titles=TRUE, specifyupdn=FALSE,
          up=1.5, dn=0.5, returnprice=FALSE, logy=FALSE)

# option price with real data

S0_MRNA <- last(MRNA$MRNA.Adjusted)
S0_MRNA

MRNA_opt <- getOptionChain("MRNA",NULL)
names(MRNA_opt)

# implied volatility
MRNA_exp <- names(MRNA_opt)# all expiration dates
T.vec <- (as.Date(MRNA_exp,"%b.%d.%Y")-Sys.Date())/365
T.vec <- as.numeric(T.vec)
MRNA.S0 <- getQuote("MRNA")$Last
r <- 0.048 * 0.01 # from 13 week Treasury bill

bisection.new <- function(f,a,b,tol=0.001,N.max=100){ # Generate bisection method
  f.a <- f(a)
  f.b <- f(b)
  if(f.a*f.b>0){
    warning("f(a) and f(b) have same sign, output may not be a root")
  } else if(f.a==0){
    return(a)
  } else if(f.b==0){
    return(b)
  } else if(is.na(f.a*f.b)){
    return(NA)
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
    } else {
      a<-c
      f.a <- f.c
    }
  }
  return(c)
}
bs.put <- function(S0, K, T1, sigma, r){ # bs put option pricing
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  exp(-r*T1)*K*pnorm(-d2) - S0*pnorm(-d1)
}

implied.vol.put <- function(S0, K, T1, r, price){ # implied vol, put option
  price.diff <- function(sigma)bs.put(S0,K,T1,sigma,r) - price
  return(bisection.new(price.diff,0.01,5))
}
calc <- function(x, T1){ # add a column of implied vol and price for each expiration date
  # put options same process
  x$puts$Price <- 0.5*(x$puts$Bid + x$puts$Ask)
  func.put <- function(K,P)implied.vol.put(353.84,K,4/252,0.00048,P)
  x$puts$impliedVol <- mapply(func.put,x$puts$Strike,x$puts$Price)
  x$puts <- x$puts[c("Bid","Ask","Strike","Price","impliedVol")]
  
  return(x)
}
MRNA_opt <- mapply(calc, MRNA_opt, T.vec, SIMPLIFY = FALSE)
plot(NA, xlim = c(100,500), ylim = c(0,7), xlab = "Strike", ylab = "ImpliedVol") # plot of volatility smile, 3 exp date
lines(MRNA_opt[[1]]$puts$Strike, MRNA_opt[[1]]$puts$impliedVol,col = "red") 
lines(MRNA_opt[[2]]$puts$Strike, MRNA_opt[[2]]$puts$impliedVol,col = "blue")
legend("bottomleft", MRNA_exp[c(1,2)], fill = c("red","blue"))

kStrikes <- numeric()
callPrice <- numeric()
putPrice <- numeric()
callDelta <- numeric()
callGamma <- numeric()
putDelta <- numeric()
putGamma <- numeric()

bs <- function(S0,K,T1,sigma,r,type){ # Generate Black scholes formula for both call and put
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1)) # d1
  d2 <- d1 - sigma*sqrt(T1) # d2
  if(type=="call"){ # call option
    return(S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2))
  } else if(type=="put"){ # put option
    return(exp(-r*T1)*K*(1-pnorm(d2))-S0*(1-pnorm(d1)))
  }
}

for(i in 1:9){ #option price and sensitivity with MRNA
  k <- 305 + 5*(i-1)
  S0 <- 320.72
  S0up <- S0 + 0.1
  S0down <- S0 - 0.1
  copt <- binomopt(S0,k,vol_MRNA,0.00048,7/252,0,nstep=100,american=TRUE,putopt = FALSE)
  coptup <- binomopt(S0up,k,vol_MRNA,0.00048,7/252,0,nstep=100,american=TRUE,putopt = FALSE)
  coptdown <- binomopt(S0down,k,vol_MRNA,0.00048,7/252,0,nstep=100,american=TRUE,putopt = FALSE)
  
  popt <- binomopt(S0,k,vol_MRNA,0.00048,7/252,0,nstep=100,american=TRUE,putopt = TRUE)
  poptup <- binomopt(S0up,k,vol_MRNA,0.00048,7/252,0,nstep=100,american=TRUE,putopt = TRUE)
  poptdown <- binomopt(S0down,k,vol_MRNA,0.00048,7/252,0,nstep=100,american=TRUE,putopt = TRUE)
  
  kStrikes <- c(kStrikes,k)
  callPrice <- c(callPrice,copt)
  callDelta <- c(callDelta,(coptup - coptdown)/0.2)
  callGamma <- c(callGamma,(coptup + coptdown - 2*(copt))/0.01)
  
  putPrice <- c(putPrice,popt)
  putDelta <- c(putDelta,(poptup - poptdown)/0.2)
  putGamma <- c(putGamma,(poptup + poptdown - 2*(popt))/0.01)
}

callData <- data.frame(kStrikes,callPrice,callDelta,callGamma)
putData <- data.frame(kStrikes,putPrice,putDelta,putGamma)

tail(MRNA)

kStrikes_MRNA <- numeric()
call_MRNA <- numeric()
put_MRNA <- numeric()
cbs <- numeric()
pbs <- numeric()

for(i in 1:7){ #option price and sensitivity with MRNA
  k <- 305 + 5*(i-1)
  S0 <- 320.72
  copt <- binomopt(S0,k,vol_MRNA,0.00048,2/252,0,nstep=100,american=TRUE,putopt = FALSE)
  popt <- binomopt(S0,k,vol_MRNA,0.00048,2/252,0,nstep=100,american=TRUE,putopt = TRUE)
  bs_call <- bs(S0,k,2/252,vol_MRNA,0.00048,"call")
  bs_put <- bs(S0,k,2/252,vol_MRNA,0.0048,"put")
  kStrikes_MRNA <- c(kStrikes_MRNA,k)
  call_MRNA <- c(call_MRNA,copt)
  put_MRNA <- c(put_MRNA,popt)
  cbs <- c(cbs,bs_call)
  pbs <- c(pbs,bs_put)
}
callD_MRNA <- data.frame(kStrikes_MRNA,call_MRNA,cbs)
putD_MRNA <- data.frame(kStrikes_MRNA,put_MRNA,pbs)

plot(kStrikes, putDelta)
plot(kStrikes, putGamma)

# Hedging
# put option with K = 320

hedge0 <-  binomopt(50,50,0.01,0.01,30/252,0,nstep=100,american=FALSE,putopt=FALSE)
hedge0up <-   binomopt(50.1,50,0.01,0.01,30/252,0,nstep=100,american=FALSE,putopt=FALSE)
hedge0down <-   binomopt(49.9,50,0.01,0.01,30/252,0,nstep=100,american=FALSE,putopt=FALSE)
delta0 <- (hedge0up + hedge0down - hedge0*2)/0.01
delta0
