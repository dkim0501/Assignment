# Final
# FE515
# Dong Woo Kim


rm(list = ls())
library(quantmod)

#Question1
#1.1
getSymbols("SPY", src="yahoo", from="2019-01-01") #download SPY data

#1.2
P_SPY <- SPY$SPY.Adjusted #get adjusted closed value
R_SPY <- na.omit(log(P_SPY/lag(P_SPY))) #get daily log return

#1.3
plot(R_SPY,col="red") #plot with red line

#Question2
#2.1
skew <- function(x,adjusted){ #skewness function
  n <- length(x)
  m2 <- mean((x-mean(x))^2)
  m3 <- mean((x-mean(x))^3)
  if(adjusted == TRUE){
    (m3/(m2^1.5))*sqrt(n*(n-1))/(n-2)}
  else{
    m3/(sd(x)^3)}
}

SPY_skew_ad <- skew(R_SPY,TRUE) #adjusted skewness
SPY_skew_unad <- skew(R_SPY,FALSE) #unadjusted skewness
SPY_skew_ad #print adjusted skewness
SPY_skew_unad #print unadjusted skewness

kurt <- function(x,adjusted){ #kurtosis function
  n <- length(x)
  m2 <- mean((x-mean(x))^2)
  m4 <- mean((x-mean(x))^4)
  m4_moment <- m4/(m2^2)
  if(adjusted == TRUE){
    (n-1)*((n+1)*m4_moment-3*(n-1))/((n-2)*(n-3)) + 3}
  else{
    m4/(sd(x)^4)}
}

SPY_kurt_ad <- kurt(R_SPY,TRUE) #adjusted kurtosis
SPY_kurt_unad <- kurt(R_SPY,FALSE) #unadjusted kurtosis
SPY_kurt_ad #print adjusted kurtosis
SPY_kurt_unad #print unadjusted kurtosis

#2.2
M <- matrix(c(SPY_skew_ad,SPY_skew_unad,SPY_kurt_ad,SPY_kurt_unad),2,2) #generate matrix
rownames(M) <- c("Adjusted","Unadjusted") #change row name
colnames(M) <- c("SPY.skewness","SPY.kurtosis") #change column name
M #report the matrix

#Question3
#3.1, 3.2, 3.3
SPY.options.all <- getOptionChain("SPY",NULL) #download option data
SPY.expiration <- names(SPY.options.all)# all expiration dates
T.vec <- (as.Date(SPY.expiration,"%b.%d.%Y")-Sys.Date())/365
T.vec <- as.numeric(T.vec)
SPY.S0 <- getQuote("SPY")$Last
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

bs.call <- function(S0, K, T1, sigma, r){ # bs call option pricing
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2)
}

implied.vol.call <- function(S0, K, T1, r, price){ # implied vol, call option
  price.diff <- function(sigma)bs.call(S0, K, T1, sigma, r) - price
    return(bisection.new(price.diff, 0.01, 2))
   
}

bs.put <- function(S0, K, T1, sigma, r){ # bs put option pricing
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  exp(-r*T1)*K*pnorm(-d2) - S0*pnorm(-d1)
}

implied.vol.put <- function(S0, K, T1, r, price){ # implied vol, put option
  price.diff <- function(sigma)bs.put(S0,K,T1,sigma,r) - price
  return(bisection.new(price.diff,0.01,2))
}

calc <- function(x, T1){ # add a column of implied vol and price for each expiration date
  # add a column of price
  x$calls$Price <- 0.5*(x$calls$Bid + x$calls$Ask)
  # add a column of implied volatility
  func <- function(K,P)implied.vol.call(SPY.S0,K,T1,r,P)
  x$calls$impliedVol <- mapply(func, x$calls$Strike, x$calls$Price)
  # delete columns
  x$calls <- x$calls[c("Bid","Ask","Strike","Price","impliedVol")]
  
  # put options same process
  x$puts$Price <- 0.5*(x$puts$Bid + x$puts$Ask)
  func.put <- function(K,P)implied.vol.put(SPY.S0,K,T1,r,P)
  x$puts$impliedVol <- mapply(func.put,x$puts$Strike,x$puts$Price)
  x$puts <- x$puts[c("Bid","Ask","Strike","Price","impliedVol")]

  return(x)
}
SPY.options.all <- mapply(calc, SPY.options.all, T.vec, SIMPLIFY = FALSE) # print SPY option price and implied vol

#3.4
plot(NA, xlim = c(400,600), ylim = c(0,0.8), xlab = "Strike", ylab = "ImpliedVol") # plot of volatility smile, 3 exp date
lines(SPY.options.all[[14]]$puts$Strike, SPY.options.all[[14]]$puts$impliedVol,col = "red") 
lines(SPY.options.all[[18]]$puts$Strike, SPY.options.all[[18]]$puts$impliedVol,col = "green")
lines(SPY.options.all[[21]]$puts$Strike, SPY.options.all[[21]]$puts$impliedVol,col = "blue")
legend("bottomleft", SPY.expiration[c(14,18,21)], fill = c("red","green","blue"))

#3.5
setwd("C:/Users/USER/Desktop/Stevens/FE515/Final")
Sys.Date()
today <- format(Sys.Date(),"%Y_%m_%d")
Exp <- names(SPY.options.all)
Exp <- as.Date(Exp, format = "%b.%d.%Y")
for(i in 1:length(Exp)){
  write.csv(SPY.options.all[[i]]$calls, file=paste0("data",today,"Exp",Exp[i],"calls.csv"))
  write.csv(SPY.options.all[[i]]$puts, file=paste0("data",today,"Exp",Exp[i],"puts.csv"))
}


