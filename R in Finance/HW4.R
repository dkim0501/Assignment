# FE 515
# HW4
# Dong Woo Kim

rm(list = ls())

#1.1

bs <- function(S0,K,T1,sigma,r,type){ # Generate Black scholes formula for both call and put
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1)) # d1
  d2 <- d1 - sigma*sqrt(T1) # d2
  if(type=="call"){ # call option
  return(S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2))
  } else if(type=="put"){ # put option
    return(exp(-r*T1)*K*(1-pnorm(d2))-S0*(1-pnorm(d1)))
  }
}

call <- bs(100,100,1,0.2,0.05,"call") # call option with S0=100, K=100, T=1, sigma=0.2, r=0.05
call 

put <- bs(100,100,1,0.2,0.05,"put") # put option with same condition
put

#1.2

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


iv <- function(S0,K,T1,r,price,type){ # Generate implied volatility function
  price.diff <- function(sigma)bs(S0,K,T1,sigma,r,type) - price
  return(bisection.new(price.diff,0.01,5))
}

iv.call <- iv(100,100,1,0.05,10,"call") # Get implied volatility when S0=100,K=100,T=1,r=0.05 call option
iv.call

iv.put <- iv(100,100,1,0.05,5,"put") # Get implied volatility with same condition but put option
iv.put


# 2.1

S0 <- 100
T1 <- 1
sigma <- 0.2
r <- 0.05
n <- 252
h <- T1/n

MC <- function(m){ # MC simulation with S0=100, K=100, T=1, sigma=0.2, r=0.05 and n=252
  #m <- 10000
  S.vec <- rep(100,m)
  Z <- matrix(rnorm(n*m),nrow = n)
  for(i in 1:n){
    S.vec <- S.vec + r*S.vec*h + sigma*S.vec*Z[i,]*sqrt(h)
  }
  exp(-r*T1)*mean(pmax(0,100 - S.vec))
}

MC(10000) # 10000 times simulation, get put option price

# put option price is 5.598282 and it is close to 5.573526
