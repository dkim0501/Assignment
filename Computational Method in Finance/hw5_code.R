# FE 621
# Dong Woo Kim
# hw5

rm(list = ls())

# 1
x0 <- 1
error_1 <- function(h){
  error <- cos(x0) - (sin(x0+h)-sin(x0))/h
  return(error)
}

error_0 <- function(h){
  error <- cos(x0) - (sin(x0+h)-2*sin(x0)+sin(x0-h))/(h)
  return(error)
}

error_h <- function(h){
  error <- cos(x0) - (2*sin(x0+h)+3*sin(x0)-6*sin(x0-h)+sin(x0-2*h))/(6*h)
  return(error)
}

hset <- NULL
for(i in 1:20){
  hset[i] <- 2^(-i)
}


e1 <- abs(error_1(hset))
e0 <- abs(error_0(hset))
eh <- abs(error_h(hset))


plot(log(hset),log(e1),type='l',col="blue",ylim=c(-40,0),xlab="log h",ylab="log error",main="log-log plot")
lines(log(hset),log(e0),type='l',col="red")
lines(log(hset),log(eh),type='l',col="green")
legend("bottomright",c("eh(1)","eh(0)","eh"),col=c("blue","red","green"),lwd=c(1,1))

slope1 <- (log(e1[4])-log(e1[3]))/(log(hset[4])-log(hset[3]))
slope2 <- (log(e0[4])-log(e0[3]))/(log(hset[4])-log(hset[3]))
slope3 <- (log(eh[4])-log(eh[3]))/(log(hset[4])-log(hset[3]))


## 2
K <- 10
r <- 0.05
sigma <- 0.2
T <- 0.5
n <- 3

# bs price
bs.put <- function(S0, K, T1, sigma, r){ # bs put option pricing
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  exp(-r*T1)*K*pnorm(-d2) - S0*pnorm(-d1)
}

bs.put_set <-NULL
for(i in 1:13){
  bs.put_set[i] <- bs.put(i+3,K,T,sigma,r)
}

# explicit method
explicit <- function(K,r,sigma,T,S0,n,dx){
  
  m <- n
  dt <- T/n
  dx <- dx

  pu <- dt*sigma^2/(2*dx^2)+(r-sigma^2/2)*dt/(2*dx)
  pd <- dt*sigma^2/(2*dx^2)-(r-sigma^2/2)*dt/(2*dx)
  ps <- 1-pu-pd
  
  ns <- 0
  firstRow <- 1
  firstCol <- 1
  
  if(ns!=0){
    lastRow <- 2*ns+1
    middleRow <- ns+1
    lastCol <- n+1
    u <- matrix(0,nrow=lastRow,ncol=lastCol)
  } else{
    ns <- n
    lastRow <- 2*ns+1
    middleRow <- n+1
    u <- matrix(0,nrow=lastRow,ncol=lastCol)
  }
  X <- NULL
  for(i in 1:(2*n+1)){
    X[i] <- log(S0) + (i-(n+1))*dx 
  }
  
  for(i in 1:lastRow){
    u[i,lastCol] <- exp(-r*T)*pmax(0,(K-exp(X[i])))
  }
  
  # backward
  for(i in n:1){
    for(j in (middleRow+ns-1):(middleRow-ns+1)){
      u[j,i] <- pu*u[j-1,i+1] + ps*u[j,i+1] + pd*u[j+1,i+1]
    }
  }
  price <- u[middleRow,firstCol]
  return(price)
}

explicit(K,r,sigma,T,4,3,0.2^2/3) # S0 = 4
explicit(K,r,sigma,T,16,3,0.2^2/3) # S0 = 16
explicit_set <- NULL
explicit_set_2 <- NULL
explicit_set_3 <- NULL

for(i in 1:13){
  explicit_set[i] <- explicit(K,r,sigma,T,i+3,3,sqrt(0.04/6)) # alpha = 0.5
  explicit_set_2[i] <- explicit(K,r,sigma,T,i+3,3,sqrt(0.08/6)) # alpha = 0.25
  explicit_set_3[i] <- explicit(K,r,sigma,T,i+3,3,0.025) # aplha = 0.55
}


# 3
N <- 3
dt <- 0.5/3
dx <- 0.003
alpha <- -(r-sigma^2/2)*(dt/(2*dx))
beta <- -(sigma^2/2)*(dt/dx^2)

Thomas <- function(S0){
  C <- matrix(c(1+alpha-beta,alpha+beta,0,0,0,-alpha+beta,1-2*beta,alpha+beta,0,0,0,-alpha+beta,
              1-2*beta,alpha+beta,0,0,0,-alpha+beta,1-2*beta,alpha+beta,0,0,0,alpha+beta,1+alpha-beta),nrow=5)
  b <- NULL
  S0 <- S0
  for(i in 1:5){
  b[i] <- log(S0) + dx*(3-i)
  }

  # upward sweeping
  for(i in 2:5){
  C[i,i] <- C[i,i] - C[i,i-1]/C[i-1,i-1]*C[i-1,i]
  b[i] <- b[i] - C[i,i-1]/C[i-1,i-1]*b[i-1]
  }
  
  # downward sweeping
  u <- matrix(c(0,0,0,0,0))

  for(i in 2:5){
  u[1] <- b[1]/C[1,1]
  u[i] <- (b[i]-C[i-1,i]*u[i-1])/C[i,i]
  }
  price <- pmax(K-exp(b[1]),0)
  
  if(u[5] < price){
    return(u[5])
  } else{
    return(price)
  }
}

result <- NULL
for(i in 4:16){
  result[i-3] <- Thomas(i)
}

