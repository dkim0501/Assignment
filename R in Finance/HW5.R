# FE515
# HW5
# Dong Woo Kim

rm(list = ls())

# 1.1
f <- function(x)0.5*x %*% matrix(c(0.01,0.002,0.001,0.002,0.011,0.003,0.001,0.003,0.02),3,3) %*% x # Set f
ui <- matrix(c(0.0427,0.0015,0.0285,-1,-1,-1),2,3,byrow=TRUE) # Set ui
ci <- c(0.05,-1) # Set ci

constrOptim(c(2,-2,0), f, grad=NULL, ui=ui, ci=ci)$par # find optimal portfolio weight x


# 1.2
install.packages("quadprog")
library(quadprog)

D <- matrix(c(0.01,0.002,0.001,0.002,0.011,0.003,0.001,0.003,0.02),3,3) # Set D
d <- c(0,0,0) # Set the d whichi is 0
A <- t(matrix(c(0.0427,0.0015,0.0285,-1,-1,-1),2,3,byrow=TRUE)) # Set A
b <- c(0.05,-1) # Set b

solve.QP(D,d,A,b)$value # get the minimization value by using quadratic programming


# 2
# linear interpolation
t <- c(0.25,0.5,1,2,3,5,7,10) # time
r <- c(0.09,0.11,0.16,0.20,0.24,0.36,0.53,0.64) # Treasury yield rate

tout <- c(0.75,1.5,4,6,8) # t out
rout <- approx(x=t,y=r,xout=tout)$y # linear interpolation with built in function to find r out
rout # print r out

# Spline interpolation
yout.s <- spline(x=t,y=r,xout=tout,method="natural") # spline interpolation with built in function to find r out
yout.s$y # print r out


# 3
# setting the data
S0 <- 100 
K <- 100
T <- 1
rate <- 0.05
sigma <- 0.2
d2 <- (log(S0/K) + (rate - 0.5*sigma^2)*T)/(sigma*sqrt(T)) 

I <- function(x)((S0*exp((rate-0.5*sigma^2)*T+sigma*sqrt(T)*x)-K)*dnorm(x)) # using integrate fucntion
value <- exp(-rate*T)*integrate(I,-d2,Inf)$value # Integrate function I from -d2 to infinite
value # print value

# Black-Scholes model
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

# The price from BS model is 10.45058 and from integration is 10.45058.
# Two values are same
