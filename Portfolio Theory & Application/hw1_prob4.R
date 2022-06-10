# FE 630
# Hw 1
# Dong Woo Kim
# Problem 4

rm(list = ls())
library(quantmod)

# download stock and get the daily log return (a), (b)
AAPL <- "AAPL"
P_AAPL <- get(getSymbols(AAPL,from="2021-01-01",to="2021-12-31")) # get apple stock price
P_AAPL <- P_AAPL$AAPL.Adjusted
R_AAPL <- na.omit(log(P_AAPL/lag(P_AAPL))) # get daily log return

GOOGL <- "GOOGL"
P_GOOGL <- get(getSymbols(GOOGL,from="2021-01-01",to="2021-12-31"))
P_GOOGL <- P_GOOGL$GOOGL.Adjusted
R_GOOGL <- na.omit(log(P_GOOGL/lag(P_GOOGL)))

FB <- "FB"
P_FB <- get(getSymbols(FB,from="2021-01-01",to="2021-12-31"))
P_FB <- P_FB$FB.Adjusted
R_FB <- na.omit(log(P_FB/lag(P_FB)))

AMZN <- "AMZN"
P_AMZN <- get(getSymbols(AMZN,from="2021-01-01",to="2021-12-31"))
P_AMZN <- P_AMZN$AMZN.Adjusted
R_AMZN <- na.omit(log(P_AMZN/lag(P_AMZN)))  

Annual.R_AAPL <- mean(R_AAPL)*252
Annual.R_GOOGL <- mean(R_GOOGL)*252
Annual.R_FB <- mean(R_FB)*252
Annual.R_AMZN <- mean(R_AMZN)*252

# covariance matrix (c)
return_set <- data.frame(R_AAPL,R_GOOGL,R_FB,R_AMZN)
cov_set <- cov(return_set)*252

# create sequence 0 to 0.5 in steps of 0.001 (d)
lamda <- seq(from=0.001, to=0.5, by=0.001)

# Make a loop (e)
library(quadprog)

Mu <- rep(0, length(lamda))
sigma <- rep(0, length(lamda))
mean_return <- c(Annual.R_AAPL,Annual.R_GOOGL,Annual.R_FB,Annual.R_AMZN)
value <- NULL

for(i in lamda) {
  D <- i*cov_set
  dvec <- t(mean_return)
  A <- matrix(c(1,1,1,1))
  bvec <- c(1)
  
  result <- solve.QP(D,dvec,A,bvec,meq=1)
  sol <- result$solution
  R <- round(t(mean_return)%*%sol, digits=2)
  SD <- round((t(sol)%*%cov_set%*%sol), digits=2)
  val <- result$value
  
  value <- c(value,val)
  Mu <- c(Mu,R)
  sigma <- c(sigma,SD)
  
}

max(value) # find optimal value

# plot the graph (f)
plot(sigma[501:1000],Mu[501:1000],xlab='Volatility', ylab='Expected Return',type='l')
points(19.95,10.26,col='red') #optimal point
