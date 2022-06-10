# FE 535
# Lab 3
# Team 9

library(quantmod)

symbol <- "SPY"
P <- get(getSymbols(symbol,from="1990-01-01"))
head(P)

P <- P$SPY.Adjusted
R <- na.omit(log(P/lag(P)))

library(lubridate)
R_sub <- R["2017-01-01/2021-09-24",]

S <- 443.91
m <- 252*mean(R_sub)
sig <- sqrt(252)*sd(R_sub)
Mu <- m + 0.5*sig^2

gbm_path <- function(N,mu,sigma,T_end) {
  R_t <- rnorm(N,T_end*(mu-0.5*sigma^2),sigma*sqrt(T_end))
  s_t <- S*exp(R_t)
  return(s_t)
}

sim1 <- gbm_path(1000,Mu,sig,1)
hist(sim1,main = "1000 simulations with daily data", xlab = "SPY price")

F_bar_1 <- mean(sim1)
var1_0.05 <- F_bar_1 - quantile(sim1,0.05)
var1_0.05

PW <- to.weekly(P$SPY)
P2 <- PW$`P$SPY.Close`
R2 <- na.omit(log(P2/lag(P2)))
R2_sub <- R2["2017-01-01/2021-09-24",]

m2 <- 52*mean(R2_sub)
sig2 <- sqrt(52)*sd(R2_sub)
Mu2 <- m + 0.5*sig2^2

sim2 <- gbm_path(1000,Mu2,sig2,1)
hist(sim2,main = "1000 simulations with weekly data", xlab = "SPY price")

F_bar_2 <- mean(sim2)
var2_0.05 <- F_bar_2 - quantile(sim2,0.05)
var2_0.05
