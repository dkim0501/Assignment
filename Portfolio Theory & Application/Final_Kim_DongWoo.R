# FE 630 WS
# Dong Woo Kim
# Final Project

rm(list = ls())

library(quantmod)
library(quadprog)
library(lubridate)
library(zoo)
library(xts)
library(PerformanceAnalytics)
library(nloptr)

# tickers: SPY,FXE,EWJ,GLD,QQQ,SHV,DBA,USO,XBI,ILF,EPP,FEZ
#### download ticker
tickers <- c("SPY","FXE","EWJ","GLD","QQQ","SHV","DBA","USO", "XBI", "ILF","EPP", "FEZ")
getSymbols(tickers,src="yahoo",from="2007-2-28", to="2022-04-01")

# return function (log return)
retfun <- function(p) {
  na.omit(log(p) - log(lag(p)))
}

# get the Adjusted daily stock price
P_merge <- merge(SPY$SPY.Adjusted,FXE$FXE.Adjusted,EWJ$EWJ.Adjusted,GLD$GLD.Adjusted,QQQ$QQQ.Adjusted,SHV$SHV.Adjusted,
                 DBA$DBA.Adjusted,USO$USO.Adjusted,XBI$XBI.Adjusted,ILF$ILF.Adjusted,EPP$EPP.Adjusted,FEZ$FEZ.Adjusted)

names(P_merge) <- c("SPY","FXE","EWJ","GLD","QQQ","SHV","DBA","USO", "XBI", "ILF","EPP", "FEZ")

# get daily return of ticker
R_merge <- retfun(P_merge)

# load French-Fama 3-factors
setwd("C:/Users/USER/Desktop/Stevens/22SS/FE630/Final")
FF <- read.csv("FF.csv")
FF <- FF[,c(-1,-6)]/252 # make daily
FF <- as.data.frame(FF)
Date <- index(R_merge)
rownames(FF) <- Date

# get the beta, covmat Q and Mu with FF 3 factors
r <- R_merge - FF[,2] # ri - rf
FF_cov <- cbind(rep(1,length(FF[,1])),FF[,1:3])
index_SPY <- R_merge[,1]
R_reg <- R_merge
coef <- matrix(0,nrow=12,ncol=4)
beta <- matrix(0,nrow=12,ncol=1)

MuQBeta <- function(Mu_dt,Q_dt,date){
  # set the terms
  base <- which(Date == date)
  mu_a <- base - Mu_dt
  mu_b <- base - 1
  
  Q_a <- base - Q_dt
  Q_b <- base - 1
  
  index_mu <- index_SPY[mu_a:mu_b,] #benchmark index which is SPY
  FF_mu <- FF[mu_a:mu_b,] #FF factors for mu
  R_mu <- R_reg[mu_a:mu_b,] #historical data for mu
  r_mu <- r[mu_a:mu_b,] #ri-rf for mu
  
  FF_Q <- FF_cov[Q_a:Q_b,] #FF facotrs for cov
  R_Q <- R_merge[Q_a:Q_b,] #FF factors for cov
  
  # get the beta and some coefs from fama factor
  for (i in 1:12) {
    coef[i,] <- coefficients(lm(r_mu[,i] ~ FF_mu[,1]+FF_mu[,2]+FF_mu[,3]))
    beta[i,] <- coefficients(lm(R_mu[,i] ~ index_mu[,1]))[2]
  }
  rownames(beta) <- c("SPY","FXE","EWJ","GLD","QQQ","SHV","DBA","USO", "XBI", "ILF","EPP", "FEZ")
  
  #3 get the Mu, covmat Q
  rho_ff <- t(as.matrix(coef) %*% t(as.matrix(FF_cov)))
  Mu <- as.matrix(apply(rho_ff,2,mean))
  Q <- as.matrix(cov(rho_ff))
  rownames(Q) <- c("SPY","FXE","EWJ","GLD","QQQ","SHV","DBA","USO", "XBI", "ILF","EPP", "FEZ")
  colnames(Q) <- c("SPY","FXE","EWJ","GLD","QQQ","SHV","DBA","USO", "XBI", "ILF","EPP", "FEZ")
  result <- list(Q,Mu,beta) #list covmat, Mu, beta
  names(result) <- c("Q","Mu","beta")
  return(result)
}

MuQBeta(60,30,"2008-03-04")

# make portfolio strategy 1

port1 <- function(Mu,Q,beta,lambda){ # strategy 1
  objective <- function(w){
    obj <- lambda*sqrt(t(w) %*% Q %*% w) - t(Mu) %*% w # obj function lambda = 2
    return(obj)
  }

  hin.objective <- function(w){ # -0.5 <= beta * w <= 0.5
    hin <- abs(t(beta) %*% w) - 0.5
    return(hin)
  }

  heq.objective <- function(w) { # sum(w) = 1
    sum_w <- sum(w)
    return(sum_w-1)
  }
  
  result <- slsqp(x0=as.matrix(rep(1/12,12)),fn=objective,hin=hin.objective,heq=heq.objective,lower=rep(-2,12),
                  upper=rep(2,12)) # make constraint of w and get the portfolio
  return(result$par)
}

Mu1 <- as.matrix(as.data.frame(MuQBeta(60,40,"2009-01-06")[2])) #check the function
beta1 <- as.matrix(as.data.frame(MuQBeta(60,40,"2009-01-06")[3]))
Q1 <- as.matrix(as.data.frame(MuQBeta(60,40,"2009-01-06")[1]))

port1(Mu1,Q1,beta1,2)


# make portfolio strategy 2

TEV <- function(dt,date){ # get cov(r,r_spy), var(spy) for TEV
  
  base <- which(Date == date)
  a <- base - dt
  b <- base - 1
  
  var_spy <- var(index_SPY[a:b])
  r <- R_reg[a:b,]
  cov_spy <- matrix(0,nrow=12,ncol=1)
  
  for(i in 1:12){
    cov_spy[i] <- cov(r[,1],r[,i])
  }
  rownames(cov_spy) <- c("SPY","FXE","EWJ","GLD","QQQ","SHV","DBA","USO", "XBI", "ILF","EPP", "FEZ")
  colnames(cov_spy) <- c("cov")
  result <- list(var_spy,cov_spy)
  return(result)
}
 
var_spy <- as.matrix(as.data.frame(TEV(90,"2009-01-06")[1]))
cov_spy <- as.matrix(as.data.frame(TEV(60,"2009-01-06")[2]))
  
port2 <- function(Mu,Q,beta,var_spy,cov_spy,lambda){ # strategy 2
  objective <- function(w){
    TEV <- sqrt(abs(t(w)%*%Q%*%w - 2*t(w)%*%cov_spy + var_spy))
    obj <- lambda*sqrt(t(w) %*% Q %*% w) - (t(Mu) %*% w)/TEV
    return(obj)
  }
  
  hin.objective <- function(w){ # -1 <= beta * w <= 1
    hin <- abs(t(beta) %*% w) - 1
    return(hin)
  }
  
  heq.objective <- function(w) { # sum(w) = 1
    sum_w <- sum(w)
    return(sum_w-1)
  }
  
  result <- slsqp(x0=as.matrix(rep(1/12,12)),fn=objective,hin=hin.objective,heq=heq.objective,lower=rep(-2,12),
                  upper=rep(2,12)) # make constraint of w and get the portfolio
  return(result$par) 
}

port2(Mu1,Q1,beta1,var_spy,cov_spy,2)
w0 <- as.matrix(rep(1/12,12))
TEV <- sqrt(abs(t(w0)%*%Q1%*%w0 - 2*t(w0)%*%cov_spy + var_spy))

## get the return
# before crisis
return_1 <- function(date_start,date_end,dt_mu,dt_Q,lambda){ # strategy 1
  
  first <- which(Date == date_start)
  last <- which(Date == date_end)
  term <- last - first
  rebal <- term %/% 5
  
  R_st <- as.matrix(R_merge[first:last-1,])
  
  w <- NULL
  ret <- NULL
  port_ret <- NULL
  
  for(i in 1: rebal){
    Q <- as.matrix(as.data.frame(MuQBeta(dt_mu,dt_Q,date_start)[1]))
    Mu <- as.matrix(as.data.frame(MuQBeta(dt_mu,dt_Q,date_start)[2]))
    beta <- as.matrix(as.data.frame(MuQBeta(dt_mu,dt_Q,date_start)[3]))
    
    w_rebal <- port1(Mu,Q,beta,lambda)
    w <- rbind(w,t(w_rebal))
    ret <- R_st[(5*(i-1)+1):(5*(i-1)+5),] %*% w_rebal
    port_ret <- rbind(port_ret,ret)
    
    first <- first + 5
    date_start <- as.Date(Date[first])
  }
  return(port_ret)
}

return_2 <- function(date_start,date_end,dt_mu,dt_Q,lambda){ # strategy 2
  
  first <- which(Date == date_start)
  last <- which(Date == date_end)
  term <- last - first
  rebal <- term %/% 5
  
  R_st <- as.matrix(R_merge[first:last-1,])
  
  w <- NULL
  ret <- NULL
  port_ret <- NULL
  
  for(i in 1:rebal){
    Q <- as.matrix(as.data.frame(MuQBeta(dt_mu,dt_Q,date_start)[1]))
    Mu <- as.matrix(as.data.frame(MuQBeta(dt_mu,dt_Q,date_start)[2]))
    beta <- as.matrix(as.data.frame(MuQBeta(dt_mu,dt_Q,date_start)[3]))
    
    var_spy <- as.matrix(as.data.frame(TEV(dt_mu,date_start)[1]))
    cov_spy <- as.matrix(as.data.frame(TEV(dt_mu,date_start)[2]))
    
    w_rebal <- port2(Mu,Q,beta,var_spy,cov_spy,lambda)
    w <- rbind(w,t(w_rebal))
    ret <- R_st[(5*(i-1)+1):(5*(i-1)+5),] %*% w_rebal
    port_ret <- rbind(port_ret,ret)
    
    first <- first + 5
    date_start <- as.Date(Date[first])
  }
  return(port_ret)
} 


#strategy 1 return
before_st <- return_1("2007-12-05","2008-06-27",60,40,2) #short term 60, 40
before_mt <- return_1("2007-12-05","2008-06-27",90,40,2) #mid term 90, 40
before_lt <- return_1("2007-12-05","2008-06-27",180,90,2) #long term 180, 90

before_spy <- as.matrix(R_merge["2007-12-04/2008-06-24"][,1]) #before crisis spy index

before_set <- cbind(before_spy,before_st,before_mt,before_lt)
colnames(before_set) <- c("SPY","st","mt","lt")

chart.CumReturns(before_set,wealth.index = TRUE,main="cumreturn before crisis - strategy 1",colorset=c("Blue","orange","green","red"),legend.loc = "topleft")

before_mean <- apply(before_set,2,mean)*250 #daily return mean
before_vol <- apply(before_set,2,sd)*sqrt(250) #volatility
before_dd <- apply(before_set,2,maxDrawdown) #DD
before_kurt <- apply(before_set,2,kurtosis) #kurt
before_skew <- apply(before_set,2,skewness) #skew
before_VaR <- apply(before_set,2,VaR) #VaR
before_sharpe <- apply(before_set,2,SharpeRatio.annualized) #SR

before_table <- rbind(before_mean,before_vol,before_dd,before_kurt,before_skew,before_VaR,before_sharpe)
hist(before_st,main="Before Crisis Strat 1 - S(60/40)")
hist(before_mt,main="Before Crisis Strat 1 - S(90/40)")
hist(before_lt,main="Before Crisis Strat 1 - S(180/90)")

# strategy 2
before_st_2 <- return_2("2007-12-05","2008-06-27",60,40,2)
before_mt_2 <- return_2("2007-12-05","2008-06-27",90,40,2) #mid term 90, 40
before_lt_2 <- return_2("2007-12-05","2008-06-27",180,90,2) #long term 180, 90

before_set_2 <- cbind(before_spy,before_st_2,before_mt_2,before_lt_2)
colnames(before_set_2) <- c("SPY","st","mt","lt")

chart.CumReturns(before_set_2,wealth.index = TRUE,main="cumreturn before crisis - strategy 2",colorset=c("Blue","orange","green","red"),legend.loc = "topleft")

before_mean_2 <- apply(before_set_2,2,mean)*250 #daily return mean
before_vol_2 <- apply(before_set_2,2,sd)*sqrt(250) #volatility
before_dd_2 <- apply(before_set_2,2,maxDrawdown) #DD
before_kurt_2 <- apply(before_set_2,2,kurtosis) #kurt
before_skew_2 <- apply(before_set_2,2,skewness) #skew
before_VaR_2 <- apply(before_set_2,2,VaR) #VaR
before_sharpe_2 <- apply(before_set_2,2,SharpeRatio.annualized) #SR

before_table_2 <- rbind(before_mean_2,before_vol_2,before_dd_2,before_kurt_2,before_skew_2,before_VaR_2,before_sharpe_2)
hist(before_st_2,main="Before Crisis strat 2 - S(60/40)")
hist(before_mt_2,main="Before Crisis strat 2 - S(90/40)")
hist(before_lt_2,main="Before Crisis strat 2 - S(180/90)")

## During crisis
#strategy 1
during_st <- return_1("2008-09-02","2010-09-01",60,40,2)
during_mt <- return_1("2008-09-02","2010-09-01",90,40,2)
during_lt <- return_1("2008-09-02","2010-09-01",180,90,2)

during_spy <- as.matrix(R_merge["2008-08-29/2010-08-24"][,1])

during_set <- cbind(during_spy,during_st,during_mt,during_lt)
colnames(during_set) <- c("SPY","st","mt","lt")

chart.CumReturns(during_set,wealth.index = TRUE,main="cumreturn during crisis - strategy 1",
                 colorset=c("Blue","orange","green","red"),legend.loc = "topleft")

during_mean <- apply(during_set,2,mean)*250 #daily return mean
during_vol <- apply(during_set,2,sd)*sqrt(250) #volatility
during_dd <- apply(during_set,2,maxDrawdown) #DD
during_kurt <- apply(during_set,2,kurtosis) #kurt
during_skew <- apply(during_set,2,skewness) #skew
during_VaR <- apply(during_set,2,VaR) #VaR
during_sharpe <- apply(during_set,2,SharpeRatio.annualized) #SR

during_table <- rbind(during_mean,during_vol,during_dd,during_kurt,during_skew,during_VaR,during_sharpe)

hist(during_st,main="During Crisis Strat 1 - S(60/40)")
hist(during_mt,main="During Crisis Strat 1 - S(90/40)")
hist(during_lt,main="During Crisis Strat 1 - S(180/90)")

#strategy 2
during_st_2 <- return_2("2008-09-02","2010-09-01",60,40,2)
during_mt_2 <- return_2("2008-09-02","2010-09-01",90,40,2)
during_lt_2 <- return_2("2008-09-02","2010-09-01",180,90,2)

during_set_2 <- cbind(during_spy,during_st_2,during_mt_2,during_lt_2)
colnames(during_set_2) <- c("SPY","st","mt","lt")

chart.CumReturns(during_set_2,wealth.index = TRUE,main="cumreturn during crisis - Straety2",
                 colorset=c("Blue","orange","green","red"),legend.loc = "topleft")

during_mean_2 <- apply(during_set_2,2,mean)*250 #daily return mean
during_vol_2 <- apply(during_set_2,2,sd)*sqrt(250) #volatility
during_dd_2 <- apply(during_set_2,2,maxDrawdown) #DD
during_kurt_2 <- apply(during_set_2,2,kurtosis) #kurt
during_skew_2 <- apply(during_set_2,2,skewness) #skew
during_VaR_2 <- apply(during_set_2,2,VaR) #VaR
during_sharpe_2 <- apply(during_set_2,2,SharpeRatio.annualized) #SR

during_table_2 <- rbind(during_mean_2,during_vol_2,during_dd_2,during_kurt_2,during_skew_2,during_VaR_2,during_sharpe_2)

hist(during_st_2,main="During Crisis Strat 2 - S(60/40)")
hist(during_mt_2,main="During Crisis Strat 2 - S(90/40)")
hist(during_lt_2,main="During Crisis Strat 2 - S(180/90)")


## After crisis
#strategy 1

after_st <- return_1("2010-09-03","2022-03-31",60,40,2)
after_mt <- return_1("2010-09-03","2022-03-31",90,40,2)
after_lt <- return_1("2010-09-03","2022-03-31",180,90,2)

after_spy <- as.matrix(R_merge["2010-09-02/2022-03-24"][,1])

after_set <- cbind(after_spy,after_st,after_mt,after_lt)
colnames(after_set) <- c("SPY","st","mt","lt")

chart.CumReturns(after_set,wealth.index = TRUE,main="cumreturn after crisis - strategy 1",
                 colorset=c("Blue","orange","green","red"),ylim=c(0,50),legend.loc = "topleft")

after_mean <- apply(after_set,2,mean)*250 #daily return mean
after_vol <- apply(after_set,2,sd)*sqrt(250) #volatility
after_dd <- apply(after_set,2,maxDrawdown) #DD
after_kurt <- apply(after_set,2,kurtosis) #kurt
after_skew <- apply(after_set,2,skewness) #skew
after_VaR <- apply(after_set,2,VaR) #VaR
after_sharpe <- apply(after_set,2,SharpeRatio.annualized) #SR

after_table <- rbind(after_mean,after_vol,after_dd,after_kurt,after_skew,after_VaR,after_sharpe)

hist(after_st,main="After Crisis Strat 1 - S(60/40)")
hist(after_mt,main="After Crisis Strat 1 - S(90/40)")
hist(after_lt,main="After Crisis Strat 1 - S(180/90)")


#strategy 2
after_st_2 <- return_2("2010-09-03","2022-03-31",60,40,2)
after_mt_2 <- return_2("2010-09-03","2022-03-31",90,40,2)
after_lt_2<- return_2("2010-09-03","2022-03-31",180,90,2)


after_set_2 <- cbind(after_spy,after_st_2,after_mt_2,after_lt_2)
colnames(after_set_2) <- c("SPY","st","mt","lt")

chart.CumReturns(after_set_2,wealth.index = TRUE,main="cumreturn after crisis - strategy 2",
                 colorset=c("Blue","orange","green","red"),ylim=c(0,50),legend.loc = "topleft")

after_mean_2 <- apply(after_set_2,2,mean)*250 #daily return mean
after_vol_2 <- apply(after_set_2,2,sd)*sqrt(250) #volatility
after_dd_2 <- apply(after_set_2,2,maxDrawdown) #DD
after_kurt_2 <- apply(after_set_2,2,kurtosis) #kurt
after_skew_2 <- apply(after_set_2,2,skewness) #skew
after_VaR_2 <- apply(after_set_2,2,VaR) #VaR
after_sharpe_2 <- apply(after_set_2,2,SharpeRatio.annualized) #SR

after_table_2 <- rbind(after_mean_2,after_vol_2,after_dd_2,after_kurt_2,after_skew_2,after_VaR_2,after_sharpe_2)

hist(after_st_2,main="After Crisis Strat 2 - S(60/40)")
hist(after_mt_2,main="After Crisis Strat 2 - S(90/40)")
hist(after_lt_2,main="After Crisis Strat 2 - S(180/90)")

