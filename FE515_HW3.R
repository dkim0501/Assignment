# HW3
# FE 515
# Dong Woo Kim

#Q1
#1,1
rm(list = ls())
library(quantmod)

VIX.option <- getOptionChain("^VIX")

#1.2
getSymbols(Symbols="^VIX")
VIX <- getQuote("^VIX")$Last # get last quote price

#1.3
VIX.option$calls$Price <- 0.5*(VIX.option$calls$Bid + VIX.option$calls$Ask) #add Price col in call

VIX.option$puts$Price <- 0.5*(VIX.option$puts$Bid + VIX.option$puts$Ask) #add Price in put

#1.4
VIX.option$calls$In_The_Money <- VIX.option$calls$Strike < VIX # add In the Money column in call
VIX.option$puts$In_The_Money <- VIX.option$puts$Strike > VIX # add In the MOney column in put
# result is in csv file

#1.5
VIX.option$calls <- VIX.option$calls[c("Strike","Bid","Ask","Price","In_The_Money")]
VIX.option$puts <- VIX.option$puts[c("Strike","Bid","Ask","Price","In_The_Money")]

setwd("C:/Users/USER/Desktop/Stevens/FE515/HW")
write.csv(VIX.option$calls, file = "VIXdata2021-10-04Exp2021-10-08calls.csv")

#Q2
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

#2.2
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

#2.3

SPY <- "SPY"
P_SPY <- get(getSymbols(SPY,from="1990-01-01")) # download SPY
P_SPY <- P_SPY$SPY.Adjusted 
R_SPY <- na.omit(log(P_SPY/lag(P_SPY))) # log return of SPY
R_SPY <- R_SPY["2012-01-01/2013-12-31"] # get 2012, 2013 data

#2.4
SPY_skew_ad <- skew(R_SPY,TRUE) # adjusted skewness
SPY_skew_unad <- skew(R_SPY,FALSE) # unadjusted skewness
SPY_skew_ad
SPY_skew_unad

#2.5
SPY_kurt_ad <- kurt(R_SPY,TRUE) # adjusted kurtosis
SPY_kurt_unad <- kurt(R_SPY,FALSE) # unadjusted kurtosis
SPY_kurt_ad
SPY_kurt_unad
