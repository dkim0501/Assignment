
rm(list=ls())
library("quantmod")
library("PerformanceAnalytics")
library("rpart")
library("writexl")
library("xlsx")


ticker <- c("DGS2","DGS5","DGS10","DGS30")

getSymbols(ticker,src="FRED",from="2018-01-02",to="2021-11-04")

DGS2 <- na.omit(DGS2["2017-12-29/2021-11-04"])
DGS5 <- na.omit(DGS5["2017-12-29/2021-11-04"])
DGS10 <- na.omit(DGS10["2017-12-29/2021-11-04"])
DGS30 <- na.omit(DGS30["2017-12-29/2021-11-04"])

setwd("C:/Users/USER/Desktop/Stevens/FE535/Project2")
getwd()
P_merge <- merge(DGS2,DGS5,DGS10,DGS30)
R_merge <- na.omit(log(P_merge/lag(P_merge)))
Bond <- read.csv(file='bond.csv')
Bond



Pr <- c(0)
Price <- rep(0,length(Bond[,1]))

Bpr <- function(Coupon,Yield,Maturity){
  Yield <- Yield / 100

  for (i in 1:Maturity){
    Pr <- Pr + Coupon / ((1+Yield)^i)
    }
  Pr <- Pr + 100/((1+Yield)^Maturity)
  
  return(Pr)
}


for (i in 1:length(Bond[,1])){
  
  Price[i] <- Bpr(Bond[i,2],Bond[i,4],Bond[i,5])
  
  }

Price

plot(Bond[,3],Price,xlab="Price(Reported)",ylab="Price(Calculated)")
abline(coef=c(0,1),col="blue")

Bond
