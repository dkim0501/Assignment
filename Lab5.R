# Lab5
# FE 535
# Team 9

rm(list = ls())
library(quantmod)
library(PerformanceAnalytics)

getSymbols("DJFUELUSGULF",src="FRED",from="2000-09-01",to="2020-12-31")
Jet <- DJFUELUSGULF

FUT <- get(getSymbols("HO=F",from="2000-09-01",to="2020-12-31"))
FUT <- FUT$`HO=F.Adjusted`

P <- merge(Jet,FUT)
R <- na.omit(P/lag(P)-1)
chart.CumReturns(R)

vol_jet <- sd(R[,1])*sqrt(252)
vol_fut <- sd(R[,2])*sqrt(252)

getSymbols("SPY",from="2000-09-01",to="2020-12-31")
R_SPY <- na.omit(SPY/lag(SPY)-1)

vol_spy <- sd(R_SPY)*sqrt(252)
summary(lm(R$DJFUELUSGULF~R$HO.F.Adjusted))

R_N <- matrix(0,nrow=4875,ncol=100)

for(i in 1:100){
  R_N[,i] <- R[,1]-i*0.02*R[,2]
}

Rp_vol <- sqrt(252)*apply(R_N,2,sd)
plot(Rp_vol,xlab = "N",ylab = "volatility",type="l",ylim=c(0,0.5))
min(Rp_vol)
points(0,min(Rp_vol),col="red")
points(47,0,col="red")

vol_jet * sqrt(1-0.6462)
