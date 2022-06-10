# FE 515
# Dong Woo Kim
# Midterm

rm(list = ls())
library(quantmod)

#1.1
getSymbols("JPM") #download data
getSymbols("WFC")
jpm <- data.frame(JPM) 
wfc <- data.frame(WFC)

jpm.price <- jpm$JPM.Adjusted #get Adjusted close price
wfc.price <- wfc$WFC.Adjusted

r_jpm <- diff(log(jpm.price)) #get og return
r_wfc <- diff(log(wfc.price))
r_jpm
r_wfc

#1.2
lm1 <- lm(r_jpm ~ r_wfc) # simple linear regession y = JPM, x = WFC
summary(lm1) # summarize the regression

#1.3
plot(r_jpm ~ r_wfc)
abline(lm1, col="red")

#1.4
setwd("C:/Users/USER/Desktop/Stevens/FE515/Midterm")
cheese <- read.csv("cheese.csv")

cheese.model <- lm(taste ~ acetic + h2s + lactic, data=cheese) # build multiple regression with 3 factors
null.model <- lm(taste ~ 1, data=cheese) # build null model
summary(cheese.model) #summarize 3 factor model
summary(null.model) #summarize null model

#1.5
full.model.formula <- taste ~ acetic + h2s +lactic

step(null.model, full.model.formula, direction = "forward") # forward selection
step(cheese.model, full.model.formula, direction = "backward") # backward selection
step(null.model, full.model.formula, direction = "both") # both selection

# According to these stepwise regression, the best model is
# taste = 3.946*h2s + 19.887*lactic - 27.592