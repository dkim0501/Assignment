# FE 630 WS
# Dong Woo Kim
# Midterm

rm(list = ls())
library(xts)

# Problem 1
# 1.1 load all data files
dataset <- setwd("C:/Users/USER/Desktop/Stevens/22SS/FE630/Midterm/data")
file_list <- list.files(dataset)
data <- lapply(file_list,read.table,header=FALSE)

names(data) <- c("AA","AXP","BA","BAC","CAT","CSCO","CVX","DD","DIS","GE","HD","HPQ","IBM","INTC","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","PFE","PG","T","TRV","UNH","UTX","VZ","WMT","XOM")

# 1.2 make Price mat P
P <- matrix(c(data$AA$V7,data$AXP$V7,data$BA$V7,data$BAC$V7,data$CAT$V7,data$CSCO$V7,data$CVX$V7,data$DD$V7,data$DIS$V7,data$GE$V7,data$HD$V7,data$HPQ$V7,data$IBM$V7,data$INTC$V7,data$JNJ$V7,data$JPM$V7,data$KO$V7,data$MCD$V7,data$MMM$V7,data$MRK$V7,data$MSFT$V7,data$PFE$V7,data$PG$V7,data$T$V7,data$TRV$V7,data$UNH$V7,data$UTX$V7,data$VZ$V7,data$WMT$V7,data$XOM$V7),nrow=689)
colnames(P) <- c("AA","AXP","BA","BAC","CAT","CSCO","CVX","DD","DIS","GE","HD","HPQ","IBM","INTC","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","PFE","PG","T","TRV","UNH","UTX","VZ","WMT","XOM")

# 1.3 get the daily return matrix
for(i in 1:30){
  for(j in 1:688){
    P[j,i] <- (P[j+1,i]-P[j,i])/P[j,i] # compute daily simple return
  }
}

P <- P[-c(689),]


# 1.4 get mu for each stocks
mu <- apply(P,2,mean)

# 1.5 compute cov mat Q
Q <- cov(P)

# 1.6 save the input file
save(mu,Q,file="inputs.rData")


# Problem 2
# set the function port
library(quadprog)
port <- function(mu,Q,tau){
  D <- Q # cov matrix
  dvec <- matrix(tau*mu,nrow=nrow(Q),ncol=1) 
  A <- cbind(matrix(rep(1,length(mu))),diag(nrow(Q)),-diag(nrow(Q)))
  bvec <- c(1,rep(0,nrow(Q)),rep(-0.1,nrow(Q)))
  
  result <- solve.QP(D,dvec,A,bvec,meq=1)$solution  
  names(result) <- names(mu)
  
  return(result)
} 



# Problem 3
# 3.1 load the input.rData
load("input.rData")

# 3.2 create tau sequence
TAU <- seq(0,0.5,by=0.001)

# 3.3 run the loop
port_return <- NULL
port_sd <- NULL
for(i in 1:501){
  h <- port(mu,Q,TAU[i])
  port_return[i] <- mu %*% h
  port_sd[i] <- sqrt(t(h) %*% Q %*% h)
}

# 3.4 plot the efficient frontier
plot(port_sd,port_return,xlab="standard deviation",ylab="expected return")


# Problem 4
# 4.1 read the Dow-Jones data
setwd("C:/Users/USER/Desktop/Stevens/22SS/FE630/Midterm/Midtermdata")
load("data.rda")

# 4.2 get the daily return matrix
for(i in 1:31){ # make a for loop to get the simple daily return
  for(j in 1:249){
    prices[j,i] <- (prices[j+1,i]-prices[j,i])/prices[j,i] 
  }
}
prices_return <- prices[-c(250),] #delete the last column

# 4.3 annualize the return
annual_return <- 252*prices_return

# 4.4 move the index out of the mat
index <- annual_return[,1] # copy the index data
annual_return <- annual_return[,-1] # delete the index data

# 4.5 compute the Cov matrix
Qts <- cov(annual_return)

# 4.6 print out first 5 rows and columns
Qts[1:5,1:5]


# Problem 5
# 5.1 loop to regress
table <- NULL # make a empty table
for(i in 1:30){ 
  reg <- summary(lm(annual_return[,i]~index)) # get the each regression
  inter <- reg$coefficients[1,1] # interception
  coef <- reg$coefficients[2,1] # slope
  sd <- reg$sigma # idio standard deviation
  
  table <- rbind(table,c(inter,coef,sd)) # merge each data
}
colnames(table) <- c("Intercept","Slope","Idio sd")
rownames(table) <- colnames(annual_return)

# 5.2 print the table 
t(table) # print the table of each stocks with Intercept, Slope and idio sd

# 5.3 variance of index returns
var_index <- var(index)
var_index

# 5.4 compute single-index approx to the cov mat Qsi
beta <- matrix(table[,2],ncol=1) # get the beta matrix 30 rows and 1 column
Qsi <- var_index*beta %*% t(beta) + diag(table[,3]**2) # cov(ri,rj) = beta * beta^t * sigma^2 + diag(sigma^2)
colnames(Qsi) <- rownames(table)
rownames(Qsi) <- colnames(Qsi)

# 5.5 print the qsi
Qsi[1:5,1:5]


# Problem 6
# 6.1
mu_dow <- apply(annual_return,2,mean)
Qts_return <- NULL 
Qts_sd <- NULL
for(i in 1:501){ # make a efficient frontier with mu, Qts, and TAU
  h1 <- port(mu_dow,Qts,TAU[i])
  Qts_return[i] <- mu_dow %*% h1
  Qts_sd[i] <- sqrt(t(h1) %*% Qts %*% h1)
}

# 6.2
Qsi_return <- NULL 
Qsi_sd <- NULL
for(i in 1:501){ # make a efficient frontier with mu, Qsi, and TAU
  h2 <- port(mu_dow,Qsi,TAU[i])
  Qsi_return[i] <- mu_dow %*% h2
  Qsi_sd[i] <- sqrt(t(h2) %*% Qsi %*% h2)
}

# 6.3 plot the efficient frontier and compare Qts, Qsi
plot(Qts_sd,Qts_return,col="blue",type="l",xlim=c(2.065,2.09),ylim=c(0.02,0.055),xlab="standard deviation",ylab="expected Return")
lines(Qsi_sd,Qsi_return,col="red")
legend("bottomleft",c("TS model","Si model"),col=c("blue","red"),lwd=c(1,1))
