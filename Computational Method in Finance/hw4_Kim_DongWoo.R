## FE 621
## Hw 4
## Dong Woo Kim

rm(list = ls())

# Problem 1
# c
# parameters
S0 <- 50
K <- 60
sigma <- 0.3
T <- 0.25
r <- 0.05
H <- 55
m <- 63
n <- 100000
h <- T/m

S.mat <- matrix(0, nrow = m+1, ncol = n)
Z <- matrix(rnorm(m*n), nrow = m)
S.mat[1,] <- S0

for(i in 1:m) {
  S.mat[i+1,] <- S.mat[i,]*exp((r-sigma^2/2)*h + sigma*sqrt(h)*Z[i,])
}

Sk <- S.mat[64,]

for(i in 1:n){
  for(j in 1:m){
    if(S.mat[j,i]>=H){
      S.mat[64,i] <- 0
    }
  }
}
B.mat <- pmax(0,60-S.mat[64,])
for(i in 1:n){
  if(B.mat[i] == 60){
    B.mat[i] <- 0
  }
}

mean(B.mat)*exp(-r*T)
1.96*sd(B.mat)/sqrt(n)

# e rebate
R.mat <- pmax(0,60-S.mat[64,])
for(i in 1:n){
  if(R.mat[i] == 60){
    R.mat[i] <- 5
  }
}

mean(R.mat)*exp(-r*T)
sd(R.mat)*1.96/sqrt(n)


# Problem 2
# a
# p(0,S0,T,S(k)) = exp((2/sigma^2)/T * ln(H/S0) * ln(S(k)/H)
ubB.mat <- B.mat
for(i in 1:n){
  if(Sk[i] > H){
    Sk[i] <- 0
  }
}
unb.B <- pmax(K-Sk,0)
for(i in 1:n){
  if(unb.B[i] == 60){
    unb.B[i] <- 0
  }
}
for(i in 1:n){
  ubB.mat[i] <- unb.B[i]*(1-exp(log(H/S0)*log(Sk[i]/H)*2/(T*sigma^2))) #unbiased estimator
}
mean(ubB.mat)*exp(-r*T)
1.96*sd(ubB.mat)/sqrt(n) #confidence interval

# b
# rebate with unbiased estimator

R.ub <- pmax(K-Sk,0)
for(i in 1:n){
  if(R.ub[i] == 60){
    R.ub[i] <- 5
  }
}

for(i in 1:n){
  R.ub[i] <- R.ub[i]*(1-exp(log(H/S0)*log(Sk[i]/H)*2/(T*sigma^2)))+5*exp(log(H/S0)*log(Sk[i]/H)*2/(T*sigma^2)) #unbiased estimator
}


mean(R.ub)*exp(-r*T)
1.96*sd(R.ub)/sqrt(n)

