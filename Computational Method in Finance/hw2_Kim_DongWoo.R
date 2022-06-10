# FE621
# HW2
# Dong Woo Kim

rm(list = ls())

# 1
# a)
# generate stock price tree
stock_tree <- function(S, sigma, delta_t, N) {
  tree <- matrix(0, nrow=N+1, ncol=N+1)
  u <- exp(sigma*sqrt(delta_t))
  d <- exp(-sigma*sqrt(delta_t))

  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

# calculate the q with u and d
q <- function(r, delta_t, sigma){
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp(r*delta_t) - d)/(u-d))
}

# generate option tree
option_tree <- function(tree, sigma, delta_t, r, X, type) {
  q <- q(r, delta_t, sigma)
  option_tree <- matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}

eu_option_binom <- function(type, sigma, T, r, X, S, N) {
  q <- q(r=r, delta_t=T/N, sigma=sigma)
  tree <- stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- option_tree(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}

eu_option_binom(type='call', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=5) ## European option
eu_option_binom(type='put', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=5)

# Generate american option tree
option_tree_am <- function(tree, sigma, delta_t, r, X, type) {
  q <- q(r, delta_t, sigma)
  option_tree <- matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      exercise.payoff <- if(type=='put'){
        max(X - tree[i,j],0)
      } else {
        max(tree[i,j] - X,0)
      }
      original.tree <- ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      option_tree[i,j] <- max(exercise.payoff,original.tree)
    }
  }
  return(option_tree)
}

am_option_binom <- function(type, sigma, T, r, X, S, N) {
  q <- q(r=r, delta_t=T/N, sigma=sigma)
  tree <- stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- option_tree_am(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}

am_option_binom(type='call', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=5) ## American option
am_option_binom(type='put', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=5)

# b)

nSteps <- numeric()
am_put <- numeric()
am_call <- numeric()
eu_put <- numeric()
eu_call <- numeric()

for(i in 1:100){
  n <- i + 1
  copt_am <- am_option_binom(type='call', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=n)
  popt_am <- am_option_binom(type='put', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=n)
  copt_eu <- eu_option_binom(type='call', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=n)
  popt_eu <- eu_option_binom(type='put', sigma=0.2, T=1, r=0.1, X=1780, S=1750, N=n)
  
  nSteps <- c(nSteps,n)
  am_put <- c(am_put,popt_am$price)
  am_call <- c(am_call,copt_am$price)
  eu_put <- c(eu_put,popt_eu$price)
  eu_call <- c(eu_call,copt_eu$price)
}

#American option convergence plot 100 steps
plot(nSteps,am_put,xlim=c(0,100),ylim=c(90,100),xlab='Steps', ylab='price',type='l',main='American put option')
abline(h=98.0184,col='blue') # put option price by using option calculator

plot(nSteps,am_call,xlim=c(0,100),ylim=c(205,225),xlab='Steps', ylab='price',type='l',main='American call option')
abline(h=214.5604,col='blue') # call option price by using option calculator

#European option convergence plot 100 steps
plot(nSteps,eu_put,xlim=c(0,100),ylim=c(65,85),xlab='Steps', ylab='price',type='l',main='European put option')
abline(h=75.4698,col='blue') # put option price from bs

plot(nSteps,eu_call,xlim=c(0,100),ylim=c(205,225),xlab='Steps', ylab='price',type='l',main='European call option')
abline(h=214.4012,col='blue') # put option price from bs


# 2
# a)
up_out_call_barrier <- function(tree, sigma, delta_t, r, X, H) {
  q <- q(r, delta_t, sigma)
  option_tree <- matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  option_tree[nrow(option_tree),] <- pmax(tree[nrow(tree),] - X, 0)
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] <- ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      
      if(tree[i,j]>H){
        option_tree[i,j] <- 0
      } else{
        option_tree[i,j] <- option_tree[i,j]
      }
    }
  }
  return(option_tree)
}

barrier_call <- function(sigma, T, r, X, S, N, H) {
  q <- q(r=r, delta_t=T/N, sigma=sigma)
  tree <- stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- up_out_call_barrier(tree, H=H, sigma=sigma, delta_t=T/N, r=r, X=X)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}

barrier_call(sigma=0.2, T=0.3, r=0.01, X=10, S=10, N=10, H=11)

b_call_price <- NULL
nSteps_b <- NULL

for(i in 1:100){
  n <- i + 1
  b_call <- barrier_call(sigma=0.2,T=0.3,r=0.01,X=10,S=10,N=n,H=11)
  nSteps_b <- c(nSteps_b,n)
  b_call_price <- c(b_call_price,b_call$price)
}


plot(nSteps,b_call_price,xlim=c(0,100),ylim=c(0,0.5),xlab='Steps', ylab='price',type='l',main='Up-and-Out call option with barrier H=11')
abline(h=0.05, col='blue') # up and out barrier call option price from closed form formula (bs model) is 0.05


# 3
option_tree_installment <- function(tree, sigma, delta_t, r, X, p) { # construct installment option tree with p
  q <- q(r, delta_t, sigma)
  option_tree <- matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  option_tree[nrow(option_tree),] <- pmax(tree[nrow(tree),]-X, 0)
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] <- ((1-q)*pmax(option_tree[i+1,j]-p,0) + q*(pmax(option_tree[i+1,j+1]-p,0)))/exp(r*delta_t)
    }
  }
  return(option_tree)
}

inst_option_call <- function(sigma, T, r, X, S, N, p) {
  q <- q(r=r, delta_t=T/N, sigma=sigma)
  tree <- stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- option_tree_installment(tree, sigma=sigma, delta_t=T/N, r=r, X=X, p=p)
  
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}

inst_option_call(0.2,1,0.04,90,100,4,4.2167) # installment call option price with sigma=0.2,T=1,r=0.04,X=90,S=100,n=4 and p=2)

nSteps_i <- NULL
i_call_price <- NULL

for(i in 1:5000){
  n <- i*0.001
  i_call <- inst_option_call(sigma=0.2,T=1,r=0.04,X=90,S=100,N=4,p=n)
  nSteps_i <- c(nSteps_i,n)
  i_call_price <- c(i_call_price,i_call$price)
}

plot(nSteps_i,i_call_price,xlim=c(0,5),ylim=c(0,15),xlab='p', ylab='price',type='l',main='V0(p) as a function of p')
lines(x=nSteps_i,y=nSteps_i,col='blue') # x=y plot

# find intersect point
above <- i_call_price > nSteps_i # p =4.2167

# c)
eu_option_binom(type='call', sigma=0.2, T=1, r=0.04, X=90, S=100, N=4) # Eu call with same parameter, price=16.40643
sum_di <- 4.2167*(exp(-0.04*0*1/4)+exp(-0.04*1/4)+exp(-0.04*2/4)+exp(-0.04*3/4)) # sum of discounted installment payments, price = 16.61
