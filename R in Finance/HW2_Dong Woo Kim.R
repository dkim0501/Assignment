# HW2
# FE 515
# Dong Woo Kim

# Q1
# 1.1
seed <- 1
lcg <- function(n){ #Set lcg using m=244944, a=1597, b=51749
  m <- 244944
  a <- 1597
  b <- 51749
  x <- rep(NA,n)
  x[1] <- (a*seed+b) %% m
  for(i in 1:(n-1)){
    x[i+1] <- (a*x[i]+b) %% m
  }
  seed <<- x[n]
  return(x/m)
}

# 1.2
U <- lcg(10000) # 10000 uniform r.v
X <- qchisq(U,df=10) # X follows chi-square distribution

# 1.3
hist(X, nclass = 40) # histogram previous chisquare with 40 cells
hist(rchisq(10000,df=10), 40) # random number chisquare for comparing


# Q2
# Estimate the volume of Sphere

N <- 10000 # total number of points
x <- lcg(N) 
y <- lcg(N)
z <- lcg(N)

n_pts <- 0 # number of points which are inside of sphere
for(i in 1:N){
  if(x[i]^2 + y[i]^2 + z[i]^2 <=1){
    n_pts <- n_pts + 1
  }
}

n_pts <- sum(x^2 + y^2 + z^2 <= 1) # vectorized
sample_sphere_vol <- n_pts/N # volume of 1/8 unit sphere
vol_sphere <- 8 * sample_sphere_vol # volume of unit sphere
vol_sphere # result: Estimated volume of unit sphere

# Q3
# 3.1
setwd("C:/Users/USER/Desktop/Stevens/FE515/HW")
getwd()
JPM <- read.csv("JPM.csv") # read the JPM.csv 

JPM$Date <- as.Date(JPM$Date) # change to the date
par(mfcol = c(2,2)) # setup 2 by 2 matrix filled by columns

# 3.2
plot(JPM$Date, JPM$Adj.Close, type="l", main="JPM", xlab="Date", ylab="Adjusted Close Price", col="red") # Generate Plot

# 3.3
plot(JPM$Open, JPM$Close, xlab="Open Price", ylab="Close Price") # Generate Scattor plot

# 3.4
barplot(table(cut(JPM$Adj.Close, breaks=4))) # divide 4 intervals and generate barplot

# 3.5
boxplot(JPM$Adj.Close ~ cut(JPM$Adj.Close, breaks=4)) # Generate box plot against 4 intervals
