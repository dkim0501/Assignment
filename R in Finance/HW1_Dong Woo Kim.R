# HW1
# FE 515
# Name: Dong Woo Kim
# CWID: 10474401

# Q1
# 1.1
x <- seq(5, 35, by = 2) #generate vector x from 5 to 35 with increment 2
length(x) #calculate x's length

# 1.2
A <- matrix(x, nrow=4, ncol=4, byrow = T) # create the matrix using vector x by row

# 1.3
eigen(A)$values # calculate eigenvalues of A

# 1.4
A[1:2, 1:2] <- 7 # change the value of A11, A12, A21, A22 to 7

# 1.5
det(A) # Calculate determinant of A

# 1.6
solve(A) # Calculate inverse of A

# 1.7
b <- A[1,] # assign the first row of A to vector b

# 1.8
y <- solve(A)%*%b # Find y by solving linear equation Ay = b

# 1.9
pmin(y,pi/2) # Calculate the mininum of each element in y and pi/2

# 1.10
diag(1:10,10,10) # Generate 10 by 10 matrix by using diag function

# Q2
S <- rep(NA,50) # Set S0 = 0, S1 = 1
S[1] <- 0
S0 <- S[1]
S[2] <- 1

for(n in 2:50) # for loop
   S[n+1] <- S[n] + S[n-1] # Generate Fibonacci sequence

S[4] # Determine S3
S[51] # Determine S50

# Q3
for(i in 1:100) # for loop between 1 to 100
  if(i %% 3 == 0 && i %% 5 == 0) # get the number which are divisible by both 3 and 5
    print(i)

# Q4

mydiv <- function(n) { # function, Creating vector which contain all numbers 1:n which are divisible by 3 and 5
  S <- NULL # make null space
  i <- 1 # starting point
  while(i <= n){ # loop to find numbers which are divisible by a and b
    if(i %% 3 == 0 && i %% 5 == 0)
      S <- c(S, i) # put the numbers in vector
    i <- i + 1
  }
  return (S)
}

mydiv(100) # get all numbers in 1:100, divisible by 3 and 5
mydiv(200) # get all numbers in 1:200, divisible by 3 and 5

# Q5

mysmall <- function(a,b) { # function, creating vector and find smallest number which is divisible by a and b
  N <- a*b # max number with smallest value
  S <- NULL # make null space
  i <- 1 # starting point
  while(i <= N){ # loop to find numbers which are divisible by a and b
    if( i %% a == 0 && i %% b == 0)
      S <- c(S, i) 
    i <- i +1
  }
  return(min(S)) # find the smallest number
}

mysmall(3,5) # get the smallest number which is divisible by 3 and 5
mysmall(6,10) # get the smallest number which is divisible by 6 and 10

# Q6

setwd("C:/Users/USER/Desktop/Stevens/FE515/HW")
getwd()
JPM <- read.csv("JPM.csv") # read the JPM.csv

subtable <- JPM[,2:5] # make subtable which only contains Open, High, Low, Close
sapply(subtable,mean) # sapply to calculate the mean of each column

