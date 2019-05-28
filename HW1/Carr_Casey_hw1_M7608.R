#Assignment# 01
#Statistical Programming with R
#Date: 02/19/2018
#Author: Casey Carr
#Question 1: Give R assignment statements that set the variable z to
b = 2
a = 3
x = 4.3752
#1(a)
z <- x**(a**b);z
# [1] 587463.9
#1(b)
z2 <- (x^a)^b;z2
# [1] 7014.35
.Last.value * x ^ 3
#[1] 587463.9
#1(c)
z3 <- 3*(x^3) + 2*(x^2) + 6*x + 1;z3 
#[1] 316.7911
#1(d)
z4a <- (x%%1);z4
#[1] 0.3752
z4b <- (x%%1*10);z4
#[1] 3.752
z4c <- ((x%%1*10)%%1*10);z4
#[1] 7.52
z4d <- ((x%%1*10)%%1*10)%/%1;z4
#[1] 7
#1(e)
z4f <- z4f + 1; z4f
#[1] 8

#Question 2: Give R expressions that return the following matrices and vectors
#2(a):
seqSplit = c(seq(1,8),seq(8,1))
#2(b):
seqRep = rep(1:5, 1:5)
#2(c):
converseIdentity = matrix(c(0,1,1,1),byrow = TRUE, ncol = 3, nrow = 3)
#2(d):
selectMatrix = matrix(c(0,2,3,0,5,0,7,0,0),byrow = TRUE,ncol = 3)
selectMatrix

#Question 3:Suppose vec is a vector of length 2. Interpreting vec as the coordinates of a
#           point in R2, use R to express it in polar coordinates.
vec = c("x","y")
r = sqrt(x^2+y^2)
d = atan2(y,x)
polarCoordinate = c(r,d)

#Question 4: Use R to produce a vector containing all integers from 1 to 100 
# that are not divisible by 2, 3, or 7.
pickyList = c(1:100)
pickyList = ifelse(!(pickyList %% 2 == 0 | pickyList %% 3 == 0 | pickyList %% 7 == 0), pickyList, 0)
pickyList = pickyList[pickyList != 0]
pickyList

#Question 5: Suppose that queue ("Steve"; "Russell"; "Alison"; "Liam") represents a supermarket queue with 
#           Steve first in line. Using R expressions update the supermarket queue as successively:
supermarketQueue = c("Steve","Russell", "Alison", "Liam")
#5(a): Barry arrives
supermarketQueue = c(supermarketQueue, "Barry")
#5(b): Steve is served
supermarketQueue = supermarketQueue[-1]
#5(c): Pam talks her way to the front with one item
supermarketQueue = c("Pam", supermarketQueue)
#5(d): Barry gets impatient and leaves
supermarketQueue = supermarketQueue[-length(supermarketQueue)]
#5(e): Alison gets impatient and leaves.
supermarketQueue = supermarketQueue[supermarketQueue != "Alison"]
#5(f): Finally, find the position of Russell in the queue.
positionFinal = which(supermarketQueue == 'Russell')

#Question 6: Which of the following assignments will be successful? What will the vectors x, y, and
#           z look like at each stage?
# rm(list = ls()) - creates a list containing the (global enviornment) variable stack and removes it
# x <- 1 - assigns 1 to x, (y, z same)
# x[3] <- 3 - doesn't work if variable x not previously assigned a value, else generates : 1 NA 3, (y, z same)
# y <- c() - creates an empty list for y variable, (x, z same)
# y[2] <- 2 - appends the number 2 to index 2, fills in preceding indices with "NA", (x, z same)
# y[3] <- y[1] - sets index 3 to "NA", (x, z same)
# y[2] <- y[4] - sets index 2 to the value located at index 4 == "NA", (x, z same)
# z[1] <- 0 - no object 'z' found, (y, z same)

