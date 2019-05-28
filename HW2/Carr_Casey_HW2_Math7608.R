#Assignment# 02
#Statistical Programming with R
#Date: 02/26/2018
#Author: Casey Carr

# Questions: 1, 5, 7, 12, 13

# Question 1: Consider the function y = f(x) such that y = -x^3 for x<= 0, y = x^2 for x in (0,1], y = sqrt(x) for x > 1
#             Supposing that you are given x, write an expression for y using if-statements.
# input: 
x.values <- seq(-2,2,by = 0.1)
# for each x calculate y
n <- length(x.values)
y.values <- rep(0,n)
for (i in 1:n) {
  x <- x.values[i]
  if (x<=0) {
  y = -x^3
} else if (x > 0 && x <= 1) {
  y = x^2
} else {
  y = sqrt(x)
}
 y.values[i] <- y 
}
# output
plot(x.values, y.values, type = 'l')

# Does the plot look like figure 3.2? Do you think f has a derivative at x = 1? x = 0?
# The derivative of f at x = 1 does NOT exist since there exists a "corner" at that point
# The derivative at x = 0 EXISTS 

# Question 5: To rotate a vector (x,y)^T widdershins (anticlockwise) by theta radians, you premultiply it by
#             the matrix (cos(theta) -sin(theta); sin(theta) cos(theta)), where ';' indicates a carriage return to the next row. 
#             Write an R program that does this for you.

rotateVector <- function(vectorToRotate, thetaRadians) {
  rotationMatrix <- matrix(c(cos(thetaRadians), -sin(thetaRadians), sin(thetaRadians), cos(thetaRadians)), byrow = TRUE, ncol = 2)
  result <- rotationMatrix %*% vectorToRotate
  return(result)
}
# enter any vector in R2 and theta in radians to return the rotated solution
rotateVector(c(1,2),pi/2)


# Question 7: How would you find the sum of every third element of a vector X?
vectorA = c(1,2,4,5,6,7,9,10,11,12) # result = 22
sum(vectorA[(1:length(vectorA))%%3==0]) # iterates through vectorA and only sums the elements at indices divisible by 3

' Question 12: The dice game craps is played as follows. The player throws two dice, and if the sum is seven or
eleven, then he wins. If the sum is two, three, or twelve, then he loses. If the sum is anything else, then he 
continues throwing until he either throws that number again (in which case he wins) or he throws a seven
(in which case he loses).
  Write a program to simulate a game of craps. You can use the following snippet of code 
  to simulate the roll of two (fair) die: x <- sum(ceiling(6*runif(2))) 
'
playDice <- function() {
  currentPoint <- sum(ceiling(6*runif(2)))
  
  if (currentPoint == 7 || currentPoint == 11) {
    return("First roll win!")
  
  } else if (currentPoint == 2 || currentPoint == 3 || currentPoint == 12) {
    return("First roll crap out! You lose.")
  } else {
    point <- 0
    while (point != currentPoint) {
      point <- sum(ceiling(6*runif(2)))
      if (point == 7) {
        return("You lose!")
      } else if (point == currentPoint) {
        return("You win!")
      }
    }
  }
}
playDice()

' Question 13: Suppose that (x(t), y(t)) has polar coordinates (sqrt(t),2pi*t). Plot (x(t),y(t)) for t in [0,10]
               Note: in Polar Coordinates we use (r,theta), thus r = sqrt(t), theta = 2*pi*t 
               Therefore, x = rcos(theta) = sqrt(t)*cos(2*pi*t) and
                          y = rsin(theta) = sqrt(t)*sin(2*pi*t)
'
t.values <- seq(0,10, by = 0.01)
n <- length(t.values)
x.values <- rep(0, n) #initalize each of these vectors
y.values <- rep(0, n)
for (i in 1:n) {
  x.values[i] <- sqrt(t.values[i])*cos(2*pi*t.values[i])
  y.values[i] <- sqrt(t.values[i])*sin(2*pi*t.values[i])
}
plot(x.values,y.values, type = "l")
