# Fibonacci series generator
fib <- function(n){
  i <- 0
  j <- 1
  x <- numeric()
  for (k in 1:n) {
    x[k] <- j
    t <- i + j
    i <- j
    j <- t
  }
  return(x)
}

# Test of function
fib(10)
# [1]  1  1  2  3  5  8 13 21 34 55

# Test of function with n = 20
fib(20)
# [1]    1    1    2    3    5    8   13   21   34   55   89  144  233  377  610  987 1597 2584 4181 6765