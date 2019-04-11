# 11.1
fib <- function(thresh, printme) {
  fib.0 <- 1
  fib.1 <- 1
  result <- c()
  
  while (fib.0 < thresh) {
    result <- c(result, fib.0)
    n <- fib.0 + fib.1
    fib.0 <- fib.1
    fib.1 <- n
  }
  
  if (printme == TRUE) {
    cat(paste(result, collapse = ", "))
    cat("\n")
  }
  
  return (result)
}

fib(thresh=150,printme=TRUE)
fib(1000000,T)
fib(150,FALSE)
fib(1000000,printme=F)

fac <- function(n) {
  if (n < 0) return(NaN)
  
  result <- 1
  while (n > 0) {
    result <- result * n
    n <- n -1
  }
  return (result)
}

fac(5)
fac(12)
fac(0)
fac(-6)