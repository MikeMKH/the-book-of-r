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

# 11.2
compundInterest <- function(P, i, t = 12, y, plotit = TRUE, ...) {
  seq.y <- 1:y
  seq.F <- P * (1 + i / (100 * t))^(t * seq.y)
  if (plotit == TRUE) {
    plot(type = "s", x = seq.y, y = seq.F, ...)
  } else {
    return (seq.F)
  }
}

compundInterest(y = 10, P = 5000, i = 4.4, plotit = FALSE)[10]
compundInterest(y = 20, P = 100, i = 22.9, plotit = TRUE, t = 12,
                main="Compound interest calculator",
                ylab="Balance (F)", xlab="Year (y)")
iii <- compundInterest(y = 20, P = 100, i = 22.9, plotit = TRUE, t = 1)
lines(1:20, iii, lty=2, type="s")
legend("topleft", lty=c(1,2), legend=c("monthly interest","annual interest"))

realRoots <- function(k1, k2, k3) {
  if (any(c(missing(k1), missing(k2), missing(k3)))) {
    return("missing argument(s)")
  }
    
  test <- k2^2 - (4 * k1 * k3)
  if (test < 0) {
    cat("no real roots")
  } else if (test == 0) {
    return (-k2 / (2 * k1))
  } else {
    return(c((-k2 - test^0.5) / (2 * k1),
             (-k2 + test^0.5) / (2 * k1)))
  }
}

realRoots(k1 = 2, k2 = -1, k3 = -5)
realRoots(1, 1, 1)

realRoots(k1 = 1.3, k2 = -8, k3 = -3.13)
realRoots(2.25, -3, 1)
realRoots(1.4, -2.2, -5.1)
realRoots(-5, 10.11, -9.9)

realRoots()
realRoots(0)
realRoots(0, 0)

# 11.3
foo <- list("a",c("b","c","d","e"),"f",c("g","h","i"))
lapply(X = foo, FUN = function(s) {paste(s, "!")})

fac.rec <- function(n) {
  if (n < 1) {
    return (1)
  }
  
  return (n * fac.rec(n - 1))
}

fac.rec(5)
fac.rec(12)
fac.rec(0)
fac.rec(-6)

geolist <- function(x) {
  g <- function(vec) {
    return (prod(vec)^(1 / length(vec)))
  }
  
  for(i in 1:length(x)) {
    if (is.vector(x[[i]])) {
      x[[i]] <- g(x[[i]])
    } else if (is.matrix(x[[i]])) {
      x[[i]] <- apply(X = x[[i]], MARGIN = 1, FUN = g)
    } else {
      x[[i]] <- N/A
    }
  }
  return (x)
}

geolist(list(
  1:3,
  matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),4,2),
  matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),2,4)))
geolist(list(
  1:9,
  matrix(1:9,1,9),
  matrix(1:9,9,1),
  matrix(1:9,3,3)))