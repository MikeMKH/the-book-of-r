# 12.1
fac <- function(x) {
  if (x < 0) {
    warning("x is negative, it is assumed you want a positive number")
    x <- x * -1
  }
  
  result <- 1
  for(n in 1:x) {
    result <- result * n
  }
  
  return (result)
}

fac(5)
fac(8)
fac(-8)

invert <- function(x, noninv = NA, nonmat = "not a matrix", silent = TRUE) {
  if (!is.list(x)) {
    stop("x is not a list")
  }
  if (length(x) == 0) {
    stop("x must have at least one member")
  }
  nonmat <- try(as.character(nonmat), silent)
  
  for(i in 1:length(x)) {
    if (is.matrix(x[[i]])) {
      attempt <- try(solve(x[[i]]), silent)
      if (class(attempt) == "try-error") {
        x[[i]] = noninv
      } else {
        x[[i]] = attempt
      }
    }
  }
  
  return (x)
}

x <- list(1:4,matrix(1:4,1,4),matrix(1:4,4,1),matrix(1:4,2,2))
invert(x)
invert(x = x, noninv = Inf, nonmat = 666)
invert(x = x, noninv = Inf, nonmat = 666, silent = FALSE)
x <- list(diag(9),matrix(c(0.2,0.4,0.2,0.1,0.1,0.2),3,3),rbind(c(5,5,1,2),c(2,2,1,8),c(6,1,5,5),c(1,0,2,0)),matrix(1:6,2,3),cbind(c(3,5),c(6,5)),as.vector(diag(2)))
invert(x, "unsuitable matrix")
invert(x = "hello")
invert(x = list())

# 12.2
prog.test.fancy <- function(n, ...) {
  result <- 0
  prog.bar <- txtProgressBar(min = 0, max = n, ...)
  for (i in 1:n) {
    result <- result + 1
    Sys.sleep(0.25)
    setTxtProgressBar(prog.bar, value = i)
  }
  return (result)
}

s <- Sys.time()
prog.test.fancy(8, style = 3, char = "r")
e <- Sys.time()
e - s

# I think this is more interesting
fib.Progress <- function(x, ...) {
  if (x < 0) {
    warning("x is negative, it is assumed you want a positive number")
    x <- x * -1
  }
  
  if (x = 0) {
    x <- 1
  }
  
  progress <- txtProgressBar(min = 0, max = x, ...)
  result <- 1
  for(n in 1:x) {
    setTxtProgressBar(progress, n)
    result <- result * n
  }
  
  return (result)
}

fac.Progress(50)
sapply(
  X = c(3,2,7,0,9,13),
  FUN = function(n) { fac.Progress(n, style = 3, char = ".") })

fib <- function(n) {
  if (n <= 2) {
    return (1)
  }
  results <- c(1, 1, rep(NA, n))
  for (i in 3:n) {
    results[i] <- results[i - 2] + results[i - 1]
  }
  
  return (results[n])
}
fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(50)