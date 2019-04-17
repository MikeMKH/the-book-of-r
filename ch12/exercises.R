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
