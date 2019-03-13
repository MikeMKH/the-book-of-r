# 3.1
a <- matrix(data = c(4.3, 3.1, 8.2, 8.2, 3.2, 0.9, 1.6, 6.5),
       nrow = 4, ncol = 2, byrow = TRUE)

dim(a[-1,])

a[,2] <- sort(a[,2])

matrix(data = a[-4,-2], ncol = 1)

e <- a[c(-1,-2),]

a[c(4,1),2:1] <- rep(x = diag(x=e) * 1/2, times = 2)

# 3.2
2/7 * (
  matrix(data = c(1, 2, 7, 2, 4, 7), ncol = 2)
  - matrix(data = c(10, 30, 50, 20, 40, 60), ncol = 2))

A <- matrix(data = c(1, 2, 7), ncol = 1)
B <- matrix(data = c(3, 4, 8), ncol = 1)

# A %*% B # non-conformable arrays
t(A) %*% B
t(B) %*% (A %*% t(A))
# (A %*% t(A)) %*% t(B) # non-conformable arrays
((B %*% t(B)) + (A %*% t(A))) - 100 * diag(x = 3)

A <- diag(x = c(2, 3, 5, -1))
identical(x = A %*% solve(A), y = diag(x = 4))

# 3.3
A <- array(data = seq(
  from = 48, to = 0.1, length.out = 4 * 2 * 6),
  dim = c(4, 2, 6))

B <- A[c(4,1),2,]

C <- array(data = rep(x = B[2,], times = 4),
           dim = c(2, 2, 2, 3))

D <- A[,,-6]

D <- D[c(2, 4), 2, c(1, 3, 5)] <- -99