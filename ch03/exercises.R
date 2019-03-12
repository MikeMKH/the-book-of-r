# 3.1
a <- matrix(data = c(4.3, 3.1, 8.2, 8.2, 3.2, 0.9, 1.6, 6.5),
       nrow = 4, ncol = 2, byrow = TRUE)

dim(a[-1,])

a[,2] <- sort(a[,2])

matrix(data = a[-4,-2], ncol = 1)

e <- a[c(-1,-2),]

a[c(4,1),2:1] <- rep(x = diag(x=e) * 1/2, times = 2)