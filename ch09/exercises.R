# 9.1
ls("package:methods")[1:20]
length(ls("package:methods"))

environment(fun = read.table)
environment(fun = data)
environment(fun = matrix)
environment(fun = jpeg)

any(ls("package:graphics") == "smoothScatter")

# 9.2
seq(-4, 4, 0.2)

array(8:1, dim = c(2, 2, 2)) # positional and partital
rep(1:2, 3) # positional
seq(from = 10, to = 8, length = 5) # partital
sort(decreasing = T, x = c(2, 1, 1, 2, 0.3, 3, 1.3)) # full
which(matrix(c(T, F, T, T), 2, 2)) # positional
which(matrix(c(T, F, T, T), 2, 2), a = T) #positional

plot.default(type = "p", pch = NULL, xlab = "X", ylab = "Y",
             lwd = NULL, lty = NULL, col = NULL,
             x = c(1, 2), y = c(3, 4))
# pch, lwd, lty, col are arguments that full under ...