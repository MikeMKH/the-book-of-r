# 6.1
foo <- c(13563,-14156,-14319,16981,12921,11979,9568,8833,8133)
foo[is.finite(foo^75)]
foo[-which(foo^75 == -Inf)]

bar <- matrix(
  c(77875.4, -35466.25, -39803.81,
    27551.45, -73333.85, 55976.34,
    23764.3, 36599.69, 76694.82,
    -36478.88, -70585.69, 47032),
  nrow = 3, ncol = 4)
which(is.nan(bar^65/Inf))
bar[!is.nan(bar^67+Inf)]
bar[bar^67 != -Inf]
bar[bar^67 == -Inf | is.finite(bar^67)]

# 6.2
foo <- c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)
length(foo) # 8
which(x = is.na(x = foo)) # 4 8
is.null(x = foo) # FALSE
is.na(x = foo[8]) + 4 / NULL # numeric(0)

b <- list(alpha = c(7,7,NA,3,NA,1,1,5,NA))
is.null(b$beta)
b$beta = which(is.na(b$alpha))

# 6.3
is.array(foo <- array(data=1:36,dim=c(3,3,4)))
is.vector(bar <- as.vector(foo))
is.character(baz <- as.character(bar))
is.factor(qux <- as.factor(baz))
is.numeric(quux <- bar+c(-0.1,0.1))

foo.sum <- is.numeric(foo) + is.integer(foo)
bar.sum <- is.numeric(bar) + is.integer(bar)
baz.sum <- is.numeric(baz) + is.integer(baz)
qux.sum <- is.numeric(qux) + is.integer(qux)
quux.sum <- is.numeric(quux) + is.integer(quux)
factor(x = c(foo.sum, bar.sum, baz.sum, qux.sum, quux.sum),
       levels = c(0, 1, 2))
as.numeric(c(foo.sum, bar.sum, baz.sum, qux.sum, quux.sum))

c <- matrix(data = 2:13, ncol = 4, nrow = 3)
as.character(as.vector(c))

d <- cbind(c(34, 23, 33, 42, 41), c(0, 1, 1, 0, 0), c(1, 2, 1, 1, 2))
d <- as.data.frame(d)
d[,2] <- as.logical(d[,2])
d[,3] <- as.factor(d[,3])