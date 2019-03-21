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