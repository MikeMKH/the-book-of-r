# 16.1
barplot(dbinom(x = 1:13, size = 13, prob = 0.75),
        names.arg = 0:13, space = 0,
        xlab = "x", ylab = "Pr(X = x)")

dbinom(13, 13, 0.75)

1 - pbinom(q = 9, size = 13, prob = 0.75)

sum(dbinom(8:11, 13, 0.75))
pbinom(11, 13, 0.75) - pbinom(7, 13, 0.75)

pbinom(8, 13, 0.75)

visits <- rbinom(10, 13, 0.75)

mu.X <- 13 * 0.75
sigma.X <- sqrt(mu.X * .25)