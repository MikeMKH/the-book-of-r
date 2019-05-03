# probability of flipping heads 3 times in 5 flips
dbinom(3, 5, 0.5)
# probability of flipping heads 5 times in a row
dbinom(5, 5, 0.5)
# ... 100 times
dbinom(100, 100, 0.5)

# 16.1
stations = 13
p = 0.75

barplot(dbinom(x = 1:stations, size = stations, prob = p),
        names.arg = 0:stations, space = 0,
        xlab = "x", ylab = "Pr(X = x)")

dbinom(x = 13, size = stations, prob = p)

1 - pbinom(q = 9, size = stations, prob = p)

sum(dbinom(8:11, stations, p))
pbinom(11, stations, p) - pbinom(7, stations, p)

pbinom(8, stations, p)

visits <- rbinom(10, stations, p)

# probability of n successes
mu.X <- stations * p

# variance of n successes
sigma.X <- sqrt(mu.X * (1 - p))