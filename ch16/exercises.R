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

# 16.2
1 - ppois(q = 100, lambda = 107)

dpois(x = 0, lambda = 107)

barplot(ppois(60:150, 107), ylim = 0:1, space = 0,
        names.arg = 60:150, ylab = "Pr(X=x)", xlab = "x")

traffic <- rpois(n = 260, lambda = 107)
hist(traffic, xlim = c(60, 150))

# 16.3
trees.min = 3
trees.max = 70

punif(q = 5.5, min = trees.min, max = trees.max)

b <- qunif(p = 1 - .15, min = trees.min, max = trees.max)
b

trees.mu = (trees.min + trees.max) / 2
trees.sigma = sqrt((trees.max - trees.min)^2 / 12)

punif(trees.mu + .5 * trees.sigma, trees.min, trees.max) -
  punif(tree.mu - .5 * trees.sigma, trees.min, trees.max)

d <- dunif(trees.mu, trees.min, trees.max)
plot(c(trees.min, trees.max), rep(d, 2),
     type = "o", pch = 19,
     xlim = c(trees.min - 1, trees.max + 1), ylim = c(0, d),
     ylab = "f(x)", xlab = "x")
abline(h = 0, lty = 2)
segments(c(trees.min - 5, trees.max + 5, trees.min, trees.max),
         rep(0, 4), rep(c(trees.min, trees.max), 2),
         rep(c(0, d), each=2), lty = rep(1:2, each = 2))
points(c(trees.min, trees.max), c(0, 0))

# sim.b seems to be closer to b
sim.a <- runif(n = 10, min = trees.min, trees.max)
quantile(x = f.a, probs = 1 - .15)

sim.b <- runif(1000, trees.min, trees.max)
quantile(f.b, probs = 1 - .15)

# 16.4
time.mu = 17
time.sigma = 4.5

1 - pnorm(20, time.mu, time.sigma)
pnorm(10, time.mu, time.sigma) - pnorm(5, time.mu, time.sigma)
qnorm(1 - .1, time.mu, time.sigma)

xvals <- seq(time.mu - 4 * time.sigma,
             time.mu + 4 * time.sigma,
             length = 10000)
fx <- dnorm(xvals, time.mu, time.sigma)
slow10 <- qnorm(1 - .1, time.mu, time.sigma)
xvals.sub <- xvals[xvals >= slow10]
fx.sub <- fx[xvals >= slow10]
plot(xvals, fx, type = "l", main = "N(17,4.5) distribution",
     xlab = "x", ylab = "f(x)")
abline(h = 0, col = "gray")
abline(v = slow10, lty = 2)
polygon(rbind(c(slow10, 0),
              cbind(xvals.sub, fx.sub),
              c(max(xvals), 0)),
        border = NA, col = "gray")

rnorm(10, time.mu, time.sigma)

blades.mu <- 10
blades.sigma <- 2

pnorm(11, blades.mu, blades.sigma) -
  pnorm(9.5, blades.mu, blades.sigma)

pnorm((11 - blades.mu) / blades.sigma) -
  pnorm((9.5 - blades.mu) / blades.sigma)

b.iii <- qnorm(.025, blades.mu, blades.sigma)
b.iii

(b.iii - blades.mu) / blades.sigma