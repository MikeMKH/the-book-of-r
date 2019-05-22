# 18.1
a <- (3.97 - 3.5) / (2.21 / sqrt(73))
pt(-a, df = 72) + (1-pt(a, df = 72)) > .05 # TRUE = insufficient evidence

t.test(quakes$mag, alternative = "greater",
       mu = 4.3, conf.level = 1 - .01)

mean(quakes$mag) + c(-1, 1) *
  qt(0.995, df = 999) * sd(quakes$mag) / sqrt(1000)

# 18.2
library("MASS")
?anorexia
summary(anorexia)

t.test(anorexia$Postwt, anorexia$Prewt,
       conf.level = .95, alternative = "greater", paired = TRUE) # strong evidence

t.test(anorexia$Postwt[anorexia$Treat == "Cont"], anorexia$Prewt[anorexia$Treat == "Cont"],
       conf.level = .95, alternative = "greater", paired = TRUE) # no evidence
t.test(anorexia$Postwt[anorexia$Treat == "CBT"], anorexia$Prewt[anorexia$Treat == "CBT"],
       conf.level = .95, alternative = "greater", paired = TRUE) # some evidence
t.test(anorexia$Postwt[anorexia$Treat == "FT"], anorexia$Prewt[anorexia$Treat == "FT"],
       conf.level = .95, alternative = "greater", paired = TRUE) # strong evidence

?PlantGrowth
summary(PlantGrowth)

growth.control <- PlantGrowth$weight[PlantGrowth$group == "ctrl"]
growth.treated <- PlantGrowth$weight[PlantGrowth$group != "ctrl"]

max(c(sd(growth.control), sd(growth.treated))) /
  min(c(sd(growth.control), sd(growth.treated))) < 2

t.test(x = growth.control, y = growth.treated,
       alternative = "less", var.equal = TRUE) # no evidence

wrap.t.test <- function(x, y, paired = FALSE, var.equal = FALSE, ...) {
  if (!paired) {
    sd.x <- sd(x)
    sd.y <- sd(y)
    sd.min <- min(sd.x, sd.y)
    sd.max <- max(sd.x, sd.y)
    var.equal <- (sd.max / sd.min) < 2
  }
  
  return (t.test(x, y, paired = paired, var.equal = var.equal, ...))
}

# Example 1
snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,
            75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,77.18,
            79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,78.54,79.7,
            82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)
snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,79.21,
             81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,79.08,79.47,
             78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)

wrap.t.test(x = snacks2, y = snacks,
            alternative = "greater", conf.level = 0.9)

# Example 2
men <- c(102,87,101,96,107,101,91,85,108,67,85,82)
women <- c(73,81,111,109,143,95,92,120,93,89,119,79,90,126,62,92,77,106,105,111)
wrap.t.test(x = men, y = women,
            alternative = "two.sided", conf.level = 0.95)

# Example 3
rate.before <- c(52,66,89,87,89,72,66,65,49,62,70,52,75,63,65,61) 
rate.after <- c(51,66,71,73,70,68,60,51,40,57,65,53,64,56,60,59) 
wrap.t.test(x = rate.after, y = rate.before,
            alternative = "less", paired = TRUE, conf.level = 0.95)

# 18.3
# H0 : p = .9, HA : p < .9
.9 * 89 > 5
(1 - .9) * 89 > 5

# strong evidence to reject H0
prop.test(x = 71, n = 89, p = .9, alternative = "less",
          conf.level = .9, correct = FALSE)

n <- 89
p.hat <- 71 / n
p.hat + c(-1, 1) * qnorm(.95) * sqrt(p.hat * (1 - p.hat) / n)

x1 <- 97
n1 <- 445
p.hat1 <- x1 / n1
x2 <- 90
n2 <- 419
p.hat2 <- x2 / n2
p.star <- (x1 + x2) / (n1 + n2)
Z <- (p.hat2 - p.hat1) / sqrt(p.star * (1 - p.star) * (1/n1 + 1/n2))
Z
2 * pnorm(Z) # no evidence that proportion varies between x1 and x2

(p.hat2 - p.hat1) + c(-1, 1) * qnorm(.975) *
  sqrt(p.star * (1 - p.star) * (1/n1 + 1/n2))

# skip f - h

# 18.4
?HairEyeColor
summary(HairEyeColor)

chisq.test(x = HairEyeColor[,,1] + HairEyeColor[,,2], p = c(.01))
# strong evidence against hair color being related to eye color

library("car")
?Duncan
summary(Duncan)

chisq.test(table(Duncan$type)) # p-value = 0.015
# alpha = 0.05, weak evidence rejecting H0 thus their does not appear to be unifomly represented in the data
# alpha = 0.01, no evidence rejecting H0 thus their do appear to be unifomly represented in the data

# 18.5
typeI.mean <- function(mu0, sigma, n, alpha,
                       test = "two.sided", ITERATIONS = 10000) {
  stats <- rep(NA,ITERATIONS)
  for(i in 1:ITERATIONS){
    temporary.sample <- rnorm(n=n,mean=mu0,sd=sigma)
    temporary.mean <- mean(temporary.sample)
    temporary.sd <- sd(temporary.sample)
    stats[i] <- (temporary.mean-mu0)/(temporary.sd/sqrt(n))
  }
  pvals <- pt(stats, df = n-1)
  
  if (test == "two.sided") {
    result <- pvals
    result[stats > 0] <- 1 - pvals[stats > 0]
    return(mean(result < alpha/2))  
  } else if (test == "less") {
    return(mean(pvals < alpha))
  } else if (test == "greater") {
    return(mean((1 - pvals) < alpha))
  } else {
    stop("test argument not recognised")
  }
}

typeI.mean(mu0 = 0, sigma = 1, n = 40, alpha = 0.05, test = "less")
typeI.mean(mu0 = 0, sigma = 1, n = 40, alpha = 0.05, test = "greater")
typeI.mean(mu0 = 0, sigma = 1, n = 40, alpha = 0.05, test = "two.sided")

typeI.mean(mu0 = -4, sigma = .3, n = 60, alpha = 0.01, test = "less")
typeI.mean(mu0 = -4, sigma = .3, n = 60, alpha = 0.01, test = "greater")
typeI.mean(mu0 = -4, sigma = .3, n = 60, alpha = 0.01, test = "two.sided")


typeII.mean <- function(mu0, muA, sigma, n, alpha,
                        test = "two.sided", ITERATIONS = 10000) {
  stats <- rep(NA,ITERATIONS)
  for(i in 1:ITERATIONS){
    temporary.sample <- rnorm(n=n,mean=muA,sd=sigma)
    temporary.mean <- mean(temporary.sample)
    temporary.sd <- sd(temporary.sample)
    stats[i] <- (temporary.mean-mu0)/(temporary.sd/sqrt(n))
  }
  pvals <- pt(stats, df = n-1)
  
  if (test == "two.sided") {
    result <- pvals
    result[stats > 0] <- 1 - pvals[stats > 0]
    return(mean(result < alpha/2))  
  } else if (test == "less") {
    return(mean(pvals < alpha))
  } else if (test == "greater") {
    return(mean((1 - pvals) < alpha))
  } else {
    stop("test argument not recognised")
  }
}

typeII.mean(mu0 = -3.2, muA = -3.3, sigma = 0.1,
            n = 25, alpha = 0.05, test = "two.sided")
typeII.mean(mu0 = 8894, muA = 5600, sigma = 3888,
            n = 9, alpha = 0.05, test = "less")
typeII.mean(mu0 = .44, muA = .4, sigma = 2.4,
            n = 68, alpha = 0.05, test = "greater")

# 18.6
power.mean <- function(nvec,...){
  nlen <- length(nvec)
  result <- rep(NA,nlen)
  pbar <- txtProgressBar(min=0,max=nlen,style=3)
  for(i in 1:nlen){
    result[i] <- typeII.mean(n=nvec[i],...)
    setTxtProgressBar(pbar,i)
  }
  close(pbar)
  return(result)
}

power.mean(nvec = 50,  test="two.sided",
           mu0 = 10, muA = 10.5, sigma = .9, alpha = 0.01)

power.mean(nvec = 44,  test="less",
           mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.01)
power.mean(nvec = 44,  test="less",
           mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.05)
# 77.89 for alpha 0.01, 93.56 for alpha 0.05

sample.sizes <- 5:100

snacks <- power.mean(nvec = sample.sizes,  test="less",
                     mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.05)
minimum.n <- sample.sizes[min(which(snacks >= 0.8))]

snacks2 <- power.mean(nvec = sample.sizes,  test="less",
                      mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.01)
minimum.n2 <- sample.sizes[min(which(snacks2 >= 0.8))]

plot(sample.sizes, snacks, xlab = "sample size n", ylab = "simulated power")
points(sample.sizes, snacks2, col = "grey")
abline(h = 0.8, lty = 2)
abline(v = c(minimum.n, minimum.n2), lty = 3, col = c("black", "grey"))
legend("bottomright", legend = c("alpha=0.05","alpha=0.01"),
       col = c("black","grey"), pch = 1)