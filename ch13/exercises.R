# 13.1
  # a.i numeric discrete
  # a.ii categorical ordinal
  # a.iii numeric continuous
  # a.iv categorical ordinal
  # a.v categorical nominal
  # a.vi numeric continuous

# skipped b

# 13.2
round(mean(quakes$depth >= 300), 2)

mean(quakes$mag[quakes$depth >= 300])
median(quakes$mag[quakes$depth >= 300])

# c is dumb using tapply
tapply(chickwts$weight, chickwts$feed, mean)

# numeric discrete and categorical nominal
ctab <- table(InsectSprays$count)
ctab[ctab == max(ctab)]

tapply(InsectSprays$count, InsectSprays$spray, sum)

# skipped g
tapply(InsectSprays$count, InsectSprays$spray,
       function(x) round(mean(x >= 5) * 100.0, 2))

# 13.3
quantile(chickwts$weight, c(.10, .30, .90))
a <- tapply(chickwts$weight, chickwts$feed, var)
a[a == max(a)]

IQR(quakes$depth)
summary(quakes$mag[quakes$depth >= 400])

depthcat <- cut(quakes$depth,
                seq(min(quakes$depth), max(quakes$depth), length = 5),
                include.lowest = TRUE,
                right = FALSE)
levels(depthcat)

tapply(quakes$mag, depthcat, mean)
tapply(quakes$mag, depthcat, sd)

tapply(quakes$mag, depthcat, quantile, prob = .80)

# 13.4
w <- c(55,85,75,42,93,63,58,75,89,67)
h <- c(161,185,174,154,188,178,170,167,181,178)
cor(w, h)

mtcars
plot(mtcars$hp, mtcars$qsec,
     xlab = "horsepower", ylab = "1/4 mile time")
cor(mtcars$hp, mtcars$qsec)

tranfac <- factor(mtcars$am, labels = c("auto","manual"))

library(ggplot2)
qplot(x = mtcars$hp, y = mtcars$qsec,
      xlab = "horsepower", ylab = "1/4 mile time",
      color = tranfac)

cor(mtcars$hp[tranfac == "auto"], mtcars$qsec[tranfac == "auto"])
cor(mtcars$hp[tranfac == "manual"], mtcars$qsec[tranfac == "manual"])

sunflower <- chickwts$weight[chickwts$feed=="sunflower"]
plot(sunflower, rep(0, length(sunflower)),
     yaxt = "n", bty = "n",
     xlab = "sunflower feed chick weights", ylab = "")
abline(h = 0, col = "gray", lty = 2)

sd(sunflower)
IQR(sunflower)

sd(sunflower[-which(sunflower == min(sunflower))])
IQR(sunflower[-which(sunflower == min(sunflower))])