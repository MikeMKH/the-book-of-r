# 21.1
library("MASS")
?cats
summary(cats)

plot(cats$Bwt, cats$Hwt, col = cats$Sex,
     xlab = "body weight (kg)", ylab = "heart weight (kg)")
legend("topleft", legend = c("female", "male"), col = c(1, 2), pch = c(1, 1))

catsfit <- lm(Hwt~Bwt+Sex, data = cats)
summary(catsfit)
# Hwt = 4.0758 * Bwt + -0.0821 * isMale + -0.4149
# sex does not seem to really matter much

names(summary(catsfit))
summary(catsfit)$r.squared
summary(catsfit)$fstatistic

predict(catsfit, newdata = data.frame(Bwt = 3.4, Sex = "F"),
        level = .95, interval = "prediction")

plot(cats$Bwt, cats$Hwt, col = cats$Sex,
     xlab = "body weight (kg)", ylab = "heart weight (kg)")
legend("topleft", legend = c("female", "male"), col = c(1, 2), pch = c(1, 1))

seq <- seq(min(cats$Bwt) - 2, max(cats$Bwt) + 2, length = 100)
n <- length(seq)
predict <- predict(catsfit, newdata = data.frame(
  Bwt = rep(seq, 2), Sex = rep(c("M", "F"), each = n)))
lines(seq, predict[1:n], col = 2)
lines(seq, predict[(n+1):(2*n)], col = 1)
# shows that sex does not matter much

library("boot")
?nuclear
summary(nuclear)
pairs(nuclear)

nufit1 <- lm(cost~t1+t2, data = nuclear)
summary(nufit1)

nufit2 <- lm(cost~t1+t2+date, data = nuclear)
summary(nufit2)
# date should be used instead of t1

nufit <- lm(cost~date+cap+ne, data = nuclear)
summary(nufit)
confint(nufit)
# -6458 + 95.44 * date + 0.4157 * cap + 126.1 * ne

detroit <- data.frame(Murder = c(8.6,8.9,8.52,8.89,13.07,14.57,21.36,28.03,31.49,37.39,46.26,47.24,52.33),
                      Police = c(260.35,269.8,272.04,272.96,272.51,261.34,268.89,295.99,319.87,341.43,356.59,376.69,390.19),
                      Unemploy = c(11,7,5.2,4.3,3.5,3.2,4.1,3.9,3.6,7.1,8.4,7.7,6.3),
                      Guns = c(178.15,156.41,198.02,222.1,301.92,391.22,665.56,1131.21,837.6,794.9,817.74,583.17,709.59))
summary(detroit)
pairs(detroit)

detfit <- lm(Murder~Police+Unemploy+Guns, data = detroit)
confint(detfit)
summary(detfit)
# -68.852509 + Police * 0.280799 + Unemploy * 0.147248 + Guns * 0.014177
summary(detfit)$r.squared
# no causal relationship can be shown from the limited data

detfit2 <- lm(Murder~Police+Guns, data = detroit)
summary(detfit2)
summary(detfit2)$r.squared

predict(detfit2, interval="confidence", level = 0.99,
        newdata = data.frame(Police = c(300, 300), Guns = c(500, 0)))

# 21.2
roll <- data.frame(distance = c(573,534,495,451,395,337,253),
                   height = c(1,0.8,0.6,0.45,0.3,0.2,0.1))
plot(roll$distance~roll$height, xlab = "height", ylab = "distance")

rollfit.order2 <- lm(distance~height+I(height^2), data = roll)
summary(rollfit.order2)

rollfit.order3 <- lm(distance~height+I(height^2)+I(height^3), data = roll)
summary(rollfit.order3)

rollfit.order4 <- lm(distance~height+I(height^2)+I(height^3)+I(height^4), data = roll)
summary(rollfit.order4)
# order 3 is significant in it's highest term, order 4 is not, thus order 3 is a better model

seq <- seq(0.05, 1.05, length = 100)
predict <- predict(rollfit.order3, newdata = data.frame(height = seq),
                   interval = "confidence", level = 0.9)
plot(roll$distance~roll$height, xlab = "height", ylab = "distance")
lines(seq, predict[,1])
lines(seq, predict[,2], lty = 2)
lines(seq, predict[,3], lty = 2)

library("faraway")
?trees
summary(trees)
pairs(trees)

treesfit.order2 <- lm(Volume~Girth+I(Girth^2), data = trees)
summary(treesfit.order2)
# Volume = -2.09214 * Girth + 0.25454 * Girth^2 + 10.78627
# F 350.5

treesfit.orderlg <- lm(log(Volume)~log(Girth), data = trees)
summary(treesfit.orderlg)
# log(Volume) = 2.19997 * log(Girth) + -2.35332
# F 599.7

tree.predict <- function(fit, type = 2) {
  seq <- seq(7, 21, length = 100)
  p <- predict(fit, newdata = data.frame(Girth = seq),
               interval = "confidence", level = 0.95)
  lines(seq, p[,1], lty = (type - 1))
  lines(seq, p[,2], lty = type)
  lines(seq, p[,3], lty = type)
  return(p)
}

plot(trees$Volume~trees$Girth, xlab = "Girth (in)", ylab = "Volume (ft^3)")
tree.predict(treesfit.order2, type = 2)
tree.predict(treesfit.orderlg, type = 4)