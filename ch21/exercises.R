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