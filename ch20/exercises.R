# 20.1
library("MASS")
?survey
summary(survey)

survfit <- lm(Height~Wr.Hnd, data = survey)
summary(survfit)

xvals <- data.frame(Wr.Hnd = c(12, 15.2, 17, 19))
predict(survfit, newdata = xvals, interval = "confidence", level = 0.99)

# skip b

survfit <- lm(Height~Pulse, data = survey)
plot(survey$Height~survey$Pulse,
     xlab = "Pulse (bpm)", ylab = "Height (cm)")
abline(survfit, lwd = 2)

summary(survfit) # no evidence to reject H0
confint(survfit, level = 0.90)

xseq <- data.frame(Pulse = seq(30, 110, length = 100))
survfit.ci <- predict(survfit, newdata = xseq,
                      interval = "confidence", level = 0.90)
survfit.pi <- predict(survfit, newdata = xseq,
                      interval = "prediction", level = 0.90)
lines(xseq[,1], survfit.ci[,2], lty = 2) 
lines(xseq[,1], survfit.ci[,3], lty = 2)
lines(xseq[,1], survfit.pi[,2], lty = 2, col = "grey")
lines(xseq[,1], survfit.pi[,3], lty = 2, col = "grey")
legend("topleft", legend = c("90% CI", "90% PI"),
       lty = 2, col = c("black", "grey"))

incomplete.obs <- which(is.na(survey$Height) | is.na(survey$Pulse))
abline(h = mean(survey$Height[-incomplete.obs]), col = 2, lty = 3, lwd = 3)

data("mtcars")
?mtcars
summary(mtcars)

plot(mtcars$mpg~mtcars$wt, ylab = "mpg", xlab = "weight (1000 lbs)")
carfit <- lm(mpg~wt, data = mtcars)
abline(carfit, lwd = 2)

summary(carfit)
# mpg = 37.2851 + (-5.3445 * wt)

predict(carfit, newdata = data.frame(wt = 6),
        interval = "confidence", level = 0.95)
predict(carfit, newdata = data.frame(wt = 6),
        interval = "prediction", level = 0.95)
# 6,000 is outside of the existing data

# 20.2
library("MASS")
?survey
summary(survey)

boxplot(survey$Height~survey$Exer)

survfit <- lm(Height~Exer, data = survey)
summary(survfit)
# it seems that those who exercise less are shorter

predict(survfit, interval = "prediction", level = 0.95,
        newdata = data.frame(Exer = factor(c("Freq", "Some", "None"))))

summary(aov(Height~Exer, data = survey))
# same as lm

none.first <- relevel(survey$Exer, ref = "None")
levels(none.first)
summary(aov(Height~none.first, data = survey)) # no difference

?mtcars
summary(mtcars)

carfit <- lm(qsec~gear, data = mtcars)
summary(carfit) # no evidence

carfit2 <- lm(qsec~factor(gear), data = mtcars)
summary(carfit2) # evidence, 4 gears seems to be the best

plot(mtcars$qsec~mtcars$gear, xlab = "# gears", ylab = "1/4 mile time (sec)")
abline(carfit, lwd = 2) # model is not explained by a straight line

?chickwts
summary(chickwts)
plot(chickwts$weight~chickwts$feed, xlab = "feed", ylab = "weight")

chickfit <- lm(weight~feed, data = chickwts)
summary(chickfit)

fit <- function(level) {
  return (
    predict(chickfit,  level = level, interval = "prediction",
        newdata = data.frame(feed = factor(c("casein", "soybean", "sunflower"))))
  )
}
fit(0.01)
fit(0.50)
fit(0.90)
fit(0.95)
fit(0.99)
fit(0.99999)

confint(chickfit)