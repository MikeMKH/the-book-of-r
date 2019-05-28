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