# 22.1
library("boot")
?nuclear
summary(nuclear)
pairs(nuclear)

nuclear.null <- lm(cost~1, data = nuclear)
summary(nuclear.null)
nuclear.step <- step(
  nuclear.null,
  scope = .~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,
  direction = "both")
summary(nuclear.step)
# similar to backwards

galileo <- data.frame(
  d=c(573,534,495,451,395,337,253),
  h=c(1,0.8,0.6,0.45,0.3,0.2,0.1))
summary(galileo)
galileo.0 <- lm(d~1, data = galileo)
galileo.1 <- lm(d~h, data = galileo)
galileo.2 <- lm(d~h+I(h^2), data = galileo)
galileo.3 <- lm(d~h+I(h^2)+I(h^3), data = galileo)
galileo.4 <- lm(d~h+I(h^2)+I(h^3)+I(h^4), data = galileo)
summary(galileo.0)
summary(galileo.1)
summary(galileo.2)
summary(galileo.3)
summary(galileo.4)

anova(galileo.0, galileo.1, galileo.2, galileo.3, galileo.4)
# yes the difference between order 3 and 4 is not statistically significant