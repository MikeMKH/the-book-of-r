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

library("faraway")
?diabetes
summary(diabetes)

diabetes <- na.omit(diabetes[, c("chol", "age", "gender", "height", "weight",
                                 "frame", "waist", "hip", "location")])
summary(diabetes)
pairs(diabetes)

diabetes.null <- lm(chol~1, data = diabetes)
summary(diabetes.null)

diabetes.full <- lm(chol~age*gender*height*weight*frame*waist*hip+location,
                    data = diabetes)
summary(diabetes.full)

diabetes.step <- step(
  diabetes.null,
  scope = .~.+age*gender*height*weight*frame*waist*hip+location
)
summary(diabetes.step)

add1(
  diabetes.null,
  scope = .~.+age*gender*height*weight*frame*waist*hip+location,
  test = "F"
)
diabetes.1 <- update(diabetes.null, .~.+age)
add1(
  diabetes.1,
  scope = .~.+age*gender*height*weight*frame*waist*hip+location,
  test = "F"
)
diabetes.2 <- update(diabetes.1, .~.+frame)
add1(
  diabetes.2,
  scope = .~.+age*gender*height*weight*frame*waist*hip+location,
  test = "F"
)
summary(diabetes.1)
summary(diabetes.2)
# forward selection stops after age and frame

diabetes.full.step <- step(diabetes.full)
summary(diabetes.full.step)
# way more complex, maybe it is better