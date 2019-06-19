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

library("MASS")
?mtcars
summary(mtcars)
pairs(mtcars)

cars.null <- lm(I(1/mpg)~1, data = mtcars)
summary(cars.null)

cars.step <- step(
  cars.null,
  scope = .~.+wt*hp*factor(cyl)*disp*am+factor(gear)+drat+vs+qsec+carb
)
summary(cars.step)
# the GPM model is simpler than the MPG model

library("faraway")
?diabetes
summary(diabetes)

d <- diabetes[-which(is.na(diabetes$age) | is.na(diabetes$frame)),]

d.0 <- lm(chol~1, data = d)
d.1 <- lm(chol~age, data = d)
anova(d.0, d.1)
d.2 <- lm(chol~age+frame, data = d)
anova(d.0, d.1, d.2)
d.3 <- lm(chol~age*frame, data = d)
anova(d.0, d.1, d.2, d.3)

d.0 <- lm(chol~1, data = d)
summary(d.0)
add1(d.0, scope = .~.+stab.glu+hdl+ratio+glyhb+location+age+gender+
       height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn,
     test = "F")

d.1 <- update(d.0, formula = .~.+ratio)
summary(d.1)
add1(d.1, scope = .~.+stab.glu+hdl+ratio+glyhb+location+age+gender+
       height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn,
     test = "F")

d.2 <- update(d.1, formula = .~.+hdl)
summary(d.2)
add1(d.2, scope = .~.+stab.glu+hdl+ratio+glyhb+location+age+gender+
       height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn,
     test = "F")

d.3 <- update(d.2, formula = .~.+height)
summary(d.3)
add1(d.3, scope = .~.+stab.glu+hdl+ratio+glyhb+location+age+gender+
       height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn,
     test = "F")

d.4 <- update(d.3, formula = .~.+glyhb)
summary(d.4)
add1(d.4, scope = .~.+stab.glu+hdl+ratio+glyhb+location+age+gender+
       height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn,
     test = "F")

d.5 <- update(d.4, formula = .~.+bp.2d)
summary(d.5)
add1(d.5, scope = .~.+stab.glu+hdl+ratio+glyhb+location+age+gender+
       height+weight+frame+bp.1s+bp.1d+bp.2s+bp.2d+waist+hip+time.ppn,
     test = "F")

summary(d.5)

# 22.2
library("boot")
?nuclear
summary(nuclear)
pairs(nuclear)

model <- lm(cost~date+cap+pt+ne, data = nuclear)
summary(model)

plot(model, which = 1)
plot(model, which = 2)
# points appear randomly scattered around zero with no indication of heteroscedasticity with one extreme case labeled 19

cutoff <- 4 / nrow(nuclear)
cutoff
plot(model, which = 4)
abline(h = cutoff, lty = 2)
# observation 19 is highly influential

plot(model, which = 5, cook.levels = c(cutoff, 0.5, 1.0))
# 19 breaks the 0.125 line and 12 almost breaks it

model2 <- lm(cost~date+cap+pt+ne, data = nuclear[-19,])
summary(model2)
plot(model2, which = 1)
plot(model2, which = 2)
# points appear randomly scattered around zero with no indication of heteroscedasticity

library("faraway")
?diabetes
summary(diabetes)
pairs(diabetes)

fit <- lm(chol~age*frame+waist, data = diabetes)
summary(fit)

plot(fit, which = 1)
plot(fit, which = 2)
# randomness around zero and homoscedasticity with extreme values of 148, 295, and 63

cutoff <- 4/(nrow(diabetes) - 15)
cutoff
plot(fit, which = 5, cook.levels = c(1, 3, 5) * cutoff)