# 26.1
library("MASS")
?survey
summary(survey)
pairs(survey)

survey <- na.omit(survey[,c("Wr.Hnd", "NW.Hnd", "W.Hnd", "Sex", "Height")])
summary(survey)
pairs(survey)

library("rgl")
plot3d(z = survey$Height, x = survey$Wr.Hnd, y = survey$NW.Hnd)

plot3d(z = survey$Height[survey$W.Hnd == "Right"],
       x = survey$Wr.Hnd[survey$W.Hnd == "Right"],
       y = survey$NW.Hnd[survey$W.Hnd == "Right"],
       col = c("black", "red")[as.numeric(survey$Sex[survey$W.Hnd == "Right"])],
       size = 4, xlab = "Writing hand", ylab = "Non-writing hand", zlab = "Height")
plot3d(z = survey$Height[survey$W.Hnd == "Left"],
       x = survey$Wr.Hnd[survey$W.Hnd == "Left"],
       y = survey$NW.Hnd[survey$W.Hnd == "Left"],
       col = c("black", "red")[as.numeric(survey$Sex[survey$W.Hnd == "Left"])],
       size = 10, xlab = "Writing hand", ylab = "Non-writing hand", zlab = "Height")
legend3d("topleft", pch = 19, pt.cex = c(0.8, 0.8, 1.5, 1.5),
         col = c("black", "red", "black", "red"),
         legend = c("Male LH", "Female RH", "Male LH", "Female LH"))

# skip c

# 26.2
?airquality
aq <- na.omit(airquality[,c("Temp", "Wind", "Ozone", "Month")])
summary(aq)
pairs(aq)

fit <- lm(Temp~Wind*Ozone, data = aq)
summary(fit)

n <- 50
windseq <- seq(min(aq$Wind), max(aq$Wind), length = n)
ozoneseq <- seq(min(aq$Ozone), max(aq$Ozone), length = n)
grid <- expand.grid(Wind = windseq, Ozone = ozoneseq)
predict <- predict(fit, newdata = grid,
                   interval = "confidence", level = 0.95)
m <- matrix(predict[,1], n, n)

library(rgl)
persp3d(x = windseq, y = ozoneseq, z = m, color = "yellow")

# skip b - e

# 26.3
library(mvtnorm)
library(rgl)
library(misc3d)
library(ks)
covmat <- matrix(c(1,0.8,0.4,0.8,1,0.6,0.4,0.6,1),3,3)
a <- rmvnorm(1000, mean = c(0, 0, 0), sigma = covmat)

plot3d(a, xlab = "x", ylab = "y", zlab = "z")

s <- seq(-3, 3, length = 50)
grid <- expand.grid(s, s, s)
f <- array(
  dmvnorm(grid, mean = c(0, 0, 0), sigma = covmat),
  c(50, 50, 50)
)
b <- dmvnorm(c(0, 0, 0), mean = c(0, 0, 0), sigma = covmat)
contour3d(x = s, y = s, z = s, f = f, level = c(0.1, 0.5, 0.9) * norm,
          color = c("yellow", "seagreen4", "navyblue"),
          alpha = c(0.2, 0.4, 0.6), add = TRUE)

c <- kde(a, compute.cont = TRUE)
plot3d(a, xlab = "x", ylab = "y", zlab = "z")

contour3d(x = s, y = s, z = s, f = f, level = 0.5 * b,
          color = "seagreen4", alpha = 0.4, add = TRUE)
contour3d(x = c$eval.points[[1]],
          y = c$eval.points[[2]],
          z = c$eval.points[[3]],
          f = c$estimate,
          level = c$cont[50],
          color = "red", alpha = 0.2, add = TRUE)

# skip d -f
res <- 200
theta <- seq(0, 2 * pi, length = res)
alpha <- 1
beta <- 2

xm <- outer(theta, theta,
            function(t1, t2) (beta + alpha * cos(t2)) * cos(t1))
ym <- outer(theta, theta,
            function(t1, t2) (beta + alpha * cos(t2))* sin(t1))
zm <- outer(theta, theta,
            function(t1, t2) alpha * sin(t2))
doughnut <- rep("tan", res^2)

plot3d(x = xm, y = ym, z = zm)
persp3d(x = xm, y = ym, z = zm,
        col = "seagreen4", axes = FALSE, xlab = "", ylab = "", zlab = "")

doughnut[as.vector(zm) > 0] <- "pink"
sample(1:10, 4)

sprinkles <- c("blue", "green", "red", "violet", "yellow")
doughnut[sample(which(as.vector(zm) > 0), 300)] <- sprinkles

persp3d(xm, ym, zm, col = doughnut, aspect = c(1, 1, 0.4),
        axes = FALSE, xlab = "", ylab = "", zlab = "")