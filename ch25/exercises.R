# 25.1
library(car)
?Salaries
summary(Salaries)
pairs(Salaries)

col1 <- colorRampPalette(c("black","red","yellow2"))
col2 <- colorRamp(c("black","red","yellow2"))

rank.pch <- c(19, 17, 15)[as.numeric(Salaries$rank)]
sex.cex <- c(1, 1.5)[as.numeric(Salaries$sex)]

normalize <- function(datavec) {
  lo <- min(datavec, na.rm=TRUE)
  up <- max(datavec, na.rm=TRUE)
  datanorm <- (datavec - lo) / (up - lo)
  return(datanorm)
}

phd.norm <- normalize(Salaries$yrs.since.phd)
phd.col <- rgb(col2(phd.norm), maxColorValue = 255)

phd.col[Salaries$sex == "Female"] <-
  adjustcolor(phd.col[Salaries$sex == "Female"], alpha = 0.9)
phd.col[Salaries$sex == "Male"] <-
  adjustcolor(phd.col[Salaries$sex == "Male"], alpha = 0.3)

par(mar=c(5, 4, 4, 6))
plot(Salaries$salary~Salaries$yrs.service,
     col = phd.col, pch = rank.pch, cex = sex.cex,
     xlab = "Years of service", ylab = "Salary")

legend(-5, 265000, legend = levels(Salaries$rank), title = "Rank",
       pch = c(19, 17, 15), xpd = TRUE, horiz = TRUE)
legend(40, 265000, legend = levels(Salaries$sex), title = "Sex",
       pch = c(19, 19), pt.cex = c(1, 1.5),
       col = c(adjustcolor("red", 0.9), adjustcolor("red", 0.3)),
       xpd = TRUE, horiz = T)

library("shape")
colorlegend(col = col1(50), zlim = range(Salaries$yrs.since.phd),
            zval = seq(10, 50, 10), main = "Years since\nPhD")

# skip h - l

# 25.2
library(scatterplot3d)

library(faraway)
?diabetes
summary(diabetes)
pairs(diabetes)

scatterplot3d(x = diabetes$weight, y = diabetes$hip, z = diabetes$waist,
              highlight.3d = TRUE, pch = c(3, 4)[as.numeric(diabetes$gender)],
              xlab = "Weight", ylab = "Hip", zlab = "Waist")
legend("topleft", legend = levels(diabetes$gender), pch=3:4)

?airquality
summary(airquality)
pairs(airquality)

b <- na.omit(airquality)
?b
summary(b)
pairs(b)

ozcol <- topo.colors(50)
colindex <- cut(b$Ozone, include.lowest = TRUE,
                breaks = seq(min(b$Ozone), max(b$Ozone), length = 51))
scatterplot3d(x = b$Wind, y = b$Solar.R, z = b$Temp,
              type = "h", pch = b$Month - 4, color = ozcol[colindex],
              main = "NY Air Quality",
              xlab = "Wind (mph)", ylab = "Solar Radiation (lang)", zlab = "Temperature (F)")
legend("bottomright", legend = c("May", "June", "July", "August", "September"),
       pch = 1:5, title = "Month", xpd = TRUE)
library("shape")
colorlegend(ozcol, zlim = range(b$Ozone), main = "Ozone (ppb)",
            zval = seq(0, 160, 40), posx = c(0.1, 0.13), posy = c(0.7, 0.9))

# 25.3
library(boot)
?nuclear
summary(nuclear)
pairs(nuclear)

a1 <- lm(cost~date+cap, data = nuclear)
summary(a1)
a2 <- lm(cost~date*cap, data = nuclear)
summary(a2)

capseq <- seq(min(nuclear$cap), max(nuclear$cap), length = 50)
datseq <- seq(min(nuclear$date), max(nuclear$date), length = 50)
b <- expand.grid(cap = capseq, date = datseq)
p1 <- matrix(predict(a1, newdata = b), 50, 50)
p2 <- matrix(predict(a2, newdata = b), 50, 50)

par(mfrow = c(1, 2))
contour(x = capseq, y = datseq, z = p1)
contour(x = capseq, y = datseq, z = p2)
# similar with a slight curvature appearing in the surface associated with the interactive model

filled.contour(x = capseq, y = datseq, z = p1,
               xlab = "Capacity", ylab = "Permit date",
               color.palette = topo.colors,
               plot.axes = {axis(1);axis(2);
                 contour(capseq, datseq, p2,add = TRUE, lwd = 2, lty = 2)})

?faithful
summary(faithful)
plot(x = faithful$waiting, y = faithful$eruptions)

library("MASS")
dens <- kde2d(x = faithful$waiting, y = faithful$eruptions, n = 100)
contour(dens$x, dens$y, dens$z)

cols <- colorRampPalette(c("darkblue", "hotpink"))
filled.contour(dens$x, dens$y, dens$z, color.palette = cols,
               xlab = "Waiting time", ylab = "Eruption duration",
               plot.axes={axis(1);axis(2);points(faithful$waiting, faithful$eruption, cex = 0.5, col = "gray")})
# skip h

# 25.4
?airquality
summary(airquality)
pairs(airquality)

air <- na.omit(airquality[,c(4, 3, 1)])
air.fit <- lm(Temp~Wind*Ozone, data = air)
summary(air.fit)

n <- 50
windseq <- seq(min(air$Wind), max(air$Wind), length = n)
ozoneseq <- seq(min(air$Ozone), max(air$Ozone), length = n)
grid <- expand.grid(Wind = windseq, Ozone = ozoneseq)
air.pred <- matrix(predict(air.fit, newdata = grid), n, n)

normalize <- function(datavec){
  lo <- min(datavec, na.rm = TRUE)
  up <- max(datavec, na.rm = TRUE)
  datanorm <- (datavec - lo) / (up - lo)
  return(datanorm)
}

library("shape")
par(mar = c(5, 4, 4, 6))
image(x = windseq, y = ozoneseq, z = air.pred,
      col = topo.colors(20), xlab = "Wind (mph)", ylab = "Ozone (ppb)")
points(air$Wind, air$Ozone, col = gray(normalize(air$Temp)), pch = 19)
colorlegend(col = topo.colors(20),
            zlim = range(air.pred), zval=seq(60, 140, 10),
            posx = c(0.88, 0.91), main = "Pred. temp")
colorlegend(col = gray.colors(10, start = 0, end = 1),
            zlim = range(air$Temp), zval = seq(60, 95, 5),
            posx = c(0.67, 0.7), posy = c(0.4, 0.8), main = "Obs. temp")

# skip d - f

# 25.5
library(boot)
?nuclear
summary(nuclear)
pairs(nuclear)

fit1 <- lm(cost~date+cap, data = nuclear)
fit2 <- lm(cost~date*cap, data = nuclear)
summary(fit1)
summary(fit2)

dateseq <- seq(min(nuclear$date), max(nuclear$date), length = 50)
capseq <- seq(min(nuclear$cap), max(nuclear$cap), length = 50)
grid <- expand.grid(date = dateseq, cap = capseq)

pred1 <- matrix(predict(fit1, newdata = grid), 50, 50)
pred2 <- matrix(predict(fit2, newdata = grid), 50, 50)

par(mfrow = c(2, 1), mar = rep(1, 4))
persp(x = capseq, y = dateseq, z = pred1,
      theta = 25, zlim = range(c(pred1, pred2)),
      ticktype = "detailed",
      xlab = "Capacity (MWe)",
      ylab = "Permit issue date",
      zlab = "Predicted cost")
persp(x = capseq, y = dateseq, z = pred2,
      theta = 25, zlim = range(c(pred1, pred2)),
      ticktype = "detailed",
      xlab = "Capacity (MWe)",
      ylab = "Permit issue date",
      zlab = "Predicted cost")

par(mfrow=c(1,1),mar=rep(1,4))
persp(x = capseq, y = dateseq, z = pred2 - pred1,
      theta = 75, ticktype = "detailed")
# something happen around the time of 1970 and 1971

x <- nrow(volcano)
y <- ncol(volcano)
persp(x = 1:x, y = 1:y, z = volcano)

par(mar = c(1, 1, 1, 5))
m <- (volcano[-1, -1] + volcano[-1, -y] + volcano[-x, -1] + volcano[-x, -y]) / 4
cols <- terrain.colors(50)
col <- cols[cut(m, breaks = seq(min(volcano), max(volcano), length=51), include.lowest = TRUE)]
persp(1:x, 1:y, volcano, col=col, border = NA, theta = -30, phi = 15,
      scale = FALSE, expand = 0.1, axes = FALSE)
library("shape")
colorlegend(cols, zlim = range(volcano), zval = seq(100, 180, 20),
            posx = c(0.83, 0.86), posy = c(0.4, 0.7),
            main = "Elevation (m)")

# skip e - f