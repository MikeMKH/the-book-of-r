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