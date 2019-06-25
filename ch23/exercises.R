# 23.1

#skip 1-2

dev.new(width = 9, height = 4.5)
grid <- rbind(c(1,1,2,4), c(1,1,3,4))
layout(grid)
layout.show(4)
par(mar = c(4,4,2,1))

plot(quakes$long, quakes$lat, cex = 0.02 * quakes$stations,
     xlab = "Longitude", ylab = "Latitude")
box(which = "figure", col = "gray")

plot(quakes$mag, quakes$stations,
     xlab = "Magnitude", ylab = "Stations")
box(which = "figure", col = "gray")

plot(quakes$depth, quakes$stations,
     xlab = "Depth", ylab = "Stations")
box(which = "figure", col = "gray")

hist(quakes$stations, main = "", xlab = "Stations")
abline(v = mean(quakes$stations), lty = 2)
box(which = "figure", col = "gray")

dev.off()

interactive.arrow <- function(..., label = NA) {
  points <- locator(2)
  arrows(x0 = points$x[1], y0 = points$y[1],
         x1 = points$x[2], y1 = points$y[2], ...)
  
  if(!is.na(label)){
    points <- text(locator(1), label = label, xpd = NA)
  }
}

dev.new(width=9,height=4.5)
boxplot(quakes$mag)
interactive.arrow(xpd = TRUE, label = "minumum")
interactive.arrow(xpd = TRUE, label = "1st quartile")
interactive.arrow(xpd = TRUE, label = "median")
interactive.arrow(xpd = TRUE, label = "3rd quartile")
interactive.arrow(xpd = TRUE, label = "maximum")
interactive.arrow(xpd = TRUE, label = "outliers")

dev.off()

# 23.2
library(ggplot2)
?diamonds
summary(diamonds)

dev.new(width = 6, height = 6)
par(mar = c(0, 4, 2, 0))

boxplot(diamonds$price~diamonds$clarity, axes=FALSE, frame=FALSE,
        main = "Diamond Prices by Clarity")
axis(2, at = seq(0, 18000, 2000), las = 1, tcl = 1, mgp = c(3, 0.5, 0))

text(locator(1), "I1",   xpd = TRUE, cex = 1.5)
text(locator(1), "SI2",  cex = 1.5)
text(locator(1), "SI1",  cex = 1.5)
text(locator(1), "VS2",  cex = 1.5)
text(locator(1), "VS1",  cex = 1.5)
text(locator(1), "VVS2", cex = 1.5)
text(locator(1), "VVS1", cex = 1.5)
text(locator(1), "IF",   cex = 1.5)

dev.off()

dev.new(width = 8, height = 7)
par(mar = c(2, 5, 3, 5), oma = c(2, rep(1, 3)))

plot(diamonds$price~diamonds$carat,
     col = c("red", "green", "blue", "purple", "black")[as.numeric(diamonds$cut)],
     axes = FALSE, ann = FALSE)
box(bty = "u")

axis(1, at = seq(0.0, 6.0, 0.25), font = 4, mgp = c(3, 0.5, 0))
axis(1, at = seq(0.0, 6.0, 0.5), tcl = -0.25, labels = FALSE)

axis(2, at = seq(1000, 19000, 2000), las = 1, font = 4)
axis(4, at = seq(1000, 19000, 1000) * 1.37,
     labels = seq(1000, 19000, 1000), las = 1, font = 4)

fit <- lm(price~carat + I(carat^2), data=diamonds)
s <- seq(min(diamonds$carat), max(diamonds$carat), length = 100)
p <- predict(fit, newdata = data.frame(carat = s), interval = "prediction")
lines(s, p[,1], col = "gray", lwd = 2)
lines(s, p[,2], col = "gray", lty = 2)
lines(s, p[,3], col = "gray", lty = 2)

expr1 <- expression("USD$"%~~%1.37%*%"SGD$")
expr2 <- expression(paste("Price"==beta[0]+beta[1],"Carat",+beta[2],"Carat"^2))

mtext("CARAT",side=1,line=0,outer=TRUE)
mtext("SGD$",side=2,line=4)
mtext("Scatterplot of Diamond Price by Carat and Cut",side=3,line=2,cex=1.5)
mtext(expr1,side=4,line=4)

legend(locator(1), legend = levels(diamonds$cut),
       col = c("red", "green", "blue", "purple", "black"), pch=1)

dev.off()