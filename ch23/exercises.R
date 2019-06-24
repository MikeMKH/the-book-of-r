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