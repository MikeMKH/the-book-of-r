# 7.1
plot(x = -3:3, y = 7:13, type = "n", main = "", xlab = "", ylab = "")

abline(h = c(7, 13), col = "red", lty = 2, lwd = 2)
abline(v = c(-3, 3), col = "red", lty = 2, lwd = 2)

text(x = 0, y = 10, labels = "somthing\nprofound")

arrows(x0 = -2.6, y0 = 12.5, x1 = -1, y1 = 10.5)
arrows(x0 = -2.6, y0 = 10,   x1 = -1, y1 = 10)
arrows(x0 = -2.6, y0 = 7.5,  x1 = -1, y1 = 9.5)

arrows(x0 = 2.6, y0 = 12.5, x1 = 1, y1 = 10.5)
arrows(x0 = 2.6, y0 = 10,   x1 = 1, y1 = 10)
arrows(x0 = 2.6, y0 = 7.5,  x1 = 1, y1 = 9.5)

b <- data.frame(
  weight = c(55,85,75,42,93,63,58,75,89,67),
  height = c(161,185,174,154,188,178,170,167,181,178),
  sex = factor(c("female","male","male","female","male","male","female","male","male","female"))
)
plot(b$weight, b$height, type="n",
     xlab="Weight (kg)", ylab="Height (cm)",
     main="Height against weight for 10 people"
)
points(b$weight[b$sex == "male"], b$height[b$sex == "male"],
       pch = 4, col = "blue"
)
points(b$weight[b$sex == "female"], b$height[b$sex == "female"],
       pch = 2, col = "pink"
)
legend("topleft", legend=c("male","female"),
       pch=c(4, 2), col=c("blue", "pink")
)