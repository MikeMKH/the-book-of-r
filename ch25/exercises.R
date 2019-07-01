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