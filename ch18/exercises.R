# 18.1
a <- (3.97 - 3.5) / (2.21 / sqrt(73))
pt(-a, df = 72) + (1-pt(a, df = 72)) > .05 # TRUE = insufficient evidence

t.test(quakes$mag, alternative = "greater",
       mu = 4.3, conf.level = 1 - .01)

mean(quakes$mag) + c(-1, 1) *
  qt(0.995, df = 999) * sd(quakes$mag) / sqrt(1000)

# 18.2
library("MASS")
?anorexia
summary(anorexia)

t.test(anorexia$Postwt, anorexia$Prewt,
       conf.level = .95, alternative = "greater", paired = TRUE) # strong evidence

t.test(anorexia$Postwt[anorexia$Treat == "Cont"], anorexia$Prewt[anorexia$Treat == "Cont"],
       conf.level = .95, alternative = "greater", paired = TRUE) # no evidence
t.test(anorexia$Postwt[anorexia$Treat == "CBT"], anorexia$Prewt[anorexia$Treat == "CBT"],
       conf.level = .95, alternative = "greater", paired = TRUE) # some evidence
t.test(anorexia$Postwt[anorexia$Treat == "FT"], anorexia$Prewt[anorexia$Treat == "FT"],
       conf.level = .95, alternative = "greater", paired = TRUE) # strong evidence

?PlantGrowth
summary(PlantGrowth)

growth.control <- PlantGrowth$weight[PlantGrowth$group == "ctrl"]
growth.treated <- PlantGrowth$weight[PlantGrowth$group != "ctrl"]

max(c(sd(growth.control), sd(growth.treated))) /
  min(c(sd(growth.control), sd(growth.treated))) < 2

t.test(x = growth.control, y = growth.treated,
       alternative = "less", var.equal = TRUE) # no evidence

wrap.t.test <- function(x, y, paired = FALSE, var.equal = FALSE, ...) {
  if (!paired) {
    sd.x <- sd(x)
    sd.y <- sd(y)
    sd.min <- min(sd.x, sd.y)
    sd.max <- max(sd.x, sd.y)
    var.equal <- (sd.max / sd.min) < 2
  }
  
  return (t.test(x, y, paired = paired, var.equal = var.equal, ...))
}

# Example 1
snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,
            75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,77.18,
            79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,78.54,79.7,
            82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)
snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,79.21,
             81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,79.08,79.47,
             78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)

wrap.t.test(x = snacks2, y = snacks,
            alternative = "greater", conf.level = 0.9)

# Example 2
men <- c(102,87,101,96,107,101,91,85,108,67,85,82)
women <- c(73,81,111,109,143,95,92,120,93,89,119,79,90,126,62,92,77,106,105,111)
wrap.t.test(x = men, y = women,
            alternative = "two.sided", conf.level = 0.95)

# Example 3
rate.before <- c(52,66,89,87,89,72,66,65,49,62,70,52,75,63,65,61) 
rate.after <- c(51,66,71,73,70,68,60,51,40,57,65,53,64,56,60,59) 
wrap.t.test(x = rate.after, y = rate.before,
            alternative = "less", paired = TRUE, conf.level = 0.95)