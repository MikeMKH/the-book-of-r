# 18.1

a <- (3.97 - 3.5) / (2.21 / sqrt(73))
pt(-a, df = 72) + (1-pt(a, df = 72)) > .05 # TRUE = insufficient evidence

t.test(quakes$mag, alternative = "greater",
       mu = 4.3, conf.level = 1 - .01)

mean(quakes$mag) + c(-1, 1) *
  qt(0.995, df = 999) * sd(quakes$mag) / sqrt(1000)