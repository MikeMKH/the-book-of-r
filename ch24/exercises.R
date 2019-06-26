# 24.1
library("MASS")
?UScereal
summary(UScereal)
pairs(UScereal)

cereal <- UScereal
mfr <- as.numeric(cereal$mfr)
mfr[mfr>2] <- 3
cereal$mfr <- factor(mfr, labels = c("General Mills", "Kelloggs", "Other"))
cereal$shelf <- factor(cereal$shelf)

library(ggplot2)
b1 <- ggplot(cereal, aes(x = protein, y = calories, col = shelf)) +
  geom_point(aes(shape = mfr)) +
  geom_smooth(method = "lm") +
  labs(x = "Protein", y = "Calories", col = "Shelf", size = "Carbs", shape = "Manufacturer")
b1

b2 <- ggplot(cereal, aes(x = calories, fill = shelf)) +
  geom_density(alpha = 0.5) +
  labs(x = "Calories", y = "Kernel estimate", fill = "Shelf")
b2

library("gridExtra")
grid.arrange(b1, b2)

d <- ggplot(cereal, aes(x = protein, y = calories)) +
  geom_point(aes(col = sugars, size = sodium, shape = shelf)) + 
  geom_smooth(method = "loess", span = 0.9) +
  facet_wrap(~mfr) +
  labs(x = "Protein", y = "Calories", col = "Sugars", shape = "Shelf", size = "Sodium")
d