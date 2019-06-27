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

library(ggplot2)
library("car")
?Salaries
summary(Salaries)
pairs(Salaries)

gg1 <- ggplot(Salaries, aes(x = yrs.service, y = salary, col = sex)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Years of Service", y = "Salary", col = "Gender")
gg1

gg2 <- ggplot(Salaries, aes(x = rank, y = salary, col = sex)) +
  geom_boxplot() +
  labs(x = "Rank", y = "Salary", col = "Gender")
gg2

gg3 <- ggplot(Salaries, aes(x = discipline, y = salary, col = sex)) +
  geom_boxplot() +
  labs(x = "Discipline", y = "Salary", col = "Gender")
gg3

gg4 <- ggplot(Salaries, aes(x = salary, fill = rank)) +
  geom_density(alpha = 0.3) +
  labs(x = "Salary", y = "Kernel Estimate", fill = "Rank")
gg4

library("gridExtra")
grid.arrange(gg1, gg2, gg3, gg4)

h1 <- ggplot(Salaries, aes(x = salary, fill = sex)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~rank) +
  labs(x = "Salary", y = "Kernel Estimate", fill = "Gender")
h1

h2 <- ggplot(Salaries, aes(x = yrs.service, y = salary, col = sex)) +
  geom_point() +
  facet_grid(discipline~rank, scales = "free_x") +
  labs(x = "Years of Service", y = "Salary", col = "Gender")
h2