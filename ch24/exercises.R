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

# 24.2
library(ggvis)

library(car)
?Salaries
summary(Salaries)
pairs(Salaries)

fill <- input_radiobuttons(
  c("Rank"="rank", "Discipline"="discipline", "Sex"="sex"),
  map = as.name, label = "Color points by...")
Salaries %>% 
  ggvis(x = ~yrs.service, y = ~salary, fill = fill) %>%
  layer_points() %>%
  add_legend("fill", title = "") %>%
  add_axis("x", title = "Years of service") %>%
  add_axis("y", title = "Salary")

Salaries %>%
  ggvis(x = ~salary, fill = ~rank) %>%
  group_by(rank) %>%
  layer_densities(adjust = input_slider(0.1, 2, label = "Smoothness")) %>% add_axis("x",title="Salary") %>% add_axis("y",title="Kernel estimate") %>%
  add_legend("fill", title = "Rank")

library("MASS")
?UScereal
summary(UScereal)
pairs(UScereal)

cereal <- UScereal
new.mfr <- as.numeric(UScereal$mfr)
new.mfr[new.mfr>2] <- 3
cereal$mfr <- factor(new.mfr,labels=c("General Mills","Kelloggs","Other"))
cereal$shelf <- factor(cereal$shelf)

filler <- input_radiobuttons(
  c("Manufacturer" = "mfr", "Shelf" = "shelf", "Vitamins" = "vitamins"),
  map = as.name, label = "Color points by...")
sizer <- input_slider(10, 300, label = "Point size:")
opacityer <- input_slider(0.1, 1, label = "Opacity:")

cereal %>%
  ggvis(x = ~protein, y = ~calories,
        fill = filler, size := sizer, opacity := opacityer) %>%
  layer_points() %>%
  add_axis("x", title = "Protein") %>%
  add_axis("y", title = "Calories") %>%
  add_legend("fill", title="")

shaper <- input_radiobuttons(
  c("Manufacturer" = "mfr", "Shelf" = "shelf", "Vitamins" = "vitamins"),
  map = as.name, label = "Shape points by...")

cereal %>%
  ggvis(x = ~protein, y = ~calories,
        fill = filler, shape = shaper, opacity := opacityer, size := sizer) %>%
  layer_points() %>%
  add_axis("x", title = "Protein") %>%
  add_axis("y", title = "Calories") %>%
  add_legend("fill", title = "") %>%
  add_legend("shape", title = "", properties = legend_props(legend = list(y = 100))) %>%
  set_options(duration = 0)