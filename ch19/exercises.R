# 19.1
depth <- c(93,120,65,105,115,82,99,87,100,90,78,95,93,88,110,85,45,80,28,75,70,65,55,50,40,100,75,65,40,73,65,50,30,45,50,45,55,96,58,95,90,65,80,85,95,82)
site <- c(rep("I",15),rep("II",10),rep("III",12),rep("IV",9))

boxplot(depth~site)
depth.means <- tapply(depth, INDEX = site, FUN = mean)
points(1:4, depth.means, pch = 4)

depth.meancen <- depth - rep(depth.means, table(site))
qqnorm(depth.meancen)
qqline(depth.meancen)

depth.sds <- tapply(depth, INDEX = site, FUN = sd)
max(depth.sds) / min(depth.sds) < 2

summary(aov(depth~site)) # very strong evidence against H0
# there is evidence to conclude there is a difference in the mean depths of the finds across the sites

?iris
summary(iris)

iris.check <- function(measurement) {
  m <- tapply(measurement, iris$Species, mean)
  mc <- measurement - m[as.numeric(iris$Species)]
  qqnorm(mc)
  qqline(mc)
  
  s <- tapply(measurement, iris$Species, sd)
  return(max(s)/min(s) < 2)
}

iris.check(iris$Sepal.Length)  # normal, true
iris.check(iris$Sepal.Width)   # normal, true
iris.check(iris$Petal.Length)  # not normal, false
iris.check(iris$Petal.Width)   # normal, false

summary(aov(Sepal.Length~Species, data = iris))
summary(aov(Sepal.Width~Species, data = iris))
# strong evidence to reject H0 and conclude that sepal lengths and widths do vary according to species