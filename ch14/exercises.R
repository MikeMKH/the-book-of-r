# 14.1
hist(InsectSprays$count)

insect.spray <- levels(InsectSprays$spray)
insect.count <- tapply(InsectSprays$count, InsectSprays$spray, sum)
barplot(insect.count, names.arg = insect.spray,
        xlab = "Type", ylab = "Count", main = "Insects by Spray Type")
pie(insect.count, labels = insect.spray, main = "Inspects by Spray Type")

library(ggplot2)
qplot(InsectSprays$spray, InsectSprays$count, geom = "boxplot",
      xlab = "Type", ylab = "Count", main = "Inspects by Spray Type")