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

qplot(USArrests$UrbanPop, geom = "blank",
      main = "US Urban Population", xlab = "proporation urban") +
  geom_histogram(color = "blue", fill = "white",
                 breaks = seq(0, 100, 10), closed = "right") +
  geom_vline(mapping = aes(xintercept = c(
    quantile(USArrests$UrbanPop, .25),
    mean(USArrests$UrbanPop),
    quantile(USArrests$UrbanPop, .75)),
    linetype = factor(c("1st Q", "median", "3rd Q"))),
    show.legend = TRUE) +
  scale_linetype_manual(values = 3:5) +
  labs(linetype = "")

barplot(t(as.matrix(USArrests[,-3])), names.arg = state.abb,
        horiz = TRUE, las = 1, legend.text = c("murder","rape","assault"))

urbancat <- rep(0, 50)
urbancat[USArrests$UrbanPop > median(USArrests$UrbanPop)] <- 1
urbancat <- factor(urbancat)

g <- USArrests[, -3]
g$urbancat <- urbancat

library(GGally)
ggpairs(g, aes(col = urbancat), axisLabels = "internal")

# skipped i - k