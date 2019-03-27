data("quakes")
write.table(x = quakes[quakes$mag >= 5,],
            file = "./q5.txt", sep = "!", row.names = FALSE
)
q5.dframe <- read.table(file = "./q5.txt", sep = "!", header = TRUE)

# install.packages("car")
library("car")
data("Duncan")

library("ggplot2")
ggplot(data = Duncan, aes(education, income)) +
  geom_point() +
  xlim(0, 100)