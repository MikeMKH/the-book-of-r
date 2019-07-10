# 26.1
library("MASS")
?survey
summary(survey)
pairs(survey)

survey <- na.omit(survey[,c("Wr.Hnd", "NW.Hnd", "W.Hnd", "Sex", "Height")])
summary(survey)
pairs(survey)

library("rgl")
plot3d(z = survey$Height, x = survey$Wr.Hnd, y = survey$NW.Hnd)

plot3d(z = survey$Height[survey$W.Hnd == "Right"],
       x = survey$Wr.Hnd[survey$W.Hnd == "Right"],
       y = survey$NW.Hnd[survey$W.Hnd == "Right"],
       col = c("black", "red")[as.numeric(survey$Sex[survey$W.Hnd == "Right"])],
       size = 4, xlab = "Writing hand", ylab = "Non-writing hand", zlab = "Height")
plot3d(z = survey$Height[survey$W.Hnd == "Left"],
       x = survey$Wr.Hnd[survey$W.Hnd == "Left"],
       y = survey$NW.Hnd[survey$W.Hnd == "Left"],
       col = c("black", "red")[as.numeric(survey$Sex[survey$W.Hnd == "Left"])],
       size = 10, xlab = "Writing hand", ylab = "Non-writing hand", zlab = "Height")
legend3d("topleft", pch = 19, pt.cex = c(0.8, 0.8, 1.5, 1.5),
         col = c("black", "red", "black", "red"),
         legend = c("Male LH", "Female RH", "Male LH", "Female LH"))

# skip c