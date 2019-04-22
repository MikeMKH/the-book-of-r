# 13.1
  # a.i numeric discrete
  # a.ii categorical ordinal
  # a.iii numeric continuous
  # a.iv categorical ordinal
  # a.v categorical nominal
  # a.vi numeric continuous

# skipped b

# 13.2
round(mean(quakes$depth >= 300), 2)

mean(quakes$mag[quakes$depth >= 300])
median(quakes$mag[quakes$depth >= 300])

# c is dumb using tapply
tapply(chickwts$weight, chickwts$feed, mean)

# numeric discrete and categorical nominal
ctab <- table(InsectSprays$count)
ctab[ctab == max(ctab)]

tapply(InsectSprays$count, InsectSprays$spray, sum)

# skipped g
tapply(InsectSprays$count, InsectSprays$spray,
       function(x) round(mean(x >= 5) * 100.0, 2))