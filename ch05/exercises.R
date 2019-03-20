# 5.1
a <- list(
  seq(from = -4, to = 4, length.out = 20),
  matrix(data = c(F,T,T,T,F,T,T,F,F), ncol = 3, nrow = 3),
  c("don", "quixote"),
  factor(x = c("LOW","MED","LOW","MED","MED","HIGH"))
)

a[[2]][c(2, 1)]

a[[3]] <- sub(pattern = "don", replacement = "Don", a[[3]])
a[[3]] <- sub(pattern = "quixote", replacement = "Quixote", a[[3]])
cat("\"Windmills! ATTACK!\"\n  -\\", a[[3]][1], a[[3]][2], "/-")

a[[1]][a[[1]]>1]

which(x = a[[4]] == "MED")

b <- list(
  facs = a[[4]],
  nums = c(3,2.1,3.3,4,1.5,4.9),
  oldlist = a[1:3]
)

b$facs[b$nums >= 3]
b$flags <- rep(b$oldlist[[2]][,3], times = 2)
b$nums[!b$flags]
b$oldlist[[3]] <- "Don Quixote" # deep copy, thus a is untouched
a[[3]]

# 5.2
dframe = data.frame(
  person = c("Stan", "Franchine", "Steve", "Roger", "Hayley", "Klaus"),
  sex = factor(c("M", "F", "M", "M", "F", "M")),
  funny = factor(c("High", "Med", "Low", "High", "Med", "Med")),
  stringsAsFactors = FALSE
)

dframe$age <- c(41, 41, 15, 21, 60, 16000)
dframe <- dframe[c(1, 4, 2, 3)]
# skip d - e
dframe[
  dframe$sex == "F"
  & (dframe$funny == "Med" | dframe$funny == "High")
  , c(1, 2)]
dframe[substr(x = dframe$person, start = 1, stop = 1) == "S",]