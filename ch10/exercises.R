# 10.1
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)

if((vec1[1]+vec2[2])==10){ cat("Print me!") }       # Print me!
if(vec1[1]>=2&&vec2[1]>=2){ cat("Print me!") }      # Print me!
if(all((vec2-vec1)[c(2,6)]<7)){ cat("Print me!") }  # nothing
if(!is.na(vec2[3])){ cat("Print me!") }             # Print me!

ifelse(vec1 + vec2 > 3, vec1 * vec2, vec1 + vec2)

if (any(toupper(substr(diag(mymat), 1, 1)) == "G")) {
  i <- which(toupper(substr(diag(mymat), 1, 1)) == "G")
  diag(mymat)[i] <- "HERE"
} else {
  mymat <- diag(nrow(mymat))
}
mymat

mymat <- matrix(as.character(1:16),4,4)
mymat <- matrix(c("DANDELION","Hyacinthus","Gerbera","MARIGOLD","geranium","ligularia","Pachysandra","SNAPDRAGON","GLADIOLUS"),3,3)
mymat <- matrix(c("GREAT","exercises","right","here"),2,2,byrow=T)

# 10.2
switch(mynum,12,34,56,78,NA)

if (mynum == 1) {
  12
} else if (mynum == 2) {
  34
} else if (mynum == 3) {
  56
} else if (mynum == 4) {
  78
} else {
  NA
}

mynum <- 3 # 56

if (any(doselevel == "High")) {
  lowdose <- ifelse(lowdose >= 10, 10, lowdose / 2)
  meddose <- ifelse(meddose >= 26, 26, meddose)
  highdose <- ifelse(highdose < 60, 60, highdose * 1.5)
  
  dosage <- rep(x = lowdose, length.out = length(doselevel))
  dosage[which(doselevel == "Med")] <- meddose
  dosage[which(doselevel == "High")] <- highdose
} else {
  doselevel <- factor(doselevel,
                      levels = c("Low","Med"),
                      labels = c("Small","Large"))
  
  if (lowdose < 15 && meddose < 35) {
    lowdose <- lowdose * 2
    meddose <- meddose + highdose
  }
  
  dosage <- rep(x = lowdose, length.out = length(doselevel))
  dosage[doselevel == "Large"] <- meddose
}

lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","High","High","High","Low","Med","Med"),levels=c("Low","Med","High"))

lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","Low","Low","Med","Low","Med","Med"),levels=c("Low","Med","High"))

lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","Med","Med"),levels=c("Low","Med","High"))

lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","High","High","High","Low","Med","Med"),levels=c("Low","Med","High"))

# skipped c

# 10.3
# skipped a

mystrings <- c("Peter","Homer","Lois","Stewie","Maggie","Bart")
mynums <- rep(NA, length(mystrings))
for(i in 1:length(mystrings)) {
  mynums[i] <- switch(EXPR=mystrings[i], Homer=12, Marge=34, Bart=56, Lisa=78, Maggie=90, NA)
}

count <- 0
for(i in mylist) {
  if (is.matrix(i)) {
    count <- count + 1
  } else if(is.list(i)) {
    for(j in i) {
      count <- ifelse(is.matrix(j), count + 1, count)
    }
  }
}

mylist <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),cc=matrix(c(T,T,F,T,F,F),3,2),dd="string here",ee=list(c("hello","you"),matrix(c("hello","there"))),ff=matrix(c("red","green","blue","yellow")))
# 4
mylist <- list("tricked you",as.vector(matrix(1:6,3,2)))
# 0
mylist <- list(list(1,2,3),list(c(3,2),2),list(c(1,2),matrix(c(1,2))),rbind(1:10,100:91))
# 2

# 10.4

# 10.5

# b
matlist <- list(matrix(c(T,F,T,T),2,2),
                matrix(c("a","c","b","z","p","q"),3,2),
                matrix(1:8,2,4))
matlist
i <- length(matlist)
for(list in matlist){
  matlist[[i]] <- t(list)
  i <- i +1
}
matlist