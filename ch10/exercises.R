# 10.1
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)

if((vec1[1]+vec2[2])==10){ cat("Print me!") }       # Print me!
if(vec1[1]>=2&&vec2[1]>=2){ cat("Print me!") }      # Print me!
if(all((vec2-vec1)[c(2,6)]<7)){ cat("Print me!") }  # nothing
if(!is.na(vec2[3])){ cat("Print me!") }             # Print me!

ifelse(vec1 + vec2 > 3, vec1 * vec2, vec1 + vec2)

if(any(toupper(substr(diag(mymat), 1, 1)) == "G")) {
  i <- which(toupper(substr(diag(mymat), 1, 1)) == "G")
  diag(mymat)[i] <- "HERE"
} else {
  mymat <- diag(nrow(mymat))
}
mymat

mymat <- matrix(as.character(1:16),4,4)
mymat <- matrix(c("DANDELION","Hyacinthus","Gerbera","MARIGOLD","geranium","ligularia","Pachysandra","SNAPDRAGON","GLADIOLUS"),3,3)
mymat <- matrix(c("GREAT","exercises","right","here"),2,2,byrow=T)