# 15.1
4/52 # Pr(ace)
1/52 # Pr(4 of spades)

13/52 # Pr(club | red)

13/52 # Pr(club)
13/51 # Pr(club | red)

(4 * 3)/52      # Pr(face)
(2 * 13)/52     # Pr(black)
((4 * 3)/2)/ 52 # Pr(face | black)
(((4 * 3)/2)/ 52) *
  ((2 * 13)/52) # Pr(face and black)

# 15.2
  # realized, discrete
  # random, discrete
  # random, discrete
  # random, continuous
  # realized, discrete
  # random, continuous

S.stars <- 1:5
1 - .1 - .13 - .21 - .15
S.probility <- c(.1, .13, .21, .41, .15)

S.cumulative <- cumsum(S.probility)

mu.S <- sum(S.probility * S.stars)

var.S <- sum(S.probility * (mu.S - S.stars)^2)
sd.S <- sqrt(var.S)

sum(S.probility[3:5])

# unimodal, symmetric, skew left
barplot(S.probility, names.arg=S.stars,
        space=0, ylim=c(0,0.5),
        xlab="stars", ylab="Pr(S = stars)")

fw <- function(w) {
  w.upper <- w >  65 & w <= 90
  w.lower <- w >= 40 & w <= 65
  
  result <- rep(0, length(w))
  result[w.upper] <- (90 - w[w.upper]) / 625
  result[w.lower] <- (w[w.lower] - 40) / 625	
  return(result)
}

Fw <- function(w) {
  w.upper <- w >  65 & w <= 90
  w.lower <- w >= 40 & w <= 65
  
  result <- rep(0, length(w))
  result[w.upper] <- (180 * w[w.upper] - w[w.upper]^2 - 6850) / 1250
  result[w.lower] <- (w[w.lower]^2 - 80 * w[w.lower] + 1600) / 1250
  result[w > 90] <- 1
  
  return(result)
}

fw(55.2)
Fw(55.2)

1 - Fw(60)
Fw(76.89) - Fw(60.3)

  # bimodal, symmetric
  # trimodal, asymmetric, skew right
  # unimodal, symmetric
  # unimodal, asymmetric, skew right