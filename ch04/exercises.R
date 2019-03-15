# 4.1
a <- c(6,9,7,3,6,7,9,6,3,6,6,7,1,9,1)
a == 6
a >= 6
a < (6 + 2)
a != 6

b <- array(data = a, dim = c(2, 2, 3))
b <= (6 / 2 + 4)
(b + 1) <= (6 / 2 + 4)

diag(x = 10) == 0

any(b <= (6 / 2 + 4))
all(b <= (6 / 2 + 4))
any((b + 1) <= (6 / 2 + 4))
all((b + 1) <= (6 / 2 + 4))

any(diag(diag(x = 10) == 0))

# 4.2
foo <- c(7,1,7,10,5,9,10,3,10,8)
(foo > 5) | (foo == 2)

bar <- c(8,8,4,4,5,1,5,6,6,8)
(bar == 6) & (bar != 4)

((foo > 5) | (foo == 2)) & ((bar == 6) & (bar != 4))

baz <- foo + bar
(baz >= 14) & (baz != 15)
((baz / foo) > 4) | ((baz / foo) <= 2)

(foo > 5) || (foo == 2)
(bar == 6) && (bar != 4)
((foo > 5) || (foo == 2)) && ((bar == 6) && (bar != 4))
(baz >= 14) && (baz != 15)
((baz / foo) > 4) || ((baz / foo) <= 2)

# 4.3
foo <- c(7,5,6,1,2,10,8,3,8,2)
bar <- foo[foo >= 5]
foo[-which(x = foo >= 5)]

baz <- matrix(data = bar, nrow = 2, ncol = 3, byrow = TRUE)

baz [baz == 8] <- baz[1, 2]^2
all(baz <= 25 & baz > 4)

qux <- array(data = c(10,5,1,4,7,4,3,3,1,3,4,3,1,7,8,3,7,3),
             dim = c(3,2,3))
which(x = qux == 3 | qux == 4, arr.ind = TRUE)
qux[qux < 3 | qux >= 7] <- 100

foo[c(FALSE, TRUE)]
# foo[c(0, 1)] # no

# 4.4
paste("The quick brown fox\n  jump over\n    the lazy dogs")

num1 <- 4
num2 <- 0.75
paste("The result of multiplying", num1, "by", num2, "is", num1 * num2)

sub(pattern = "tdavies", replacement = "mharris",
    x = "/Users/tdavies/Documents/RBook/")

bar <- "How much wood could a woodchuck chuck"
bar <- paste(bar, "if a woodchuck could chuck wood")
gsub(pattern = "wood", replacement = "metal", x = bar)

e <- "Two 6-packs for $12.99"
"6-pack" == substr(x = e, start = 5, stop = 10)
e <- sub(pattern = "12\\.99", replacement = "10.99", x = e)