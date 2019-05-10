# 17.1
test.total <- 65
test.mean <- 41.1
test.sd <- 11.3
test.se <- test.sd / sqrt(6)

pnorm(55, mean = test.mean, sd = test.se) -
  pnorm(45, mean = test.mean, sd = test.se)

pnorm(32.5, mean = test.mean, sd = test.se)

survey.n <- 140
survey.A <- 35
survey.B <- survey.n - survey.A

# n * pi > 5 and n * (1 - pi) > 5
survey.pi <- survey.A / survey.n
survey.n * survey.pi
survey.n * (1 - survey.pi)

1 - pnorm(.4, mean = survey.pi,
          sd = sqrt(survey.pi * (1 - survey.pi) / survey.total))

qnorm(.9, mean = survey.pi,
      sd = sqrt(survey.pi * (1 - survey.pi) / survey.total))
qnorm(.1, mean = survey.pi,
      sd = sqrt(survey.pi * (1 - survey.pi) / survey.total))

# skip g - h