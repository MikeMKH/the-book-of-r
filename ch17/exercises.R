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

# 17.2
runner.n <- 34
runner.mean <- 14.22
runner.sd <- 2.9
runner.se <- runner.sd / sqrt(runner.n)

runner.mean + c(-1, 1) * qnorm(.9) * runner.se
runner.mean + c(-1, 1) * qt(.9, df = runner.n - 1) * runner.se

runner.sample <- rnorm(n = 5, mean = runner.mean, sd = runner.se)
runner.sample

hand.n <- 400
hand.left <- 37
hand.ambidextrous <- 11

hand.hat <- hand.left / hand.n
hand.se <- sqrt(hand.hat * (1 - hand.hat) / hand.n)
hand.hat + c(-1, 1) * qnorm(1 - .01 / 2) * hand.se

hand.hat <- (hand.left + hand.ambidextrous) / hand.n
hand.se <- sqrt(hand.hat * (1 - hand.hat) / hand.n)
hand.hat + c(-1, 1) * qnorm(1 - .01 / 2) * hand.se

# skipped e - f