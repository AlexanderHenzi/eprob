#-------------------------------------------------------------------------------
# Packages
library(eprob)
library(forecast)

#-------------------------------------------------------------------------------
# Simulation parameters

## Scoring function
type <- "brier"

## Sample size
n <- 600

## Significance level
alpha <- 0.05

## Parameters "theta" (see Section 4.2 in the article)
theta <- 0.5

## Number of Monte Carlo replications
m <- 10000

#-------------------------------------------------------------------------------
# Run simulation
rej_pval <- numeric(3)
rej_eval <- numeric(3)
for (i in seq_len(m)) {
 sim <- sim2(n = n, theta = theta, h = 4)
 y <- sim$y
 for (j in seq_len(3)) {
  p <- sim[[5 - j]]
  q <- sim[[5 - j + 1]]
  pval <- dm.test(
   score(y = y, p = p, type = type),
   score(y = y, p = q, type = type),
   h = j,
   power = 1,
   alternative = "greater"
  )$p.value
  rej_pval[j] <- rej_pval[j] + (pval <= alpha)
  eval <- etest(
   y = y,
   p = p,
   q = q,
   alt = q,
   type = type,
   h = j,
   stopped = TRUE,
   alpha = alpha
  )$evalue
  rej_eval[j] <- rej_eval[j] + (eval >= 1 / alpha)
 }
}

rej_eval <- rej_eval / m
rej_pval <- rej_pval / m
rej_eval
rej_pval
