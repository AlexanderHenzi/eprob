#-------------------------------------------------------------------------------
# Packages
library(eprob)

#-------------------------------------------------------------------------------
# Simulation parameters

## Scoring function
type <- "brier"

## Sample size
n <- 600

## Significance level
alpha <- 0.05

## Parameters "mu" and "k" (see Section 4.1 in the paper); set k = -1 for
## alternative "pi_t" and k = 0 for alternative "q_t"
mu <- 0.7
k <- 1

## Whether the e-value is with or without stopping
stopped <- TRUE

## Function for computing p-value (t.test or wilcox.test)
testfun <- t.test

## Optional stops for p-value (opt_stops = n for no optional stopping; otherwise
## a vector of indices for stopping)
opt_stops <- n

## Number of Monte Carlo replications
m <- 10000

#-------------------------------------------------------------------------------
# Run simulation
rej_eval <- logical(m)
rej_pval <- logical(m)
for (i in seq_len(m)) {
 sim <- sim1(n = n, mu = mu, type = type)
 p <- sim$p
 q <- sim$q
 y <- sim$y
 
 # E-value
 bnd <- boundary(p, q, type)
 if (k == -1) {
  if (mu < 0.5) {
    dd <- 0
  } else {
   dd <- (pi_t - bnd)
  }
 } else if (k == 0) {
  dd <- (q - bnd)
 } else {
  dd <- (q - bnd) / (k + 1)
 }
 e <- cumprod(evalue(y = y, p = p, q = q, alt = bnd + dd, type = type))
 if (k > 1) {
  for (j in 2:k) e <- e +
   cumprod(evalue(y = y, p = p, q = q, alt = bnd + j * dd, type = type))
  e <- e / k
 }
 if (stopped) {
  rej_eval[i] <- (max(e) >= 1 / alpha)
 } else {
  rej_eval[i] <- (e[n] >= 1 / alpha)
 }
 
 # P-value
 score_p <- score(y, p, type)
 score_q <- score(y, q, type)
 
 pvals <- sapply(
  X = opt_stops,
  FUN = function(ii) {
   testfun(
    score_p[seq_len(ii)] - score_q[seq_len(ii)],
    alternative = "greater"
   )$p.value
  }
 )
 rej_pval[i] <- any(pvals <= alpha)
}

# Rejection rates
mean(rej_eval)
mean(rej_pval)