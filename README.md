
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eprob package: Drawing inference on probability forecast performance

<!-- badges: start -->
<!-- badges: end -->

## Installation

Install the the development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("AlexanderHenzi/eprob")
```

## Examples

Below are basic examples which illustrate how to use the package:

``` r
library(eprob)

# Simulation example 1 (see ?sim1). For mu > 0.5, q is better than p.
set.seed(321)
tp <- "brier"
sim <- sim1(n = 1000, mu = 0.8, type = tp)
p <- sim$p
q <- sim$q
y <- sim$y

## E-value for rejecting "H0: p is better than q", under the alternative
## hypothesis that the true probability is q. A large e-value indicates
## evidence against null hypothesis.
sim_e <- etest(p = p, q = q, y = y, alt = q, h = 1, type = tp)$evalue
sim_e

## If E is an e-value, then 1/E is a p-value. Here 1/E is very small.
1 / sim_e

## The result depends on the alternative "alt", and there is no default!
## Read the suggestions on how to choose "alt" in the preprint.
etest(p = p, q = q, y = y, alt = q * 0.75 + p * 0.25, h = 1, type = tp)$evalue
etest(p = p, q = q, y = y, alt = q * 0.6 + p * 0.4, h = 1, type = tp)$evalue

## If the goal is rejecting at a significance level "alpha", use "stopped"
## e-values. Stopped e-values have better power for rejecting at the given
## significance level. Here, the null hypothesis is already rejected with
## 115 observations (and this is valid!).
etest(p = p, q = q, y = y, alt = q, h = 1, type = tp, stopped = TRUE,
 alpha = 0.05)

# Simulation example 2 (see ?sim2). Lagged forecasts. In this example, p_j is
# the ideal forecast for y at lag j (j = 1: one "day" ahead, j = 2: two "days"
# ahead, ...).

set.seed(234)
sim <- sim2(n = 1000, theta = 0.5, h = 3)
p <- sim$p_3
q <- sim$p_2
y <- sim$y

## Try to reject the null hypothesis that p_2 is better than p_3 as a
## "day 2" forecast.
sim_e <- etest(p = p, q = q, y = y, alt = q, h = 2, type = tp)$evalue
sim_e

## Corresponding p-value is
1 / sim_e

# Be cautious when data contains NA!
set.seed(345)
n <- 1000
p <- runif(n)
q <- runif(n)
y <- rbinom(n, 1, q)
na_ind <- sample(n, 200)
y[na_ind] <- NA

## Assume lag 3. Correct e-value (NA included):
etest(p = p, q = q, y = y, alt = q, h = 3, type = tp)$evalue

## Wrong e-value when NA are removed:
p_w <- p[-na_ind]
q_w <- q[-na_ind]
y_w <- y[-na_ind]
etest(p = p_w, q = q_w, y = y_w, alt = q_w, h = 3, type = tp)$evalue
```

## References

The method is described in

Alexander Henzi, Johanna F Ziegel, Valid sequential inference on probability forecast performance, Biometrika, Volume 109, 2022, Pages 647–663, https://doi.org/10.1093/biomet/asab047

Alexander Henzi, Johanna F Ziegel, Correction to: ‘Valid sequential inference on probability forecast performance’, Biometrika, 2022;, asac043, https://doi.org/10.1093/biomet/asac043

The article is also available on <https://arxiv.org/abs/2103.08402>.

Replication material for all results in the preprint is contained in the
folder `replication_material`.
