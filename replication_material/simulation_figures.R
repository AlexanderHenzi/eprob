#-------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(ggpubr)

#-------------------------------------------------------------------------------
# Data
load("simulation_1_results.rda")

#-------------------------------------------------------------------------------
# General ggplot theme settings (Figure size: best 5x8 inch)
theme_set(theme_bw(base_size = 10))

#-------------------------------------------------------------------------------
# Figures simulation 1

## Preformat datasets
dat_eval <- simulation_1_evalue %>%
 gather(key = "mu", value = "rej", starts_with("mu")) %>%
 mutate(mu = parse_number(mu))
dat_pval <- simulation_1_pvalue %>%
 gather(key = "mu", value = "rej", starts_with("mu")) %>%
 mutate(mu = parse_number(mu))

## Select parameters (modify to obtain the figures in the Online Supplement)
p_type <- "brier"
p_alpha <- 0.05
p_n <- 600
p_method <- "student"

## Panels
fig1_a <- dat_pval %>%
 filter(
  type == p_type &
  alpha == p_alpha &
  n == p_n &
  method == p_method
 ) %>%
 mutate(nstops = factor(nstops)) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(yintercept = 0.05, col = "darkgray", lty = 5) +
 geom_point(
  aes(x = mu, y = rej, color = nstops, group = nstops, pch = nstops)
 ) +
 geom_line(aes(x = mu, y = rej, color = nstops, group = nstops)) +
 labs(
  x = expression(mu),
  y = "Rejection rate",
  color = "Stops",
  pch = "Stops"
 ) +
 theme(legend.position = "none") +
 coord_cartesian(xlim = c(0.3, 0.6), ylim = c(0, 0.25)) +
 ggtitle("(a)") +
 scale_color_grey(end = 0.5)

fig1_b <- dat_eval %>%
 filter(
  type == p_type &
  alpha == p_alpha &
  n == p_n &
  k == 1
 ) %>%
 mutate(stopped = ifelse(stopped, "Stopped", "Unstopped")) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(yintercept = p_alpha, col = "darkgray", lty = 5) +
 geom_point(
  aes(x = mu, y = rej, color = stopped, group = stopped, pch = stopped)
 ) +
 geom_line(aes(x = mu, y = rej, color = stopped, group = stopped)) +
 labs(
  x = expression(mu),
  y = element_blank(),
  color = element_blank(),
  pch = element_blank()
 ) +
 theme(legend.position = "none") +
 coord_cartesian(xlim = c(0.3, 0.9)) +
 ggtitle("(b)") + 
 scale_color_grey(end = 0.5)

fig1_c <- dat_eval %>%
 filter(
  k != 2 & k != 4 & k!= 3 &
  stopped &
  type == p_type &
  alpha == p_alpha &
  n == p_n
 ) %>%
 mutate(k = factor(k)) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(yintercept = p_alpha, col = "darkgray", lty = 5) +
 geom_point(
  aes(x = mu, y = rej, color = k, group = k, pch = k)
 ) +
 geom_line(
  aes(x = mu, y = rej, color = k, group = k)
 ) +
 labs(
  x = expression(mu),
  y = "Rejection rate",
  color = "Alternative",
  pch = "Alternative"
 ) +
 theme(legend.position = "none") +
 coord_cartesian(xlim = c(0.3, 0.9)) +
 ggtitle("(c)") +
 scale_color_grey(end = 0.5)


fig1_d <- dat_pval %>%
 filter(
  type == p_type &
  alpha == p_alpha &
  n %in% c(300, 600, 1200) &
  method == p_method &
  nstops == 0
 ) %>%
 select(-nstops, -type, -alpha) %>%
 bind_rows(
  select(mutate(filter(
   dat_eval,
   n %in% c(300, 600, 1200) &
   alpha == p_alpha &
   stopped &
   k == 5 &
   type == p_type
   ), method = "e-value"), -type, -alpha, -k)
 ) %>%
 mutate(n = factor(n)) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(yintercept = p_alpha, col = "darkgray", lty = 5) + 
 geom_point(
  aes(x = mu, y = rej, color = n, group = interaction(n, method), pch = n)
 ) +
 geom_line(
  aes(x = mu, y = rej, color = n, group = interaction(n, method), lty = method)
 ) +
 scale_linetype_manual(values = c(1, 4)) +
 theme(legend.position = "none") +
 labs(x = expression(mu), y = element_blank()) +
 coord_cartesian(xlim = c(0.3, 0.9)) +
 ggtitle("(d)") + 
 scale_color_grey(end = 0.5)

ggarrange(fig1_a, fig1_b, fig1_c, fig1_d)

#-------------------------------------------------------------------------------
# Figures simulation 2
load("simulation_2_results.rda")

p_alpha <- 0.05
p_type <- "brier"
p_n <- c(300, 600, 1200, 2400)

simulation_2_results %>%
 gather(key = "method", value = "rej", starts_with("rej")) %>%
 filter(
  alpha == p_alpha &
  n %in% p_n &
  type == p_type
 ) %>%
 ggplot() +
 geom_point(
  aes(x = theta, y = rej, group = method, shape = method, color = method)
 ) +
 geom_line(aes(x = theta, y = rej, group = method, color = method)) +
 facet_grid(rows = vars(n), cols = vars(lag)) +
 geom_hline(yintercept = p_alpha, col = "darkgray", lty = 5) +
 geom_vline(xintercept = 0, col = "darkgray", lty = 5) +
 theme(legend.position = "none") +
 labs(x = expression(theta), y = "Rejection rate") +
 scale_color_grey(end = 0.5) +
 coord_cartesian(xlim = c(0, 0.7))

#-------------------------------------------------------------------------------
# Figures supplementary material

## Remove "theme(legend.position = "none")" to see the legends

## Simulation example 1: Rejection rates of e-values with different scoring
## functions, sample sizes and significance levels (alternative: "k = 1")
dat_eval %>%
 filter(stopped & k == 1) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(aes(yintercept = alpha), col = "darkgray", lty = 5) +
 geom_point(aes(x = mu, y = rej, color = type, group = type, shape = type)) +
 geom_line(aes(x = mu, y = rej, color = type, group = type)) +
 facet_grid(rows = vars(n), cols = vars(alpha)) +
 scale_color_grey(end = 0.5) +
 theme(legend.position = "none") +
 labs(x = expression(mu), y = "Rejection rate") +
 coord_cartesian(xlim = c(0.5, 1))

## Simulation example 1: Rejection rates of e-values with different alternative
## hypotheses, sample sizes and significance levels (score: Brier)
dat_eval %>%
 filter(stopped & type == "brier" & !(k %in% c(2, 4))) %>%
 mutate(k = as.factor(k)) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(aes(yintercept = alpha), col = "darkgray", lty = 5) +
 geom_point(aes(x = mu, y = rej, color = k, group = k, shape = k)) +
 geom_line(aes(x = mu, y = rej, color = k, group = k)) +
 facet_grid(rows = vars(n), cols = vars(alpha)) +
 scale_color_grey(end = 0.5) +
 theme(legend.position = "none") +
 labs(x = expression(mu), y = "Rejection rate") +
 coord_cartesian(xlim = c(0.5, 1))

## Simulation example 1: Rejection rates of Student's t-test with different
## scoring functions, sample sizes and significance levels
dat_pval %>%
 filter(nstops == 0 & method == "student") %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(aes(yintercept = alpha), col = "darkgray", lty = 5) +
 geom_point(aes(x = mu, y = rej, color = type, group = type, shape = type)) +
 geom_line(aes(x = mu, y = rej, color = type, group = type, linetype = type)) +
 facet_grid(rows = vars(n), cols = vars(alpha)) +
 scale_color_grey(end = 0.5) +
 theme(legend.position = "none") +
 labs(x = expression(mu), y = "Rejection rate") +
 coord_cartesian(xlim = c(0.5, 1))

## Simulation example 1: Rejection rates of Student's t-test and Wilcoxon's
## signed-rank test, for varying significance level and sample size (score:
## Brier)
dat_pval %>%
 filter(nstops == 0 & type == "brier") %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(aes(yintercept = alpha), col = "darkgray", lty = 5) +
 geom_point(aes(x = mu, y = rej, color = method, group = method, shape = method)) +
 geom_line(aes(x = mu, y = rej, color = method, group = method)) +
 facet_grid(rows = vars(n), cols = vars(alpha)) +
 scale_color_grey(end = 0.5) +
 theme(legend.position = "none") +
 labs(x = expression(mu), y = "Rejection rate") +
 coord_cartesian(xlim = c(0.5, 1))

## Simulation example 1: Rejection rates of e-values Student's t-test under
## optional stopping, for varying sample sizes and significance levels (score:
## Brier)
dat_pval %>%
 filter(type == "brier" & method == "student") %>%
 mutate(nstops = factor(nstops)) %>%
 ggplot() +
 geom_vline(xintercept = 0.5, col = "darkgray", lty = 5) +
 geom_hline(aes(yintercept = alpha), col = "darkgray", lty = 5) +
 geom_point(
  aes(x = mu, y = rej, color = nstops, group = nstops, shape = nstops)
 ) +
 geom_line(aes(x = mu, y = rej, color = nstops, group = nstops)) +
 facet_grid(cols = vars(n), rows = vars(alpha), scales = "free_y") +
 scale_color_grey(end = 0.5) +
 theme(legend.position = "none") +
 labs(x = expression(mu), y = "Rejection rate") +
 coord_cartesian(xlim = c(0.4, 0.55), ylim = c(0, 0.15))

## Simulation example 2: Rejection rates at the 5% level for different 
## scoring functions
simulation_2_results %>%
 gather(key = "method", value = "rej", starts_with("rej")) %>%
 filter(alpha == 0.05) %>%
 ggplot() +
 geom_point(aes(
  x = theta,
  y = rej,
  group = interaction(method, type),
  shape = type,
  color = type
 )) +
 geom_line(aes(
  x = theta,
  y = rej,
  group = interaction(method, type),
  linetype = method,
  color = type
 )) +
 facet_grid(rows = vars(n), cols = vars(lag)) +
 geom_hline(yintercept = p_alpha, col = "darkgray", lty = 5) +
 geom_vline(xintercept = 0, col = "darkgray", lty = 5) +
 theme(legend.position = "none") +
 labs(x = expression(theta), y = "Rejection rate") +
 scale_color_grey(end = 0.5) +
 coord_cartesian(xlim = c(0, 0.7))

## Simulation example 2: Rejection rates for different significance levels and
## sample size 600 (score: Brier)
simulation_2_results %>%
 gather(key = "method", value = "rej", starts_with("rej")) %>%
 filter(type == "brier" & n == 600) %>%
 mutate(
  alpha = factor(alpha),
  method = fct_recode(
   method,
   "Diebold-Mariano" = "rej_pval",
   "E-value" = "rej_eval"
  )
 ) %>%
 ggplot() +
 geom_point(aes(
  x = theta,
  y = rej,
  group = alpha,
  shape = alpha,
  color = alpha
 )) +
 geom_line(aes(
  x = theta,
  y = rej,
  group = alpha,
  linetype = alpha,
  color = alpha
 )) +
 facet_grid(rows = vars(method), cols = vars(lag)) +
 geom_hline(yintercept = p_alpha, col = "darkgray", lty = 5) +
 geom_vline(xintercept = 0, col = "darkgray", lty = 5) +
 theme(legend.position = "none") +
 labs(x = expression(theta), y = "Rejection rate") +
 scale_color_grey(end = 0.5) +
 coord_cartesian(xlim = c(0, 0.7))
