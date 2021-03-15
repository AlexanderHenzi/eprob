#-------------------------------------------------------------------------------
# Packages, see:
# https://github.com/FK83/fdtest,
# https://github.com/AlexanderHenzi/isodistrreg
# https://github.com/AlexanderHenzi/eprob)
library(tidyverse)
library(isodistrreg)
library(crch)
library(eprob)
library(fdtest)

#-------------------------------------------------------------------------------
# Data (generated in case_study_fit_models.R)
load("precip_fcs_models.rda")

#-------------------------------------------------------------------------------
# General ggplot theme settings (Figure size: best 5x8 inch)
theme_set(theme_bw(base_size = 10))

#-------------------------------------------------------------------------------
# Functions

dmtest <- function (e1, e2, alternative = c("two.sided", "less", 
    "greater"), h = 1, power = 2, dm.weights = FALSE) {
# This function is copied from the "forecast" package and adapted to allow
# other weights for the covariance estimation. Reference for forecast package:
#
# Hyndman, Rob J., and Yeasmin Khandakar. Automatic time series for forecasting:
# the forecast package for R. No. 6/07. Clayton VIC, Australia: Monash
# University, Department of Econometrics and Business Statistics, 2007.
 alternative <- match.arg(alternative)
 d <- c(abs(e1))^power - c(abs(e2))^power
 n <- length(d)
 if (dm.weights) {
   d.cov <- acf(d, na.action = na.pass, lag.max = h - 1, type = "covariance", 
       plot = FALSE)$acf[, , 1]
   d.var <- sum(c(d.cov[1], 2 * d.cov[-1]))/length(d)
   dv <- d.var
   if (dv > 0) {
       STATISTIC <- mean(d, na.rm = TRUE)/sqrt(dv)
   }
   else if (h == 1) {
       stop("Variance of DM statistic is zero")
   }
   else {
       warning("Variance is negative, using horizon h=1")
       return(dm.test(e1, e2, alternative, h = 1, power))
   }
   k <- ((n + 1 - 2 * h + (h/n) * (h - 1))/n)^(1/2)
   STATISTIC <- STATISTIC * k
 } else {
   # Modification
   h <- floor(length(d)^{1/4})
   d.cov <- acf(d, na.action = na.pass, lag.max = h, type = "covariance", 
       plot = FALSE)$acf[, , 1]
   d.var <- sum(c(d.cov[1], 2 * d.cov[-1] * (1 - seq_len(h)/h)))/length(d)
   STATISTIC <- mean(d, na.rm = TRUE) / sqrt(d.var)
 }
 names(STATISTIC) <- "DM"
 if (alternative == "two.sided") {
     PVAL <- 2 * pt(-abs(STATISTIC), df = n - 1)
 }
 else if (alternative == "less") {
     PVAL <- pt(STATISTIC, df = n - 1)
 }
 else if (alternative == "greater") {
     PVAL <- pt(STATISTIC, df = n - 1, lower.tail = FALSE)
 }
 PARAMETER <- c(h, power)
 names(PARAMETER) <- c("Forecast horizon", "Loss function power")
 structure(list(statistic = STATISTIC, parameter = PARAMETER, 
     alternative = alternative, p.value = PVAL, method = "Diebold-Mariano Test", 
     data.name = c(deparse(substitute(e1)), deparse(substitute(e2)))), 
     class = "htest")
}

# Make out-of-sample predictions with HCLR fit (square root transformation!)
predict_hclr <- function(fit, data, at) {
 sqrtdata <- sqrt(as.matrix(select(data, hres, ctr, starts_with("p"))))
 data$sqrthres <- sqrtdata[, 1]
 data$sqrtctr <- sqrtdata[, 2]
 data$sqrtens <- apply(sqrtdata[, -(1:2)], 1, mean)
 data$sqrtenssd <- apply(sqrtdata[, -(1:2)], 1, sd)
 unname(c(predict(fit, data, at = sqrt(at), type = "probability")))
}

# Make out-of-sample predictions with IDR fit (compute ensemble mean)
predict_idr <- function(fit, data) {
 ens <- apply(data.matrix(select(data, starts_with("p"))), 1, mean)
 X <- data.frame(hres = data$hres, ctr = data$ctr, ens = ens)
 predict(fit, X, digits = 5)
}

# Wrapper for forecast dominance test
fd_test <- function(p, q, y, c) {
 mat <- cbind(p, q, y)[c, ]
 fdtest(na.omit(mat))$pval
}

#-------------------------------------------------------------------------------
# Evaluation

# Add forecasts to dataset
precip_scores <- precip_fcs_models %>%
 mutate(
  tail_precip = map_dbl(training, ~quantile(.$obs, 0.9)),
  validation = map2(
   .x = validation,
   .y = hclr,
   ~mutate(.x, pop_hclr = 1 - predict_hclr(.y, .x, at = 0))
  ),
  validation = map2(
   .x = validation,
   .y = hclr_noscale,
   ~mutate(.x, pop_hclr_noscale = 1 - predict_hclr(.y, .x, at = 0))
  ),
  validation = map2(
   .x = validation,
   .y = idr,
   ~mutate(.x, pop_idr = 1 - c(cdf(predict_idr(.y, .x), 0)))
  ),
  validation = pmap(
   .l = list(validation, hclr, tail_precip),
   .f = ~mutate(..1, tail_hclr = 1 - predict_hclr(..2, ..1, at = ..3))
  ),
  validation = pmap(
   .l = list(validation, hclr_noscale, tail_precip),
   .f = ~mutate(..1, tail_hclr_noscale = 1 - predict_hclr(..2, ..1, at = ..3))
  ),
  validation = pmap(
   .l = list(validation, idr, tail_precip),
   .f = ~mutate(..1, tail_idr = 1 - c(cdf(predict_idr(..2, ..1), ..3)))
  ) 
 ) %>%
 select(airport, lag, validation, tail_precip) %>%
 ungroup()

# Full date sequence for the dataset, so that all NA can be made explicit
precip_dates <- precip_scores %>%
 unnest(cols = validation) %>%
 group_by(airport, lag) %>%
 summarise(min_date = min(date), max_date = max(date)) %>%
 mutate(
  seq_date = map2(
   .x = min_date,
   .y = max_date,
   .f = ~seq(.x, .y, 1)
  )
 ) %>%
 select(-min_date, -max_date) %>%
 unnest(cols = seq_date) %>%
 rename(date = seq_date)

# Tests for hypotheses about probability of precipitation forecasts
tp <- "brier"
table_2 <- precip_scores %>%
 unnest(cols = validation) %>%
 full_join(precip_dates, by = c("airport", "lag", "date")) %>%
 group_by(airport, lag) %>%
 arrange(date) %>%
 mutate(y = as.numeric(obs > 0)) %>%
 group_by(airport, lag) %>%
  summarise(
  bs_idr = mean(score(y, pop_idr, tp), na.rm = TRUE),
  bs_hclr = mean(score(y, pop_hclr, tp), na.rm = TRUE),
  bs_hclr_noscale = mean(score(y, pop_hclr_noscale, tp), na.rm = TRUE),
  e_pop = etest(
   y = y,
   p = pop_idr,
   q = pop_hclr,
   alt = pop_hclr * 0.75 + pop_idr * 0.25,
   type = tp,
   h = lag[1]
  )$evalue,
  p_pop = dmtest(
   e1 = score(y, pop_idr, tp),
   e2 = score(y, pop_hclr, tp),
   power = 1, h = lag[1],
   alternative = "greater"
  )$p.value,
  e_pop_noscale_idr = etest(
   y = y,
   p = pop_hclr_noscale,
   q = pop_idr,
   alt = pop_idr * 0.75 + pop_hclr * 0.25,
   type = tp, h = lag[1]
  )$evalue,
  p_pop_noscale_idr = dmtest(
   e1 = score(y, pop_hclr_noscale, tp),
   e2 = score(y, pop_idr, tp),
   power = 1,
   h = lag[1],
   alternative = "greater"
  )$p.value,
  e_pop_noscale_hclr = etest(
   y = y,
   p = pop_hclr_noscale,
   q = pop_hclr, 
   alt = pop_hclr_noscale * 0.25 + pop_hclr * 0.75,
   type = tp,
   h = lag[1]
  )$evalue,
  p_pop_noscale_hclr = dmtest(
   e1 = score(y, pop_hclr_noscale, tp),
   e2 = score(y, pop_hclr, tp),
   power = 1,
   h = lag[1],
   alternative = "greater"
  )$p.value
 ) %>%
 mutate_at(vars(contains("bs")), round, digits = 3) %>%
 mutate_at(
  .vars = vars(starts_with("e_")),
  .funs = function(x) {
   out <- round(x, 3)
   out[out > 100] <- 999
   out <- as.character(out)
   out[out == "999"] <- "> 100"
   out
  }
 ) %>%
 mutate_at(
  .vars = vars(starts_with("p_")),
  .funs = function(x) {
   out <- round(x, 4)
   out[out == 0] <- 0
   out <- as.character(out)
   out[out == "0"] <- " < 1e-4"
   out
  }
 )
 # mutate_at(
 #  .vars = vars(starts_with("p_"), starts_with("e_")),
 #  format,
 #  scientific = TRUE,
 #  digits = 2
 # )

write.table(
 table_2[, -1],
 sep = "$ & $",
 eol = "$\\\\ \n",
 row.names = 
  c("BRU", rep("", 4), "FRA", rep("", 4), "LHR", rep("", 4), "ZRH", rep("", 4)),
 file = "case_study_table_1.txt"
)

# Tests for hypotheses about predictions for "extreme" precipitation
dominance <- precip_scores %>%
 mutate(validation = map2(
   .x = validation,
   .y = tail_precip,
   ~mutate(.x, y = as.integer(obs > .y))
 )) %>%
 unnest(cols = validation) %>%
 full_join(precip_dates, by = c("airport", "lag", "date")) %>%
 group_by(airport, lag) %>%
 arrange(date) %>%
 summarise(
  n = sum((tail_hclr >= 0.5 | tail_hclr_noscale >= 0.5), na.rm = TRUE),
  e_pop = etest(
   y = y,
   p = tail_hclr_noscale,
   q = tail_hclr,
   c = as.integer((tail_hclr_noscale >= 0.5)),
   alt = tail_hclr,
   type = "dominance",
   h = lag[1]
  )$evalue,
  p_pop = ifelse(
   lag[1] > 1,
   NA,
   fd_test(
    p = tail_hclr_noscale,
    q = tail_hclr,
    y = y,
    c = (tail_hclr >= 0.5 | tail_hclr_noscale >= 0.5)
   )
  )
 ) %>%
 mutate(p_pop = round(p_pop, 3), e_pop = round(e_pop, 3))
dominance

# Format to table as in paper
dominance$p_pop[is.na(dominance$p_pop)] <- ""
dominance$e_pop[nchar(dominance$p_pop) > 0] <- paste0(
 dominance$e_pop[nchar(dominance$p_pop) > 0],
 " (",
 dominance$p_pop[nchar(dominance$p_pop) > 0],
 ")"
)
dominance <- select(dominance, -p_pop)
case_study_table_2 <- dominance %>%
 pivot_wider(names_from = "airport", values_from = c("n", "e_pop")) %>%
 select(
  lag,
  contains("Bru"),
  contains("Fra"),
  contains("Lon"),
  contains("Zur")
 )

write.table(
 case_study_table_2,
 sep = "$ & $",
 eol = "$\\\\ \n",
 row.names = FALSE,
 file = "case_study_table_2.txt"
)

# Check influence of merging function: Merge all e-values by product
precip_scores %>%
   mutate(validation = map2(
      .x = validation,
      .y = tail_precip,
      ~mutate(.x, y = as.integer(obs > .y))
   )) %>%
   unnest(cols = validation) %>%
   full_join(precip_dates, by = c("airport", "lag", "date")) %>%
   group_by(airport, lag) %>%
   filter(tail_hclr >= 0.5 | tail_hclr_noscale >= 0.5) %>%
   select(airport, lag, date, tail_hclr, tail_hclr_noscale, y) %>%
   arrange(date) %>%
   mutate(
    e = cumprod((1 - y - tail_hclr) / (1 - y - tail_hclr_noscale)),
    xx = seq_along(e)
   ) %>%
   ggplot() +
   geom_hline(yintercept = c(1, 3.16, 10), lty = 5, color = "darkgray") +
   geom_line(aes(y = e, x = xx)) +
   facet_grid(cols = vars(airport), rows = vars(lag), scales = "free_x") +
   scale_y_log10() +
   labs(x = "Index", y = "E-value")

# Figure cumulative product of e-values
plot_dat <- precip_scores %>%
 ungroup() %>%
 filter(lag == 1) %>%
 unnest(cols = validation) %>%
 full_join(filter(precip_dates, lag == 1), by = c("airport", "lag", "date")) %>%
 group_by(airport) %>%
 arrange(date) %>%
 mutate(y = as.numeric(obs > 0)) %>%
 group_by(airport) %>%
 mutate(
  H3 = cumprod(replace_na(evalue(
   y = y,
   p = pop_hclr_noscale,
   q = pop_hclr, 
   alt = pop_hclr_noscale * 0.25 + pop_hclr * 0.75,
   type = tp
  ), 1)),
  H2 = cumprod(replace_na(evalue(
   y = y,
   p = pop_hclr_noscale,
   q = pop_idr, 
   alt = pop_hclr_noscale * 0.25 + pop_idr * 0.75,
   type = tp
  ), 1)),
  H1 = cumprod(replace_na(evalue(
   y = y,
   p = pop_idr,
   q = pop_hclr, 
   alt = pop_idr * 0.25 + pop_hclr * 0.75,
   type = tp
  ), 1))
 ) %>%
 gather(key = "hypo", value = "eval", H1, H2, H3) %>%
 mutate(
  hypo = fct_recode(
   hypo,
   "HCLR/IDR" = "H1",
   "IDR/HCLR['-']" = "H2",
   "HCLR/HCLR['-']" = "H3"
  )
 )

plot_dat %>%
 group_by(hypo, airport, lag) %>%
 summarise(
  reject = ifelse(all(eval < 10), length(eval), min(which(eval > 10))),
  n = length(eval),
  p = reject / n
 )

ggplot() +
 geom_hline(yintercept = 10, lty = 5, color = "darkgray") +
 geom_hline(yintercept = 1) +
 geom_line(
  data = plot_dat,
  aes(x = date, y = eval, color = airport, group = airport),
  lwd = 0.5
 ) +
 geom_point(
  data = filter(group_by(plot_dat, airport, hypo), seq_along(date) %% 400 == 0),
  aes(x = date, y = eval, color = airport, group = airport, shape = airport), 
  cex = 2.5
 ) +
 facet_grid(cols = vars(hypo), labeller = label_parsed) +
 scale_y_log10() +
 coord_cartesian(ylim = c(0.001, 1000)) +
 labs(x = "Year", y = "E-values", color = "Airport") +
 theme(legend.position = "none") +
 scale_color_grey(end = 0.5)
