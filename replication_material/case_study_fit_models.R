#-------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(isodistrreg)
library(crch)

#-------------------------------------------------------------------------------
# Data (generated in case_study_format_data.R)
load("precip_fcs.rda")

precip_fcs <- precip_fcs %>%
 group_by(airport, lag) %>%
 nest() %>%
 mutate(
  data = map(data, ~arrange(., date)),
  training = map(
   data,
   ~filter(., seq_along(date) <= floor(length(date)/2)  - 5)
  ),
  validation = map(data, ~filter(., seq_along(date) > floor(length(date)/2)))
 ) %>%
 select(-data)

#-------------------------------------------------------------------------------
# Functions

# fit idr to training data
fit_idr <- function(data) {
 ens <- apply(data.matrix(select(data, starts_with("p"))), 1, mean)
 X <- data.frame(hres = data$hres, ctr = data$ctr, ens = ens)
 idr(y = data$obs, X = X)
}

# fit hclr to training data
fit_hclr <- function(data) {
 sqrtdata <- sqrt(as.matrix(select(data, hres, ctr, starts_with("p"))))
 data$sqrthres <- sqrtdata[, 1]
 data$sqrtctr <- sqrtdata[, 2]
 data$sqrtens <- apply(sqrtdata[, -(1:2)], 1, mean)
 data$sqrtenssd <- apply(sqrtdata[, -(1:2)], 1, sd)
 data$sqrtobs <- sqrt(data$obs)
 crch(
  data = data,
  formula = sqrtobs ~ sqrthres + sqrtctr + sqrtens | sqrtenssd,
  dist = "logistic",
  left = 0,
  type = "crps"
 )
}

# fit hclr without scale parameter to training data
fit_hclr_noscale <- function(data) {
 sqrtdata <- sqrt(as.matrix(select(data, hres, ctr, starts_with("p"))))
 data$sqrthres <- sqrtdata[, 1]
 data$sqrtctr <- sqrtdata[, 2]
 data$sqrtens <- apply(sqrtdata[, -(1:2)], 1, mean)
 data$sqrtenssd <- apply(sqrtdata[, -(1:2)], 1, sd)
 data$sqrtobs <- sqrt(data$obs)
 crch(
  data = data,
  formula = sqrtobs ~ sqrthres + sqrtctr + sqrtens,
  dist = "logistic",
  left = 0,
  type = "crps"
 )
}

#-------------------------------------------------------------------------------
# Fit models for each airport and lag (this requires some time)

precip_fcs_models <- vector("list", 20)
for (i in seq_len(20)) {
 df <- precip_fcs$training[[i]]
 idr_fit <- fit_idr(df)
 hclr_fit <- fit_hclr(df)
 hclr_noscale_fit <- fit_hclr_noscale(df)
 precip_fcs_models[[i]] <- precip_fcs[i, ]
 precip_fcs_models[[i]]$idr <- list(idr_fit)
 precip_fcs_models[[i]]$hclr <- list(hclr_fit)
 precip_fcs_models[[i]]$hclr_noscale <- list(hclr_noscale_fit)
}

save(list = "precip_fcs_models", file = "precip_fcs_models.rda")