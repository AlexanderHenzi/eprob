#-------------------------------------------------------------------------------
# Packages
library(tidyverse)

#-------------------------------------------------------------------------------
# Data (available on https://github.com/AlexanderHenzi/isodistrreg)
load("precipData_caseStudy.rda")

#-------------------------------------------------------------------------------
# Reformat data
precip_fcs <- precipData_caseStudy %>%
 mutate(
  lag = as.integer(as.character(horizon)),
  airport = fct_recode(
   airport,
   "Brussels" = "bru",
   "London" = "lhr",
   "Frankfurt" = "fra",
   "Zurich" = "zrh"
  )
 ) %>%
 select(-horizon) %>%
 select(airport, lag, date, obs, hres, ctr, starts_with("p")) %>%
 as.data.frame()

# Some summaries
precip_fcs %>%
 group_by(airport, lag) %>%
 summarise(
  n = n(),
  min_date = min(date),
  max_date = max(date)
 )

# Save as .rda
save(list = "precip_fcs", file = "precip_fcs.rda")