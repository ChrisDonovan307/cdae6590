# Lab 10 Assignment


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  plm,
  wooldridge
)

options(scipen = 999)

# Dataset with birth weights, infant mortality, other variables across states
data(lowbrth, package = 'wooldridge')

# Check it out
str(lowbrth)

?lowbrth
# Some interesting variables:
#   lowbrth: percentage of births with low weight
#   pcinc: per capita income
#   physic: physicians per capita

# Turn this into a plm panel data object
lowbrth <- plm::pdata.frame(lowbrth, index = c('state', 'year'))
str(lowbrth)



# Assignment --------------------------------------------------------------


# 1. Run a pooled OLS on our the lowbrth dataset with lowbrth as the dependent
# variable and any predictor that you think would be appropriate. Briefly
# interpret the coefficients of your predictor.


# 2. Run a fixed effect model using the same formula. Provide a brief
# interpretation. Then test whether the pooled model or the fixed effect model
# is more appropriate.


# 3. Run a random effect model using the same formula. Then test whether the
# fixed effect model of the random effect model is more appropriate.

