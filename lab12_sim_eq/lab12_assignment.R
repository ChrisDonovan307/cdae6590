# Lab 12 Assignment


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  systemfit,
  ivreg
)

data(Kmenta, package = 'systemfit')
?systemfit::Kmenta
str(Kmenta)



# Assignment --------------------------------------------------------------


#' Demand equation: consump as a function of price and income
#' Supply equation: consump as a function of price, farmPrice, and trend
#' Exogenous variables: income, farmPrice, trend
#' Endogenous variables: price


# 1. Solve simultaneous questions independently using ivreg(). There should be
# one model for demand and one model for supply.


# 2. Recreate the same result using the systemfit() function and interpret the
# results.
