# Lab 11 Assignment


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  ivreg
)

data('Kmenta', package = 'ivreg')
str(Kmenta)
?Kmenta

# (Optional) Make the names more descriptive
df <- Kmenta %>%
  rename(
    food_cons = Q,
    food_price_ratio = P,
    disp_income = D,
    lagged_ratio = F, # F is a reserved namespace. Best to avoid it in general
    years = A
  )
str(df)




# Assignment --------------------------------------------------------------


# 1. Run a plain OLS regression for demand:
#   Q (food consumption) is dependent variable
#   P (food price ratio) is an independent variable (endogenous)
#   D (disposable income) is an independent variable (exogenous)
# Just regular OLS here - exogenous/endogenous doesn't matter yet


# 2a. Run the same demand model as 2SLS regression with ivreg::ivreg(), but add:
#   F (ratio of last yr farm prices to consumer prices) as exogenous instrument
#   A (years) as exogenous instrument
# These two variables will be instruments for P (food price ratio). Recall the
# syntax requirements for exogenous regressors, or check ?ivreg and go to the
# details section for a refresher


# 2b. What do the results of the three diagnostic tests suggest?


# 3. Are the results of the 2SLS regression different than OLS? If so, how, and
# why does it matter?
