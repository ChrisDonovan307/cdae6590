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
    lagged_ratio = 'F', # F is a reserved namespace, best to avoid it
    years = A
  )
str(df)



# Assignment --------------------------------------------------------------


# 1. Run a plain OLS regression for demand:
#   Q (food consumption) is dependent variable
#   P (food price ratio) is endogenous independent variable
#   D (disposable income) is exogenous independent variable
# (Being exogenous or endogenous doesn't matter until the next part - this is
# not supposed to be a trick question. Just do regular OLS)


# 2a. Run the same demand model as 2SLS regression with ivreg::ivreg(), but add:
#   F (ratio of last yr farm prices to consumer prices) as exogenous instrument
#   A (years) as exogenous instrument


# 2b. What do the results of the three diagnostic tests suggest?


# 3. Are the results of the 2SLS regression different than OLS? If so, how, and
# why does it matter?
