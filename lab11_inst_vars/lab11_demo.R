# Lab 11 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  plm,
  wooldridge,
  AER
)

options(scipen = 999)



# Using lists -------------------------------------------------------------


data(crime4, package = 'wooldridge')
crime <- crime4
# Do same wrangling as before



## Manual ------------------------------------------------------------------


pooling <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'pooling'
)

within <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'within'
)

random <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'random'
)
# This works, but does not scale well



## List --------------------------------------------------------------------


# Initiate an empty list
models <- list()

# Save model to an element in the list with $
models$pooling <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'pooling'
)

models$within <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'within'
)

# We can also do it with double brackets
models[['random']] <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'random'
)

# Check out the list
str(models)
names(models)

# Get a summary of a specific model by name
summary(models$pooling)
summary(models[['within']])

# or by index
summary(models[[3]])

# This is better, but still includes a lot of copy and pasting



## Make a Function ---------------------------------------------------------


# Make a function to run plm
run_plm <- function(x) {
  plm::plm(
    crmrte ~ prbarr,
    data = crime,
    model = x
  )
}

# Run one type of model
summary(run_plm('within'))

# Run all three models
model_types <- c('pooling', 'within', 'random')
models <- lapply(model_types, run_plm)

# Get a summary of all three models at once
lapply(models, summary)



# IVs and 2SLS ------------------------------------------------------------


# Examples from Hanck et al. (2024) Introduction to Econometrics
# https://www.econometrics-with-r.org/12-ivr.html



## Load Data ---------------------------------------------------------------


## Check out dataset
data("CigarettesSW", package = 'AER')
str(CigarettesSW)
# packs: packs per capita
# price: average price including sales tax
# tax: average state, federal, and average local excise tax
# taxs: average excise tax, including sales tax


## Data wrangling
# Get real per capita prices, sales tax, real income, and cigarette tax
# Then subset to 1995
df <- CigarettesSW %>%
  mutate(
    rprice = price / cpi,
    salestax = (taxs - tax) / cpi,
    rincome = income / population / cpi,
    cigtax = tax / cpi
  ) %>%
  filter(year == '1995')



## OLS ---------------------------------------------------------------------


# Exploring relationship between real per capita prices on cigarettes and the
# number of packs purchased per person
ols <- lm(log(packs) ~ log(rprice), data = df)
summary(ols)
# Log log model - elasticity

# What is the problem here?



## Manual ------------------------------------------------------------------


## 2SLS the manual way
# First stage regression - regress endogenous var onto instrument
# Reduced form equation
stage_1 <- lm(log(rprice) ~ salestax, data = df)
summary(stage_1)

# Save predicted (fitted) values
str(stage_1)
y2_hat <- stage_1$fitted.values

# Stage 2 regression - regress dependent var on predicted values
stage_2 <- lm(log(df$packs) ~ y2_hat)
summary(stage_2)



## Assumptions -------------------------------------------------------------


## Check weak instruments
summary(lm(log(rprice) ~ salestax, data = c1995))
# Null is no correlation between exogenous and endogenous


## Check exogeneity (Wu Hausman)
wu_hausman <- lm(log(packs) ~ log(rprice) + stage_1$residuals, data = df)
lht(wu_hausman, c("stage_1$residuals = 0"))
# Null is exogeneity





## IVReg -------------------------------------------------------------------


# 2SLS using ivreg::ivreg
tsls <- ivreg::ivreg(log(packs) ~ log(rprice) | salestax, data = df)
summary(tsls)
# Coefficient is same here as with stage_2
# Standard errors are not the same (does not account for error in first model)
# Note Sargan test (overidentification)

# Compare to ols
summary(ols)



## Multivariate ------------------------------------------------------------


# Add rincome (exogenous) and cigtax (instrument)
# Note that ALL exogenous variables go after the |, even if they are not
# instruments. This is why rincome is repeated
tsls2 <- ivreg(
  log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax + cigtax,
  data = df
)
summary(tsls2)
# sargan test: null is that instruments are endogenous (when q >= 2)


