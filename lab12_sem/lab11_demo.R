# Lab 11 Demo


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  plm,
  wooldridge,
  AER,
  ivreg,
  conflicted
)

# Check conflicting function names
conflicted::conflict_scout()
# Both AER and ivreg packages have an ivreg function

# Conflicted package won't let you use an ambiguous function
# filter(iris, Species == 'setosa')

# Set winners for those conflicts
conflicts_prefer(
  ivreg::ivreg(),
  dplyr::filter()
)

options(scipen = 999)



# Using lists -------------------------------------------------------------


# Lists can help manage multiple objects like models in your environment.
# Use same data from last week as an example:
data(crime4, package = 'wooldridge')



## Manual ------------------------------------------------------------------


# So far, we have been assigning every model object manually
pooling <- plm::plm(
  crmrte ~ prbarr,
  data = crime4,
  model = 'pooling'
)

within <- plm::plm(
  crmrte ~ prbarr,
  data = crime4,
  model = 'within'
)

random <- plm::plm(
  crmrte ~ prbarr,
  data = crime4,
  model = 'random'
)
# We now have 3 objects to manage. This works, but does not scale well.



## List --------------------------------------------------------------------


# Lists make this easier. Instantiate an empty list:
models <- list()

# Save model to an element in the list with $, just like with a df
models$pooling <- plm::plm(
  crmrte ~ prbarr,
  data = crime4,
  model = 'pooling'
)

models$within <- plm::plm(
  crmrte ~ prbarr,
  data = crime4,
  model = 'within'
)

# We can also do it with double brackets
models[['random']] <- plm::plm(
  crmrte ~ prbarr,
  data = crime4,
  model = 'random'
)

# Check out the list
str(models, max.level = 1)
str(models)
names(models)

# Get a summary of a specific model by name
summary(models$pooling)
summary(models[['within']])

# or by index
summary(models[[3]])

# Get a summary of all three models at once
lapply(models, summary)

# This is better, but still includes a lot of copy and pasting



## Make a Function ---------------------------------------------------------


# Make our own function to run plm
run_plm <- function(x) {
  plm::plm(
    crmrte ~ prbarr,
    data = crime4,
    model = x
  )
}

# Run one type of model
within <- run_plm('within')
summary(within)

# Get a vector of the model names
model_types <- c('pooling', 'within', 'random')
print(model_types)

# Run all 3 models at once
models <- lapply(model_types, run_plm)

# Get a summary of all three models at once
lapply(models, summary)

# Get residuals from all three models
res <- lapply(models, \(x) x$residuals)
str(res)


# Before moving on, remove objects from environment. Usually better to restart R
# though, because that will re-run rprofile, renviron, project settings, etc.
rm(list = ls())



# IVs and 2SLS ------------------------------------------------------------


# Examples from Hanck et al. (2024) Introduction to Econometrics, which are
# based on examples from Wooldridge
# https://www.econometrics-with-r.org/12-ivr.html



## Load Data ---------------------------------------------------------------


## Check out dataset
data("CigarettesSW", package = 'AER')
str(CigarettesSW)
?CigarettesSW
# packs: packs per capita
# price: average price of cigarettes, including sales tax
# tax: average state, federal, and local excise tax
# taxs: average excise taxes, including sales tax


## Data wrangling
# Add variables:
#   rprice is real (cpi adjusted) price of cigarettes
#   salestax is sales tax
#   rincome is real (cpi adjusted) income per capita
#   cigtax is real cigarette-specific taxes
# Then filter to only the year 1995
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

# Use salestax as an instrument for rprice. Salestax should be related to real
# price of cigarettes, but not to the number of packs consumed per capita

# First stage regression - regress endogenous var onto instrument
# Reduced form equation - endogenous vars in terms of exogenous
stage_1 <- lm(log(rprice) ~ salestax, data = df)
summary(stage_1)

# Save predicted (fitted) values
str(stage_1)
y2_hat <- stage_1$fitted.values
print(y2_hat)

# Stage 2 regression - regress dependent var on predicted values
stage_2 <- lm(log(df$packs) ~ y2_hat)
summary(stage_2)

# Animation:
# https://nickch-k.github.io/EconometricsSlides/Week_08/Week_08_Instrumental_Variables.html#8

# Model diagram



## Assumptions -------------------------------------------------------------


## Check for weak instruments
summary(lm(log(rprice) ~ salestax, data = df))
# Null is no correlation between exogenous and endogenous


## Check exogeneity (Wu Hausman)
wu_hausman <- lm(log(packs) ~ log(rprice) + stage_1$residuals, data = df)
summary(wu_hausman)
# Null is exogeneity


## Sargan test
# Null is exogeneity
# Only use when q > 1



## IVReg -------------------------------------------------------------------


# 2SLS using ivreg::ivreg
tsls <- ivreg::ivreg(log(packs) ~ log(rprice) | salestax, data = df)
summary(tsls)
# Same test results as above

# Compare our manual 2-stage model to ivreg
summary(stage_2)
summary(tsls)
# What is the same, what is different?

# Compare to OLS estimation
summary(ols)
summary(tsls)



## Multivariate ------------------------------------------------------------


# Add rincome (exogenous) and cigtax (instrument).
# Now we have 1 more instrument than we need (q = 2)
# Note that ALL exogenous variables go after the | (not just instruments)
tsls2 <- ivreg::ivreg(
  log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax + cigtax,
  data = df
)
summary(tsls2)
# Sargan test: null is that instruments are exogenous (when q >= 2)


# Compare to ols
ols2 <- lm(log(packs) ~ log(rprice) + log(rincome), data = df)
summary(ols2)
summary(tsls2)

