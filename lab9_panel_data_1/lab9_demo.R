# Lab 8 Demo
# Limited Dependent Variables


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  AER,
  dplyr,
  plm,
  tidyr
)

options(scipen = 999)



# Tidy Data ---------------------------------------------------------------


# Load some example data
data(world_bank_pop, package = 'tidyr')
str(world_bank_pop)
head(world_bank_pop)
# This is wide - multiple values per row

# Pivot longer so each row has one piece of information
long <- tidyr::pivot_longer(
  world_bank_pop,
  cols = starts_with('2'),
  names_to = 'year',
  values_to = 'value'
)
str(long)
head(long, 10)

# Pivot it back
wide <- tidyr::pivot_wider(
  long,
  id_cols = c('country', 'indicator'),
  names_from = 'year',
  values_from = 'value'
)
str(wide)
head(wide)



# Prep Dataset ------------------------------------------------------------


# Example from Introduction to Econometrics in R
# https://www.econometrics-with-r.org/index.html

# Load data from AER package
data(Fatalities, package = 'AER')

# Check out data frame
str(Fatalities)
head(Fatalities, 15)[, 1:10]

# Make it a panel data object
Fatalities <- pdata.frame(Fatalities)
pdim(Fatalities)
str(Fatalities)
head(Fatalities, 15)[, 1:10]

# Create a fatality rate variable
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000



# Regressions -------------------------------------------------------------
## Pooled  -----------------------------------------------------------------


# No entity specific effects, same relationship across all years and states
# Cannot account for shocks

# Plain old lm
summary(lm(fatal_rate ~ beertax, data = Fatalities))

summary(plm(
  fatal_rate ~ beertax,
  data = Fatalities,
  model = 'pooling'
))




## First Differenced -------------------------------------------------------


# Difference out the state characteristics (alpha_i)
# Controls for unobserved, time-invariant effects

# Split data into two datasets, one for 82 and one for 88
Fatalities1982 <- Fatalities[Fatalities$year == '1982', ]
Fatalities1988 <- Fatalities[Fatalities$year == '1988', ]

# Get difference in fatality rate and beer tax for each year
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# Estimate a regression using differenced data
model_diff <- lm(diff_fatal_rate ~ diff_beertax)
summary(model_diff)
# Raising beer tax by $1 drops fatalities by 1.04 per 10,000
# Within state effect


## Now with plm
# Filter data to the same two years
Fatalities_82_88 <- Fatalities[Fatalities$year %in% c('1982', '1988'), ]
model_fd <- plm(
  fatal_rate ~ beertax,
  data = Fatalities_82_88,
  index = c('state', 'year'),
  model = 'fd'
)
summary(model_fd)



## Fixed Effects -----------------------------------------------------------


# Allows for individual differences (different intercepts)
# remove alpha_i by de meaning or including dummies
# Different intercepts for each state
# Controls for unobserved, time invariant effects

model_fe <- plm(
  fatal_rate ~ beertax,
  data = Fatalities,
  index = c('state', 'year'),
  model = 'within' # for fixed effects
)
summary(model_fe)


test <- plm(
  fatal_rate ~ beertax + state,
  data = Fatalities,
  index = c('state', 'year'),
  model = 'within' # for fixed effects
)
summary(test)



