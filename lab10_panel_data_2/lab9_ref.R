# Lab 9 Demo
# Panel Data 1


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  AER,
  plm,    # linear models for panel data
  tidyr   # reshaping data frames
)

options(scipen = 999)

# Reminders
# 1. Midterm things, syntax, responses
# 2. Class topic for 4.25



# Tidy Data ---------------------------------------------------------------


# 1. Each variable is a column, each column is a variable
# 2. Each observation is a row, each row is an observation
# 3. Each value is a cell, each cell is a single value
# More at https://tidyr.tidyverse.org/articles/tidy-data.html#defining


## Load some example data
data(world_bank_pop, package = 'tidyr')
str(world_bank_pop)

# Check first few rows
head(world_bank_pop, 10)[, 1:6]


## Pivot longer to group the year columns
long <- tidyr::pivot_longer(
  world_bank_pop,
  # cols = '2000':'2017',
  cols = starts_with('2'),
  names_to = 'year',
  values_to = 'value'
)
str(long)
head(long, 10)


## Pivot wider so each column is a variable
wide <- tidyr::pivot_wider(
  long,
  id_cols = c('country', 'year'),
  names_from = 'indicator',
  values_from = 'value'
)
str(wide)
head(wide)
# This is tidy data



# Prep Dataset ------------------------------------------------------------


# Example from Introduction to Econometrics in R
# https://www.econometrics-with-r.org/index.html

# Load data from AER package
data(Fatalities, package = 'AER')

# Check out data frame
str(Fatalities)
head(Fatalities, 10)[, 1:10]
# Is it tidy?

# Make it a panel data object
Fatalities <- plm::pdata.frame(Fatalities)
str(Fatalities)
head(Fatalities, 15)[, 1:10]

# pdim is like dim, but gives panel data info
dim(Fatalities)
plm::pdim(Fatalities)

# Create a fatality rate variable
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000



# Regressions -------------------------------------------------------------
## Pooled  -----------------------------------------------------------------


# No entity specific effects, same relationship across all years and states
summary(lm(fatal_rate ~ beertax, data = Fatalities))

# We can add year as a factor as well
summary(lm(fatal_rate ~ beertax + year, data = Fatalities))

# We can add interactions to allow for different slopes
summary(lm(fatal_rate ~ beertax * year, data = Fatalities))


## Now with plm package
model_pooled <- plm::plm(
  fatal_rate ~ beertax,
  data = Fatalities,
  index = c('state', 'year'), # If null, assumes first two columns
  model = 'pooling'
)
summary(model_pooled)


## Chow test
plm::pooltest(fatal_rate ~ beertax, data = Fatalities, model = 'pooling')
# REvisit chow test



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
# Different than pooled, which ignores state-specific effects


## Now with plm
# Filter data to the same two years
Fatalities_82_88 <- Fatalities[Fatalities$year %in% c('1982', '1988'), ]

# Run equivalent model
model_fd <- plm(
  fatal_rate ~ beertax,
  data = Fatalities_82_88,
  index = c('state', 'year'),
  model = 'fd'
)
summary(model_fd)

model_fd <- plm(
  fatal_rate ~ beertax - 1, # remove intercept
  data = Fatalities_82_88,
  index = c('state', 'year'),
  model = 'fd'
)
summary(model_fd)



## Fixed Effects -----------------------------------------------------------


# Allows for individual differences (different intercepts)
# Controls for unobserved, time invariant effects
# remove alpha_i by de meaning rather than differencing

model_fe <- plm(
  fatal_rate ~ beertax,
  data = Fatalities,
  index = c('state', 'year'),
  model = 'within' # for fixed effects
)
summary(model_fe)


summary(lm(fatal_rate ~ beertax + year - 1, data = Fatalities))

model_fe <- plm(
  fatal_rate ~ beertax,
  data = Fatalities,
  index = c('state', 'year'),
  model = 'within'
)
summary(model_fe)

