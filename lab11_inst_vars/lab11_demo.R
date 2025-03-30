# Lab 11 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  plm,
  wooldridge
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


