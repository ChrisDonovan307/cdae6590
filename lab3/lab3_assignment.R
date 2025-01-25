# Lab 3 Assignment
# 2025-01-25


# Description -------------------------------------------------------------



# Load Data ---------------------------------------------------------------


# Load survey .rds file from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/clean_survey.rds'
download.file(github_url, 'clean_survey.rds', method = 'curl')
df <- readRDS('clean_survey.rds')

# Use str(), View(), names(), or whatever you like to explore the dataset and
# get acquainted with the column names



# Multiple Regression -----------------------------------------------------


# Run a multiple regression on the dataset using any variables you like as the
# independent and dependent variables.
# Remember that the syntax is: lm(y ~ x + z, data = df), and that you will have
# to assign the model to an object of your choosing.


# Use summary() to see the results of your regression.


# 1. Interpret the estimate and p-value for any one of your predictor variables


# 2. Interpret the F test and p-value for the model


# 3. Either add another variable to your regression or take one away.
#   What happened to the variance of your model?
#   What happened to the R^2 and adjusted R^2 of your model?

# 4. Use an anova() to determine whether your new model is significantly
# different from your old model. Interpret the results.
