# Lab 3 Assignment


# Load Data ---------------------------------------------------------------


# Load survey .rds file from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab3_survey.rds'
download.file(github_url, 'lab3_survey.rds', method = 'curl')
df <- readRDS('lab3_survey.rds')

# Use str(), View(), names(), or whatever you like to explore the dataset and
# get acquainted with the column names



# Multiple Regression -----------------------------------------------------


# Run a multiple regression on the dataset using any variables you like as the
# independent and dependent variables.

# Use summary() to see the results of your regression.


# 1. Interpret the estimate and p-value for any one of your predictor variables


# 2. Interpret the R^2 and adjusted R^2 for your model


# 3. Use either base plots of the performance::model_check() function to
#   create residual plots. Does your model meet assumptions of:
#     i. linear relationship?
#     ii. normally distributed residuals?
#     iii. uncorrelated (homoskedastic) residuals?

# 4. Either add another variable to your regression or take one away. Save the
#   new model to a new object. What happened to the variance and R^2 of your
#   model?

# 5. Use an anova() to determine whether your new model is significantly
# different from your old model. Interpret the results - which model is better?
