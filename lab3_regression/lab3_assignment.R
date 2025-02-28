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


# 1. Interpret the results of your model. Pick out the important variables and
#   values as you see fit to get a concise summary of the findings. This might
#   include R2, p-values, test statistics - whatever tells the story of your
#   model.


# 2. Use either base plots or the performance::model_check() function to
#   create residual plots. Does your model meet assumptions of:
#     i. normally distributed residuals?
#     ii. uncorrelated (homoskedastic) residuals?
#     iii. is there anything else that is noteworthy about your residual plots?


# 3. Either add another variable to your regression OR take one away. Save the
#   new model to a new object. What happened to the variance and R^2 of your
#   model compared to the original model?


# 4. Use an anova() to determine whether your new model is significantly
#   different from your old model. Interpret the results. Is one model better
#   than the other? If so, which?

