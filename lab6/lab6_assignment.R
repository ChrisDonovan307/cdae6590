# Lab 6 Assignment


# Housekeeping ------------------------------------------------------------


# Load lab6 df from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/life_exp.rds'
con <- gzcon(url(github_url, 'rb'))
df <- readRDS(con)
close(con)

# This dataset comes from Our World in Data:
# https://ourworldindata.org/life-expectancy



# Assignment --------------------------------------------------------------


# 1. Run a regression with life_exp as the dependent variable and population
# and year as independent variables. Use residuals plots and formal tests to
# determine whether the residuals are heteroskedastic.


# 2. Use each of the following methods to adjust or account for
# heteroskedasticity. For parts b and c, check to see whether the new model
# is heteroskedastic or homoskedastic, and explain how you know.
#   a. Robust standard errors
#   b. Transformation of the dependent variable
#   c. Weighted least squares, using the inverse of population as weights
