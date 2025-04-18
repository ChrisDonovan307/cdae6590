# Final Assignment


# Load Data ---------------------------------------------------------------


github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/nass_panel.rds'
con <- gzcon(url(github_url, 'rb'))
df <- readRDS(con)
close(con)

str(df)
#' Variables:
#'  easements: conservation easements, acres / operation
#'  cons: conservation tillage, excluding no till, acres / operation
#'  no_till: conservation tillage, no till, acres / operation
#'  cover: cover crops planted, excluding CRP, acres / operation
#'  income: farm related income, $ / operation



# Assignment --------------------------------------------------------------


# Imagine you are studying association between conservation practices and income
# by state over three years (2012, 2017, 2022). You are running a regression
# with the formula:
#   income ~ easements + cons + no_till + cover
# A colleague ran a fixed effects model and found that cover crops are
# significantly and positively associated with income per farm.


# a. With your responses from #2 as a guide, use statistical tests to show
# whether this is an appropriate model for the data, and explain why or why not.
# If not, which model is more appropriate?


# b. Interpret the important results from the most appropriate model using
# robust statistics for coefficients.


# Remember to load any packages you might need into the script.
