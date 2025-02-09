# Lab 5 Demo: Dummy Variables
# 2025-02-14



# Housekeeping ------------------------------------------------------------


# Load packages with pacman (install pacman if necessary)
# install.packages('pacman')
pacman::p_load(
  fastDummies
)

# Load lab5 survey.
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab4_survey.rds'
df <- readRDS(gzcon(url(github_url, 'rb')))
close(con)

# Explore lab5 survey.
str(df)
# Note that this is a 'dirtier' version of what we have been working with. There
# are no binary or dummy variables in it.


