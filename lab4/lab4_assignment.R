# Lab 4 Assignment

# Load Packages
pacman::p_load(
  dplyr,
  broom,
  skimr
)

# Load lab 4 survey file
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab4_survey.rds'
download.file(github_url, 'lab4_survey.rds', method = 'auto')
df <- readRDS('lab4_survey.rds')


# Explore your df using View, str, skim, or whichever other way you like:


# Run any multiple regression that you like:


# 1. Turn your model output into a data frame of coefficients using
# broom::tidy():


# 2. Calculate confidence intervals around your betas using the confint()
# function:


# 3. Combine the confidence intervals into your model output with bind_rows():


# 4. Interpret the confidence intervals and p value of any one predictor:

