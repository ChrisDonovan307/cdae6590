# Lab 4 Demo
# Hypothesis testing, T, and F tests
# 2025-02-07


# Load Packages -----------------------------------------------------------


# The pacman package (package manager) is really convenient.
install.packages('pacman')
pacman::p_load(dplyr, skimr)
# Perks:
#   Both installs and loads each package at the same time
#   Can do it for multiple packages at once
#   Do not need quotes (although quotes are fine)
# Note the use of the double colon operator ::



# Lab 3 Recap -------------------------------------------------------------


# Load lab 3 survey file from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab3_survey.rds'
download.file(github_url, 'lab3_survey.rds', method = 'auto')
lab3_df <- readRDS('lab3_survey.rds')


## R^2 and adding predictors
# Run the same regressions as before, adding letters in first name
lm1 <- lm(income ~ educ_num + age, data = lab3_df)
lm2 <- lm(income ~ educ_num + age + letters, data = lab3_df)
summary(lm1)
summary(lm2)

# Instead of printing summary results, save to new objects
sum1 <- summary(lm1)
sum2 <- summary(lm2)

# Check structure of a model summary object
str(sum1)

# Use extraction operator to pull out r.squared term
sum1$r.squared
sum2$r.squared


## Categorical variables and curious results
lm1 <- lm(income ~ educ, data = lab3_df)
summary(lm1)
# What is going on with the education variable?
# T tests vs F test?

# Summary stats with dplyr using group_by and summarize workflow with pipes
lab3_df %>%
  group_by(educ) %>%
  summarize(mean_income = mean(income, na.rm = TRUE))

# Coding error in income! Also room for bias
table(lab3_df$educ)
table(lab3_df$income, useNA = 'always')



# Lab 4 -------------------------------------------------------------------


# Load lab 4 survey file
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab4_survey.rds'
download.file(github_url, 'lab4_survey.rds', method = 'auto')
df <- readRDS('lab4_survey.rds')

# Note that this one is smaller and dirtier
str(df)

# Hypothesis testing, p value dive, t and f tests...
