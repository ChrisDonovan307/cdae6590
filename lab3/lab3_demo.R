# Lab 3 Demo
# 2025-01-25



# Load Packages -----------------------------------------------------------


# skimr has the skim function, which is a summary of a df
install.packages('skimr')
library(skimr)



# Load Data ---------------------------------------------------------------


# Load survey .rds file from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/clean_survey.rds'
download.file(github_url, 'clean_survey.rds', method = 'curl')
df <- readRDS('clean_survey.rds')

# Check the structure of the data
str(df)

# View the data frame
View(df)

# Check overall summary of data with skimr package
skim(df)
# Pay attention to classes and missing data!



# One Predictor Regression ------------------------------------------------


# Let's explore the relationship between monthly rent
lm1 <- lm(income ~ age, data = df)
summary(lm1)
# What does this mean?
# Estimate (Beta)
# Std. Error
# t value
# Pr(>|t|): a brief detour on p-values
# R^2

# Note - if you don't want scientific notation, you can turn it off:
options(scipen = 999)
summary(lm1)

# Remember that we are assuming causation - it is not implied by the model
# What happens if we reverse regression?
lm2 <- lm(age ~ income, data = df)
summary(lm2)
# Same t, p, R^2, and F values

# Calculate betas manually
(var_both <- var(df$age, df$income, use = 'pairwise.complete.obs'))
(var_age <- var(df$age[!is.na(df$income)]))
(var_income <- var(df$income, use = 'complete.obs'))

# Beta for income ~ age:
var_both / var_age

# Beta for age ~ income:
var_both / var_income

# Causality is assumed by the researcher, not implied by the model



# Multiple Regression -----------------------------------------------------


# Let's add another variables to our regression
lm_multi <- lm(income ~ age + educ_num, data = df)
# How does this compare to our one-predictor regression?

# Let's add another variable - live_years
lm_multi2 <- lm(income ~ age + live_years + educ_num, data = df)
summary(lm_multi2)
# How does this compare to our 2 predictor regression?

# We can use an F test to compare two models. Let's compare our last multi
# predictor model to our single predictor model from earlier
anova(lm1, lm_multi2)
# There is no significant difference between the models



# Checking Assumptions ----------------------------------------------------


# Some packages that will help us check assumptions:
install.packages('ggplot2')    # graphing
install.packages('GGally')     # more graphing
install.packages('dplyr')      # data wrangling
install.packages('lmtest')     # BP test
install.packages('DescTools')  # Variance inflation factors
library(ggplot2)
library(dplyr)
library(GGally)
library(lmtest)
library(VIF)



## Linearity ------------------------------------------------------------


# Plot each variable against income to observe relationship
plot(df$age, df$income)
plot(df$educ_num, df$income)
plot(df$employ_full, df$income)

# A better way to graph:
ggplot2::ggplot(df, aes(x = educ_num, y = income)) +
  geom_jitter(width = 0.05, height = 2500, alpha = 0.4, size = 2) +
  geom_smooth(method = 'lm')

# To check many at once, try the GGally package.
# First, reduce our df to the relevant columns we want
# The two methods below are identical. First is base R, second is dplyr package
small_df <- df[c('income', 'age', 'educ_num')]
small_df <- dplyr::select(df, income, age, educ_num)
str(small_df)

# Now we can make a pairs plot
GGally::ggpairs(small_df)

# Linear relationships are a good sign, but remember that the assumption is for
# linearity in the parameters



## Uncorrelated Residuals --------------------------------------------------


# We can use the scale-location plot to explore residuals.
par(mfrow = c(2, 2))
plot(lm_multi)
par(mfrow = c(1, 1))
# A horizontal red line suggests homoscedastic errors

# We can also use the Breusch Pagan test, which we will get into later!
lmtest::bptest(lm_multi)



## Normality of Residuals --------------------------------------------------


# You might see people check marginal normality:
hist(df$income)
shapiro.test(df$income)
# But the assumption of normality is in the residuals!

# So let's check our residuals.
# We can use the QQ residual plot to explore normality of residuals
par(mfrow = c(2, 2))
plot(lm_multi)
par(mfrow = c(1, 1))
# Normal residuals will fall along the straight line

# We can use a histogram to look at residuals
hist(resid(lm_multi), breaks = 10)

# We can also use the shapiro test on model residuals
shapiro.test(resid(lm_multi))



## No Multicollinearity -------------------------------------------------


# Explore bivariate correlations of all of our variables:
cor(small_df, use = 'pairwise.complete.obs')

# But checking multicollinearity is harder
# We can use VIF - variance inflation factor, a measure of how much variance
# is overestimated because of collinearity
DescTools::VIF(lm_multi)
# 5 or greater is considered problematic

