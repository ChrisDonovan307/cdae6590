# Lab 3 Demo
# Multiple Regression
# 2025-01-31


# Housekeeping ------------------------------------------------------------


# skimr has the skim function, which is a summary of a df. Highly recommend!
# install.packages('skimr')
library(skimr)

# Optionally turn off scientific notation - can make outputs easier to read
options(scipen = 999)



# Load Data ---------------------------------------------------------------


# Load survey .rds file from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab3_survey.rds'
download.file(github_url, 'lab3_survey.rds', method = 'auto')
df <- readRDS('lab3_survey.rds')

# View the data frame
View(df)

# Check the structure of the data
str(df)

# Observe a single column
df$month_rent

# Check overall summary of data with skimr package
skim(df)
# Pay attention to classes and missing data!



# One Predictor Regression ------------------------------------------------


## Let's explore the relationship between income and education (numeric)
lm1 <- lm(income ~ educ_num, data = df)
summary(lm1)
# What does this mean?
#  Estimate (Beta)
#  Std. Error
#  t value
#  Pr(>|t|)
#  R^2


## Remember that we are assuming causation causation and its direction. What
# happens if we reverse the regression? Now we look at age as a function of
# income.
lm2 <- lm(educ_num ~ income, data = df)
summary(lm2)
# What do we see?

# Causality is assumed by the researcher, not implied by the model



# Multiple Regression -----------------------------------------------------


## Let's add age to our regression model
lm_multi <- lm(income ~ educ_num + age, data = df)
summary(lm1)
summary(lm_multi)
# How does this compare to our one-predictor regression?
# Residual standard error?
# Degrees of freedom?
# R2 and Adjusted R2
# F test degrees of freedom


## We can use an F test to compare two models. Let's compare our last multi
# predictor model to our single predictor model from earlier
anova(lm1, lm_multi)
# Are the models significantly different?


## Let's add another variable - the number of letters in the first name
lm_multi2 <- lm(income ~ educ_num + age + letters, data = df)
summary(lm_multi)
summary(lm_multi2)
# How does this compare to our 2 predictor regression?



# Residual Plots ----------------------------------------------------------


## View four residual plots one by one
plot(lm_multi)

## View all four residual plots at once
par(mfrow = c(2, 2))
plot(lm_multi)
par(mfrow = c(1, 1))

# 1. Residuals vs Fitted
#   Check for linearity of relationship
# 2. QQ Residuals
#   Distribution of residuals. If normal, they fall along the line
# 3. Scale Location
#   Like residuals vs fitted, but with root of standardized residuals
#   Check for homoscedasticity
# 4. Residuals vs Leverage
#   Outliers with respect to independent variables. Not necessarily influential
#   Cook's distance - changes to model when observation is removed from it


## Other great tools
install.packages('performance', dependencies = TRUE)
library(performance)
check_model(lm_multi)

install.packages('DHARMa')
library(DHARMa)
DHARMa::testSimulatedResiduals(lm_multi)



# Extra - Checking Assumptions --------------------------------------------


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
library(DescTools)



## Linearity ------------------------------------------------------------


# Plot each variable against income to observe relationship
plot(df$age, df$income)
plot(df$educ_num, df$income)

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
# A horizontal red line suggests homoskedastic errors

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


