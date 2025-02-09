# Lab 5 Demo: Dummy Variables
# 2025-02-14



# Housekeeping ------------------------------------------------------------


# Load packages with pacman (install pacman if necessary)
# install.packages('pacman')
pacman::p_load(
  fastDummies
)

# Load lab5 survey.
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab5_survey.rds'
con <- gzcon(url(github_url, 'rb'))
df <- readRDS(con)
close(con)

# Explore lab5 survey.
str(df)
# Note that this is a 'dirtier' version of what we have been working with. There
# are no binary or dummy variables in it.



# Recode Gender -----------------------------------------------------------


# First let's just see how the gender variable looks
class(df$gender)
table(df$gender)
# This is a character (categorical) variable with two outcomes

# Make a binary dummy variable for female. 1 is female, 0 is not female. We make
# a new variable by assigning values (<-) to a new column name using the extract
# ($) operator. We do this with the ifelse() function. The == is a comparison
# operator. Note that it is different than a single =.
?ifelse
?'=='
df$gender_dummy <- ifelse(df$gender == 'Female', 1, 0)

# Explore our new variable. What will the class be?
class(df$gender_dummy)
table(df$gender_dummy)
# Same as our female/male table


## Regressions
# Let's see what the regression looks like both ways. First gender as character
lm1 <- lm(income ~ gender + age, data = df)
summary(lm1)

# Now with gender dummy
lm1 <- lm(income ~ gender_dummy + age, data = df)
summary(lm1)
# Note that R codes as binary automatically!



# Recode Race -------------------------------------------------------------


# Again, check how the uncoded race variable looks
class(df$race)
table(df$race)
