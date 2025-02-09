# Lab 5 Demo: Dummy Variables
# 2025-02-14



# Housekeeping ------------------------------------------------------------


# Load packages with pacman (install pacman if necessary)
# install.packages('pacman')
pacman::p_load(
  dplyr,
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
df$gender_female <- ifelse(df$gender == 'Female', 1, 0)

# Explore our new variable. What will the class be?
class(df$gender_female)
table(df$gender_female)
# Same as our female/male table


## Regressions
# Let's see what the regression looks like both ways. First gender as character
lm1 <- lm(income ~ gender + age, data = df)
summary(lm1)

# Now with gender dummy
lm2 <- lm(income ~ gender_female + age, data = df)
summary(lm2)
# How to interpret betas for continuous and dummy variables together?
# Note that R codes binary categorical variables as dummies automatically



# Recode Employ -----------------------------------------------------------


# Again, check how the uncoded employ variable looks
class(df$employ)
unique(df$employ)
table(df$employ)
# Note that this is a 'pick all that apply', so some responses are combinations

# Save our data with a new name

# Make dummy for full time employment
df$employ_full <- ifelse(df$employ == 'full time', 1, 0)
table(df$employ_full)
# 77 people employed full time, 26 not

# Make dummies for other employment categories
df$employ_retired <- ifelse(df$employ == 'retired', 1, 0)
df$employ_student <- ifelse(df$employ == 'student', 1, 0)
df$employ_not <- ifelse(df$employ == 'not employed', 1, 0)
df$employ_part_student <- ifelse(df$employ == 'part time,student', 1, 0)
df$employ_part <- ifelse(df$employ == 'part time', 1, 0)
df$employ_full_student <- ifelse(df$employ == 'full time,student', 1, 0)
str(df)


## A better way to recode variables
# If you find anything tedious in R, there is probably a better way to do it.
# Here we can use fastDummies package to make our variables for us.
# First let's remove the columns we just created (anything with employ_)
test_df <- df[, !grepl('employ_', names(df))]
str(test_df)

# Create columns with fastDummies
test_df <- dummy_cols(test_df, select_columns = 'employ')
get_str(test_df)
# Same thing! Note that column names are hinky though

# Check documentation for function
?dummy_cols()
# Why remove_first_dummy?



# Interactions ------------------------------------------------------------


# Adding an interaction with the * term
lm3 <- lm(income ~ age + gender_female * employ_full, data = df)
summary(lm3)



# Ordinal Variables -------------------------------------------------------


# Here we will compare education as a numeric, categorical, and binary

# Create a numeric version of education from 0 to 5
df$educ_numeric <- case_when(
  df$educ == 'None of the above' ~ 0,
  df$educ == 'High school degree' ~ 1,
  df$educ == 'Associates degree' ~ 2,
  df$educ == "Bachelor's degree" ~ 3,
  df$educ == "Master's degree" ~ 4,
  df$educ == "Doctoral degree" ~ 5
)
str(df)

# Create categorical version of education with 'None of the above' as reference
df$educ_categorical <- factor(df$educ)
df$educ_categorical <- relevel(df$educ_categorical, ref = 'None of the above')

# Create binary for whether or not they have a bachelor's degree
df$educ_binary <- ifelse(
  df$educ %in% c("Doctoral degree", "Master's degree", "Bachelor's degree"),
  1,
  0
)
str(df)

# Compare regressions against income
lm_numeric <- lm(income ~ educ_numeric, data = df)
lm_categorical <- lm(income ~ educ_categorical, data = df)
lm_binary <- lm(income ~ educ_binary, data = df)
summary(lm_numeric)
summary(lm_categorical)
summary(lm_binary)
# What happens as we move from numeric to categorical to binary?
