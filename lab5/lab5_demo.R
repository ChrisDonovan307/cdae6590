# Lab 5 Demo: Dummy Variables
# 2025-02-14



# Housekeeping ------------------------------------------------------------


# Load packages with pacman (install pacman if necessary)
# install.packages('pacman')
pacman::p_load(
  dplyr,
  fastDummies,
  ggplot2
)

# Load lab5 survey
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab5_survey.rds'
con <- gzcon(url(github_url, 'rb'))
df <- readRDS(con)
close(con)

# Explore lab5 survey.
str(df)
# Note that this is a 'dirtier' version of what we have been working with. There
# are no binary or dummy variables in it.



# Recoding Variables ------------------------------------------------------
## Recode Gender ----------------------------------------------------------


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



## Recode Employ ----------------------------------------------------------


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
test_df <- fastDummies::dummy_cols(test_df, select_columns = 'employ')
str(test_df)
# Same thing! Note that column names are hinky though

# Check documentation for function
?dummy_cols()
# Why remove_first_dummy?



## Interactions -----------------------------------------------------------


# Adding an interaction with the * term
lm3 <- lm(income ~ age + gender_female * employ_full, data = df)
summary(lm3)

# Make another model without the interaction
lm4 <- lm(income ~ age + gender_female + employ_full, data = df)
summary(lm4)

# Compare the two models
anova(lm3, lm4)
# Was the interaction important?

# What about interactions between categorical variables?
lm5 <- lm(income ~ gender_female * own, data = df)
summary(lm5)
# How to interpret each interaction term?



# Grouping Categorical Variables ------------------------------------------


# Here we will compare education as:
#   categorical with all 6 options
#   categorical with 3 options (NoneHS, Assoc_bach, Master_PhD)
#   categorical with 2 options

# Check unique responses for education
unique(df$educ)

# Create categorical version of education with 'None of the above' as reference
df$educ_categorical <- factor(df$educ)
df$educ_categorical <- relevel(df$educ_categorical, ref = 'None of the above')

## Create a grouped version where 0 = none/high school, 1 = bachelor, 2 = higher
# degree. First try it with ifelse statements. Note that I do not recommend
# doing it like this!
df$educ_grouped_hard <- ifelse(
  df$educ %in% c('None of the above', 'High school degree'),
  'None_HS',
  df$educ
)
df$educ_grouped_hard <- ifelse(
  df$educ_grouped_hard %in% c('Associates degree', "Bachelor's degree"),
  'Assoc_Bach',
  df$educ_grouped_hard
)
df$educ_grouped_hard <- ifelse(
  df$educ_grouped_hard %in% c("Master's degree", "Doctoral degree"),
  'Master_PhD',
  df$educ_grouped_hard
)
get_table(df$educ_grouped_hard)

# A much better way is with dplyr::case_when
df$educ_grouped <- dplyr::case_when(
  df$educ %in% c('None of the above', 'High school degree') ~ 'None_HS',
  df$educ %in% c('Associates degree', "Bachelor's degree") ~ 'Assoc_Bach',
  df$educ %in% c("Master's degree", "Doctoral degree") ~ 'Master_PhD',
  .default = NA
)

# Make educ_grouped a factor and set the reference group to None_HS
df$educ_grouped <- factor(df$educ_grouped)
df$educ_grouped <- relevel(df$educ_grouped, ref = 'None_HS')
get_table(df$educ_grouped)
str(df)


## Create binary for whether or not they have a bachelor's degree
df$educ_binary <- ifelse(
  df$educ %in% c("Doctoral degree", "Master's degree", "Bachelor's degree"),
  'Bach_Master_PhD',
  'None_HS'
)

# Make educ_binary a factor and set the reference level to None_HS
df$educ_binary <- factor(df$educ_binary)
df$educ_binary <- relevel(df$educ_binary, ref = 'None_HS')
str(df)



## Regressions -----------------------------------------------------------


# Run a model with each version of the education variable that we coded
lm_categorical <- lm(income ~ educ_categorical, data = df)
lm_grouped <- lm(income ~ educ_grouped, data = df)
lm_binary <- lm(income ~ educ_binary, data = df)

# Check summary outputs, also save them as objects
(sum_categorical <- summary(lm_categorical))
(sum_grouped <- summary(lm_grouped))
(sum_binary <- summary(lm_binary))
# What changes when we do this?



## Extra - Graph Adj R2 ---------------------------------------------------


# Make a df for our adj R2
graph_df <- data.frame(
  model = factor(
    c('Categorical', 'Grouped', 'Binary'),
    levels = c('Categorical', 'Grouped', 'Binary')),
  adjusted_R2 = c(
    sum_categorical$r.squared,
    sum_grouped$r.squared,
    sum_binary$r.squared
  )
)

# Graph adj R2
ggplot(graph_df, aes(x = model, y = adjusted_R2)) +
  geom_col(fill = 'grey', color = 'black') +
  labs(
    x = 'Model',
    y = 'Adjusted R2',
    title = 'Adjusted R2 under different groupings of education'
  ) +
  theme_classic()

