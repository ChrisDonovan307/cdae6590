# Lab 4 Demo
# Hypothesis testing, T, and F tests
# 2025-02-07


# Load Packages -----------------------------------------------------------


# The pacman package (package manager) is really convenient.
# install.packages('pacman')
pacman::p_load(
  dplyr,
  skimr,
  ggplot2,
  broom
)
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
# R^2 did go up, just by such a small amount that it was rounded off!


## Categorical variables and curious results
lm1 <- lm(income ~ educ, data = lab3_df)
summary(lm1)
# What is going on with the education variable?
# T tests vs F test?

# Summary stats with dplyr using group_by and summarize workflow with pipes
lab3_df %>%
  group_by(educ) %>%
  summarize(mean_income = mean(income, na.rm = TRUE))

# I made a coding error in income! It is fixed in lab 4 survey and on
# Note that the way we coded education could also leave room for bias. We did
# not include a trade school degree.
table(lab3_df$educ)
table(lab3_df$income, useNA = 'always')



# Lab 4 -------------------------------------------------------------------


# Load lab 4 survey file
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab4_survey.rds'
download.file(github_url, 'lab4_survey.rds', method = 'auto')
df <- readRDS('lab4_survey.rds')

str(df)

# Multiple regression
lm1 <- lm(income ~ educ_num + age, data = df)
summary(lm1)
# Given the data, what are the chances that education is significantly
# associated with income?


## Confidence intervals
# First get coefficients out of summary
coefs_df <- tidy(lm1)
coefs_df

# Set some variables for degrees of freedom, alpha (type 1 error), and
# our critical t value based on student t distribution. We will use these to
# calculate confidence intervals.
dof <- 99
alpha <- 0.05
critical_t <- qt(1 - alpha/2, dof)

# Calculate CIs for educ_num manually
lower_bound <- coefs_df[2, 'estimate'] - critical_t * coefs_df[2, 'std.error']
upper_bound <- coefs_df[2, 2] + critical_t * coefs_df[2, 3]
print(lower_bound)
print(upper_bound)

# Calculate CIs automatically
cis <- as.data.frame(confint(lm1))
print(cis)
# What does this mean?


## Combine confidence intervals with betas
# First get coefficients into a data frame
(coefs <- broom::tidy(lm1))

# Combine coefs df with cis df by binding columns together
# Check dimensions of each df:
dim(coefs)
dim(cis)
# Both have 3 rows. Coefs has 5 columns, cis has 2 columns
# So result will have 3 rows, 7 columns

# Showing this two ways - first with base function, second with dplyr function
# Either way, we just smush the columns together.
clean_df <- cbind(coefs, cis)
clean_df <- dplyr::bind_cols(coefs, cis)
clean_df

# Check dimensions
dim(clean_df)
# 3 rows, 7 columns

# Rename columns to get rid of weird characters. First argument is the data
# frame. Then it goes new name = old name. So we are changing the '2.5 %' to
# 'ci.low'
clean_df <- dplyr::rename(
  clean_df,
  ci.low = `2.5 %`,
  ci.high = `97.5 %`
)
clean_df
# This is a nice clean data frame ready to put into a table for a paper



# Extras ------------------------------------------------------------------


## Making a scatterplot
ggplot(df, aes(x = educ_num, y = income)) +
  geom_point()

# Add a clean theme and make points bigger
ggplot(df, aes(x = educ_num, y = income)) +
  geom_point(size = 3) +
  theme_classic()

# Points are stacked up - use a jitter and alpha to show them better
ggplot(df, aes(x = educ_num, y = income)) +
  geom_jitter(
    size = 3,
    alpha = 0.5,
    width = 0.1,
    height = 5000
  ) +
  theme_classic()

# Add labels
ggplot(df, aes(x = educ_num, y = income)) +
  geom_jitter(
    size = 3,
    alpha = 0.4,
    width = 0.1,
    height = 5000
  ) +
  theme_classic() +
  labs(
    x = 'Education',
    y = 'Income',
    title = 'Scatter plot of income by education'
  )


## Plotting confidence intervals
# First, remove intercept, because we don't care about it
plot_df <- filter(clean_df, term != '(Intercept)')

# Plot using ggplot2 package and the geom_errorbar function
ggplot(plot_df, aes(x = term, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.2) +
  theme_classic() +
  labs(
    title = "Confidence Intervals for 'income ~ educ_num + age'",
    x = "Predictors",
    y = "Estimates"
  )
