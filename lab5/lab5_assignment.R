# Lab 5 Assignment
# 2025-02-14


# Housekeeping ------------------------------------------------------------


# Load packages
# install.packages('pacman')
pacman::p_load(
  fastDummies,
  dplyr
)

# Load lab5 survey
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab5_survey.rds'
con <- gzcon(url(github_url, 'rb'))
df <- readRDS(con)
close(con)

# Use any method you like to explore your df:



# Recode Dummies ----------------------------------------------------------


# Recode where_live variable as dummies
# First check out what it looks like
class(df$where_live)
unique(df$where_live)
table(df$where_live)

# Recode into three categories: Vermont, Any US state other than Vermont,
# and outside of US

# Run a regression with gender dummy and where_live dummies
# interpret?



# Test --------------------------------------------------------------------


get_str(df)
get_str(df$where_live)
get_table(df$where_live)
df$gender_female <- ifelse(df$gender == 'Female', 1, 0)
df$where_grouped <- dplyr::case_when(
  df$where_live == 'Vermont' ~ 'VT',
  df$where_live == 'Outside of US' ~ 'Outside US',
  .default = 'Other US state'
) %>%
  as.factor()

# Regression
lm1 <- lm(income ~ gender_female * where_grouped, data = df)
summary(lm1)
