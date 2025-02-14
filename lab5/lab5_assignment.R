# Lab 5 Assignment


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

# Use any method you like to explore your df if you wish:



# Recode Variables --------------------------------------------------------


## Recode where_live
# First check out the where_live variable
class(df$where_live)
table(df$where_live)

# 1. Recode where_live into a dummy variable for each category. You can do this
# in several ways, but the easiest will be fastDummies::dummy_cols()


# 2. Now, recode where_live again, but instead of making multiple dummy
# variables, turn it into a single categorical variable with three possible
# options: 1 means the person lives in Vermont, 2 means the person lives in
# any other US state, and 3 means the person lives outside of the US.


## Recode own
# Great! We're not actually using the where_live variable in our regression
# though. Let's check out the own variable instead:
class(df$own)
table(df$own)
# Remember that 'res' means they live in the residence halls on campus. There
# are only 2 people in this category, so it might make sense to lump them with
# the renters.

# 3. Recode the 'own' variable as a binary dummy variable and call it
# own_binary. A 1 should be people who own their home, and a 0 should be people
# who either rent or live in the residence halls.



# Regression --------------------------------------------------------------


# 4. Run the given regression. Remember that live_years is how many years a
# person has lived in their current residence. If you hit an error, make sure
# that you recoded the 'own' variable and called it 'own_binary' (or change
# own_binary in the formula to whatever you named your variable).
lm1 <- lm(live_years ~ income + age * own_binary, data = df)
summary(lm1)

# Interpret the important results from the model. For the interaction term,
# explain in real words what it means, and why it might be significant. For
# example, why might the effect of age on live_years (the number of years they
# have lived in their current residence) be different for people who own a home
# than people who rent?

