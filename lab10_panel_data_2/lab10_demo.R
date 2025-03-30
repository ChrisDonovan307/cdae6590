# Lab 10 - Panel Data 2


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  AER,
  plm,
  tidyr,
  ggplot2,
  wooldridge
)

options(scipen = 999)

data(crime4, package = "wooldridge")

# Examples from:
# https://scpoecon.github.io/ScPoEconometrics/panel-data.html
# https://nickch-k.github.io/EconometricsSlides/Week_06/Week_06_1_Within_Variation_and_Fixed_Effects.html#8

# Data from Wooldridge package, open source companion to
# Introductory Econometrics: A Modern Approach



# Explore Data ------------------------------------------------------------


# Check out variables
str(crime4)

# This is similar to the Crime dataset
?Crime
# crmrte: crimes committed per person
# prbarr: probability of being arrested for a crime

# Reduce to a few counties for clarity
crime = crime4 %>%
  filter(county %in% c(1, 3, 145, 23), prbarr < 0.5)
str(crime)

# Plot points by county
crime %>%
  ggplot(aes(x = prbarr, y = crmrte, color = factor(county))) +
    geom_point(size = 3) +
    theme_classic() +
    labs(
      x = 'Probability of Arrest',
      y = 'Crime Rate',
      color = 'County'
    )



# Pooling -----------------------------------------------------------------


# Pooling across years - plain OLS
pooling <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'pooling'
)
summary(pooling)

# Plot regression line onto pooled points
crime %>%
  ggplot(aes(x = prbarr, y = crmrte)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() +
    labs(x = 'Probability of Arrest', y = 'Crime Rate')



# Fixed Effects -----------------------------------------------------------


# Fixed effects model (within)
within <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'within'
)
summary(within)

# 'Within' effects in graph
crime %>%
  ggplot(aes(x = prbarr, y = crmrte, group = county, color = factor(county))) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() +
    labs(
      x = 'Probability of Arrest',
      y = 'Crime Rate',
      color = 'County'
    )
# Neat animation:
# https://nickch-k.github.io/EconometricsSlides/Week_06/Week_06_1_Within_Variation_and_Fixed_Effects.html#12


## Compare to pooled model to see whether fixed effects are appropriate
plm::pFtest(within, pooling)
# plm::pooltest(pooling, within)
# null: no individual effects


## Test whether there is serial correlation of idiosyncratic errors
# Help decide whether to use FD or FE
pbgtest(crmrte ~ prbarr, data = crime)
# null: no serial correlation present
# If serial correlation is present, first differencing is better



# First Difference --------------------------------------------------------


# First difference model to compare to fixed effects
difference <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'fd'
)
summary(difference)



# Random Effects ----------------------------------------------------------


# RE model - assumes alpha_i uncorrelated with Xs
# Allows for explanatory variables that are constant over time
# Includes composite error term not accounted for in OLS
# Uses feasible GLS to estimate theta_hat, between 0 (pooled) and 1 (FE)
random <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'random'
)
summary(random)
# Idiosyncratic effect - across individuals and time
# Individual effect - within county
# Note value of theta


## Hausman test - compare fixed effects to random
phtest(within, random)
# Null: x_it uncorrelated with alpha_i
# If significant, reject, there are unobserved effects, use FE
# If not significant, fail to reject, assume no unobserved effects, use RE

# Helpful diagram on selecting between FE and RE
# https://en.wikipedia.org/wiki/Durbin%E2%80%93Wu%E2%80%93Hausman_test
