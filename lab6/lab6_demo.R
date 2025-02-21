# Lab 6 Demo



# Lab 5 Assignment Recap --------------------------------------------------


pacman::p_load(
  fastDummies,
  dplyr
)

github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/lab5_survey.rds'
con <- gzcon(url(github_url, 'rb'))
lab5_df <- readRDS(con)
close(con)

str(lab5_df)
lab5_df$own_binary <- ifelse(lab5_df$own == 'own', 1, 0)

lab5_lm <- lm(live_years ~ income + age * own_binary, data = lab5_df)
summary(lab5_lm)

# CTRL/CMD + SHIFT + F10 to restart



# Housekeeping ------------------------------------------------------------


# Explore available datasets:
data()
data(iris)
str(iris)

pacman::p_load(
  AER,
  lmtest,
  sandwich,
  dplyr,
  ggplot2
)

# Packages often come with more datasets. Applied Econometrics in R (AER) comes
# with a heap of datasets. Use data()
data()
data(CPSSW8)
str(CPSSW8)
?CPSSW8

# But we will work with a modified version I have modified and downsampled
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/CPSSW8_mod.rds'
con <- gzcon(url(github_url, 'rb'))
df <- readRDS(con)
close(con)

# Check out the dataset
str(df)



# Simple Example ----------------------------------------------------------


# Show earnings ~ education with regression line
df %>%
  ggplot(aes(x = education, y = earnings)) +
  geom_jitter(
    size = 2,
    alpha = 0.5,
    width = 0.25
  ) +
  theme_classic() +
  geom_smooth(
    method = 'lm',
    se = FALSE,
    lwd = 2
  ) +
  labs(
    x = 'Years of Education',
    y = 'Hourly Wages',
    title = 'Hourly Wages as a Function of Education'
  )



# Working Example ---------------------------------------------------------


lm <- lm(earnings ~ education + age, data = df)
summary(lm)

# Check residuals with base plots
par(mfrow = c(2, 2))
plot(lm)
par(mfrow = c(1, 1))

# Check with performance package
performance::check_model(lm)



# Tests for Heteroskedasticity --------------------------------------------


# Breusch-Pagan test - Use either formula or model object
lmtest::bptest(earnings ~ education + age, data = df)
lmtest::bptest(lm)
# Test whether residuals are a function of fitted values

# White test - same thing but add squares and cross products
lmtest::bptest(earnings ~ education * age + I(education^2) + I(age^2), data = df)
# More predictors than BP test
# Test that all of the betas are 0 except intercept



# Fixing Heteroskedasticity -----------------------------------------------
## Robust SEs --------------------------------------------------------------


# vcovHC gives heteroskedasticity-consistent variance-covariance matrix
(vcov <- sandwich::vcovHC(lm, type = 'HC'))
# Off-diagonals are covariances.

# To get robust SEs, we want to sqrt of diagonals
sqrt(diag(vcov))

# Or get a df of coefficients with robust SEs automatically.
lmtest::coeftest(lm, vcov. = vcov)

# And a robust version of the F-test
waldtest(lm, vcov = vcov)

# Compare them all
summary(lm)
lmtest::coeftest(lm, vcov. = vcov)
waldtest(lm, vcov = vcov)



## Transformations ---------------------------------------------------------


# What is the distribution of earnings variable?
hist(df$earnings)

# Try log transforming. We could also try square root, cube root, box cox, etc.
df$earnings_log <- log(df$earnings)
hist(df$earnings_log)

# Test for normality
shapiro.test(df$earnings_log)
# Note that these tests are considered overpowered

# Try running another lm with earnings_log (either way works)
lm2 <- lm(log(earnings) ~ education + age, data = df)
lm2 <- lm(earnings_log ~ education + age, data = df)
summary(lm2)
# 1 unit change in education leads to 9.7% change in earnings

# Check residuals
performance::check_model(lm2)
# What a beaut

# Test new lm for heteroskedasticity
lmtest::bptest(lm2)
lmtest::bptest(
  earnings_log ~ education * age + I(education^2) + I(age^2),
  data = df
)

# Warning: non-linear transformations change relationships between variables
cor(df$earnings, df$education)
cor(df$earnings_log, df$education)



# Extras ------------------------------------------------------------------
## WLS ---------------------------------------------------------------------


# Lots of ways to do this - can weight by population, variance, theory

# Here, we weight by the inverse of our population weights (how many people each
# person represents in the population based on region)
str(df)
lm_wls <- lm(earnings ~ education + age, data = df, weights = 1/weights)
summary(lm_wls)

# Test for heteroskedasticity again
lmtest::bptest(lm_wls)

# Check residuals of weighted model
performance::check_model(lm_wls)



## FGLS --------------------------------------------------------------------


# Steps:
# 1. run regression of y on xs and get residuals u_hat
# 2. get log(u_hat^2)
# 3. run regression log(u_hat^2) ~ xs, get fitted values g_hat
# 4. h_hat = exp(g_hat)
# 5. run wls with weights = 1/h_hat

# Residuals (u_hat) from initial model
u_hat <- lm(earnings ~ education + age, data = df)$residuals

# Next regression using log(u_hat^2), pull out fitted values
g_hat <- lm(log(u_hat^2) ~ education + age, data = df)$fitted.values

# Exponentiate to get h_hat
h_hat <- exp(g_hat)


## Function to calculate h_hat (same process as above)
get_h_hat <- function(df, formula) {
  u_hat <- lm(formula = formula, data = df)$residuals
  rhs <- paste(all.vars(formula)[-1], collapse = ' + ')
  log_u_hat_2 <- log(u_hat^2)
  new_formula <- as.formula(paste("log_u_hat_2 ~", rhs))
  g_hat <- lm(formula = new_formula, data = df)$fitted.values
  h_hat <- exp(g_hat)
  return(h_hat)
}

# Check output of function against the manual process above
h_hat_out <- get_h_hat(df, earnings ~ education + age)
identical(h_hat, h_hat_out)
# Should be same output

# Now run weighted regression with our h_hat as weights
lm_fgls <- lm(earnings ~ education + age, data = df, weights = 1/h_hat)
summary(lm_fgls)
lmtest::bptest(lm_fgls)
performance::check_model(lm_fgls)

# Compare to original lm
summary(lm)
performance::check_model(lm)
# Our FGLS model is not passing bptest, but residuals do look better than the lm

