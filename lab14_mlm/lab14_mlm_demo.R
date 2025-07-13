# Lab 14 Demo
# 2025-04-23


# Housekeeping ------------------------------------------------------------


# Reset session with SHIFT+COMMAND+0 or CTRL+SHIFT+F10
# See shortcuts with OPT+SHIFT+K or ALT+SHIFT+K

# You will often see this at the top of a script:
rm(list = ls())
# Not a bad thing to do, but better to restart completely

# Packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  lme4,
  lmerTest,
  stringr,
  performance,
  ggplot2
)

# Scientific notation
options(scipen = 999)



# Multilevel Models -------------------------------------------------------


# Pull clean dataset from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/lab14_nass_data.rds'
con <- gzcon(url(github_url, 'rb'))
nass <- readRDS(con)
close(con)

str(nass)
# no-till: Total acres in no-till conservation practice
# tourism: Total income from Ag tourism and recreation
# income: Total income from farm-related activities in $ per operation

# Plot it
nass %>%
  ggplot(aes(x = no_till, y = income, color = state)) +
  # geom_point(alpha = 0.6) +
  geom_smooth(aes(group = state), method = "lm", se = FALSE, fullrange = TRUE) +
  coord_cartesian(xlim = c(0, 100000), ylim = c(0, 200000)) +
  theme_classic() +
  theme(legend.position = 'None')



## Context -----------------------------------------------------------------


# FE/RE remove unobserved effects, entity specific characteristics
# FE: model within effect only, control for between
# RE: include random intercept, but interpret as blend of within and between
# MLM: explicitly model BOTH the within effects AND between effects
#   Does the slope vary by group?

# Assumption of OLS: IID Residuals (independent and identical)
#   OLS is naive to groupings, which affects DoF, SEs, and p-values
#   Ecological fallacy - apply group level results to individual level
#   Atomistic fallacy - apply individual level results to group level



## Model Progression -------------------------------------------------------
### Null Model --------------------------------------------------------------


str(nass)

## Syntax
# 1 means intercept
# (random effect | group)
# (1 | state) means each state has a random intercept
# Good info on this here:
#   https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#linear-mixed-models


## Null model, intercept only
null <- lme4::lmer(income ~ 1 + (1 | state), data = nass)

# Check intra class correlation
performance::icc(null)
# 22% of variation is at state level, 78% at county level
# Adjusted: only random effect variances
# Unadjusted: includes fixed effect variances (but there are none)



### Add Level 1 Predictor ---------------------------------------------------


## Add predictor to level 1
lmer1 <- lme4::lmer(income ~ no_till + (1 | state), data = nass)

# Check variances
apply(X = nass, FUN = var, MARGIN = 2)

# Rescale dataset to have mean 0 and variance 1
str(nass)
df <- nass %>%
  mutate(across(where(is.numeric), scale))
str(df)
# Note that how we center affects interpretation
# Now in terms of standard deviations
# Grand mean vs group mean

# Try again
lmer1 <- lme4::lmer(income ~ no_till + (1 | state), data = df)
summary(lmer1)
# Notice there are no p-values. Great resource on this and much more with GLMMs:
#   https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# Other packages can give us p-values though:
lmer1 <- lmerTest::lmer(income ~ no_till + (1 | state), data = df)


## Interpret:
summary(lmer1)
# Fixed effects
#   Intercept is now average income level across states
#   1 SD change in no_till associated with 0.15 SD change in income
# Random effects
#   State variance: how states vary around mean of farm income
#   Residual variance: how counties vary around mean within state

# Check proportion of variance accounted for by groups
performance::icc(lmer1)
# 2.25% of variance in income explained by state

# Compare null model to no_till model (rerun null with df)
null <- lme4::lmer(income ~ 1 + (1 | state), data = df)
anova(null, lmer1)
# Significant group effect of state

# Check R2
performance::r2(lmer1)
# Marginal: Fixed effects only
# Conditional: Both fixed and random effects



# Add Random Slope --------------------------------------------------------


## Add no_till as random slope
lmer2 <- lmerTest::lmer(income ~ no_till + (no_till | state), data = df)

# Compare models
anova(lmer1, lmer2)
# Significant random effect of no_till

summary(lmer2)
# Random Effects:
#   State variance: how states vary around mean of income
#   no_till variance: how no_till slopes vary around mean slope
#   Residual variance: how counties vary around their state mean of income
#   Correlation: how intercept of no_till relates to slope of no_till
# Level 2 intercept: average value of income
# Variance: how much states vary around average income

performance::r2(lmer2)
performance::check_model(lmer2)



# Limits and Extensions ---------------------------------------------------


# The NASS example might be better off as a spatial regression

# Limitations:
#   Need large sample size
#   Convergence problems
#   Scaling
#   Worth mentioning spatial regression

# If you don't have a multi-level question, don't use a multi-level model
#   Fixed effects models
#   Cluster robust standard errors
#   Generalized estimating equations

# Extensions
#   Well suited for repeated measure data
#   MRPS: Multi-level regression with post-stratification



# Resources ---------------------------------------------------------------


# Utah State Office of Research Services - Multilevel modelsd
#   https://cehs-research.github.io/eBook_multilevel/

# Introduction to multi-level modeling: https://www.learn-mlms.com/

# Quantitude podcasts:
#   https://quantitudepod.org/s2e29-multilevel-models-the-often-unnecessary-green-monster/
#   https://quantitudepod.org/s5e05-multilevel-models/

# GLMM FAQ: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

