# Lab 12 Demo


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  systemfit,
  wooldridge,
  ivreg,
  lavaan,
  lavaanPlot
)

data('mroz', package = 'wooldridge')
?mroz

# Filter to women in labor force
mroz <- filter(mroz, inlf == 1)
str(mroz)

# Wooldridge example 16.3
# Equation 1: hours ~ lwage(endo) + educ + age + kidslt6 + nwifeinc
# Equation 2: lwage ~ hours(endo) + educ + exper + exper^2



# Eq 1: Hours -------------------------------------------------------------


# OLS
hours_ols <- lm(hours ~ lwage + educ + age + kidslt6 + nwifeinc, mroz)
summary(hours_ols)

# Regression for hours using 2SLS estimation
# '.' notation: take everything on left side, remove lwage, add exper, expersq
hours_2sls <- ivreg::ivreg(
  hours ~ lwage + educ + age + kidslt6 + nwifeinc | . - lwage + exper + expersq,
  data = mroz
)
summary(hours_2sls)



# Eq 2: Wages -------------------------------------------------------------


# Wage equation using OLS
wages_ols <- lm(lwage ~ hours + educ + exper + expersq, mroz)
summary(wages_ols)

# Wage equation with 2SLS
wages_2sls <- ivreg(
  lwage ~ hours + educ + exper + expersq |
    ~ . - hours + age + kidslt6 + nwifeinc,
  data = mroz
)
summary(wages_2sls, diagnostics = TRUE)



# Simultaneous ------------------------------------------------------------


# Save both formulas as a named list
system <- list(
  hoursEq = hours ~ lwage + educ + age + kidslt6 + nwifeinc,
  wagesEq = lwage ~ hours + educ + exper + expersq
)
print(system)

# Save the formula for the instruments (starting with ~)
instruments <- ~educ + age + kidslt6 + nwifeinc + exper + expersq
print(instruments)

# Solve simultaneous equations
sim <- systemfit(
  formula = system,
  inst = instruments,
  method = '2SLS',
  data = mroz
)
summary(sim)

# Compare to separate models
summary(hours_2sls)
summary(wages_2sls)
summary(sim)



# Rank Condition ----------------------------------------------------------


## Test for Rank Condition of wages
# Reduced form equation - endogenous vars as function of exogenous vars
wages_reduced <- lm(
  lwage ~ educ + age + kidslt6 + nwifeinc + exper + expersq,
  mroz
)

# And again without instrumental variables
wages_no_exo <- lm(lwage ~ educ + age + kidslt6 + nwifeinc, mroz)

# Another way to do it with update()
wages_no_exo <- update(wages_reduced, ~ . - exper - expersq)

# F test - do instrumental variables matter (different than 0)
anova(wages_reduced, wages_no_exo)

# Another way to do the the same thing:
linearHypothesis(wages_reduced, c("exper = 0", "expersq = 0"))


## Test for Rank Condition of hours
# Reduced form equation for hours
hours_reduced <- lm(
  hours ~ educ + age + kidslt6 + nwifeinc + exper + expersq,
  mroz
)

# Same but remove instrumental vars
hours_no_exo <- lm(hours ~ educ + exper + expersq, mroz)

# F test for instrumental vars
anova(hours_reduced, hours_no_exo)

# Same F test another way
linearHypothesis(
  hours_reduced,
  c(
    "age = 0",
    "kidslt6 = 0",
    "nwifeinc = 0"
  )
)



# Path Analysis -----------------------------------------------------------


# Path analysis is a system of regressions estimated simultaneously.
# It is half of a structural equation model (SEM)
# The other half is the measurement portion for latent variables

## Regression model:
reg_model <-'lwage ~ educ + exper + unem + motheduc + fatheduc + age'
reg_fit <- sem(reg_model, data = mroz)
lavaanPlot(model = reg_fit)
summary(reg_fit)


## Path analysis:
# Define structural model
model <-'lwage ~ educ + exper + unem
         exper ~ age
         educ ~ motheduc + fatheduc + age'

# Fit model with sem
fit <- sem(model, data = mroz)

# Plot it
lavaanPlot(model = fit)

# Get output of path analysis
summary(fit, standardized = TRUE, fit.measures = TRUE)

# Extract fit info and residuals
inspect(fit, 'R2')
fitMeasures(fit)
lavResiduals(fit)

# Plot with values and colors
e_opts <- formatting(
  list(color = "orange"), # Regressions in orange
  list(color = NULL),     # For latent vars (we have none)
  list(color = "blue"),   # Covariances in blue
  type = "edge"
)
lavaanPlot2(
  fit,
  include = "covs",
  graph_options = list(rankdir = 'TB'),
  node_options = list(fontname = "Helvetica"),
  edge_options = e_opts,
  coef_labels = TRUE
)


## Calculate indirect effects
model <-'lwage ~ b*educ + exper + unem
         exper ~ age
         educ ~ a*motheduc + fatheduc + age

         # Declare indirect effects
         motheduc_educ := a
         educ_wage := b
         indirect := a*b'
fit <- sem(model, data = mroz)
summary(fit, standardized = TRUE, fit.measures = TRUE)


## Set some covariances to 0 and see what happens
# First check modification indices (but be careful with this)
modificationindices(fit, standardized = TRUE, sort. = TRUE)

# Define model syntax, setting unem ~~ fatheduc and unem ~~ motheduc to 0
model <-'lwage ~ b*educ + exper + unem
         exper ~ age
         educ ~ a*motheduc + fatheduc + age

         # Remove covariances
         unem ~~ 0*fatheduc
         unem ~~ 0*motheduc

         # Declare indirect effects
         motheduc_educ := a
         educ_wage := b
         indirect := a*b'
fit <- sem(model, data = mroz)
summary(fit, standardized = TRUE, fit.measures = TRUE)
# What happened to fit?

# Check modification indices again
modificationindices(fit, standardized = TRUE, sort. = TRUE)

