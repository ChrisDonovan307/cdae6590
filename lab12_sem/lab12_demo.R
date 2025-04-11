# Lab 12 Demo


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  systemfit,
  wooldridge,
  ivreg,
  lavaan
)

data('mroz', package = 'wooldridge')
?mroz

# Filter to working in labor force
mroz <- filter(mroz, inlf == 1)
str(mroz)

# Wooldridge example 16.3
# Equation 1: hours ~ lwage + educ + age + kidslt6 + nwifeinc
# Equation 2: lwage ~ hours + educ + exper + exper^2



# Eq 1: Hours -------------------------------------------------------------


# OLS
hours_ols <- lm(hours ~ lwage + educ + age + kidslt6 + nwifeinc, mroz)
summary(hours_ols)

# Regression for hours using 2SLS estimation
hours_2sls <- ivreg::ivreg(
  hours ~ lwage + educ + age + kidslt6 + nwifeinc |
    . - lwage + exper + expersq,
  data = mroz
)
summary(hours_2sls)



# Eq 2: Wages -------------------------------------------------------------


# Wage equation using OLS
wages_ols <- lm(lwage ~ hours + educ + exper + expersq, mroz)
summary(wages_ols)

# Wage equation with 2SLS
wages_2sls <- ivreg(lwage ~ hours + educ + exper + expersq |
                  ~ . - hours + age + kidslt6 + nwifeinc,
                data = mroz)
summary(wages_2sls, diagnostics = TRUE)



# Simultaneous ------------------------------------------------------------


# Save both formulas as a named list
system <- list(
  hoursEq = hours ~ lwage + educ + age + kidslt6 + nwifeinc,
  wagesEq = lwage ~ hours + educ + exper + expersq
)

# Save the formula for the instruments (starting with ~)
instruments <- ~ educ + age + kidslt6 + nwifeinc + exper + expersq

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
# Reduced form equation
wages_reduced <- lm(
  lwage ~ educ + age + kidslt6 + nwifeinc + exper + expersq,
  mroz
)

# And again without instrumental variables
wages_no_exo <- lm(lwage ~ educ + age + kidslt6 + nwifeinc, mroz)
summary(wages_no_exo)

# Another way to do it with update()
wages_no_exo <- update(wages_reduced, ~ . - exper - expersq)
summary(wages_no_exo)

# F test
anova(wages_reduced, wages_no_exo)

# Another way to do the the same thing:
linearHypothesis(wages_reduced, c("exper = 0", "expersq = 0"))


## Test for Rank Condition of hours
# Reduced form equation for hours
hours_reduced <- lm(hours ~ educ + age + kidslt6 + nwifeinc + exper + expersq, mroz)
hours_no_exo <- lm(hours ~ educ + exper + expersq, mroz)
anova(hours_reduced, hours_no_exo)



# Path Analysis -----------------------------------------------------------


# Path analysis is a system of regressions estimated simultaneously
# It is half of a structural equation model (SEM)
# The other half is the measurement portion for latent variables

# Define structural model
model <-'lwage ~ educ + exper + unem
         exper ~ age
         educ ~ motheduc + fatheduc + age'

# Fit model with sem
fit <- sem(model, data = mroz)
summary(fit, standardized = TRUE, fit.measures = TRUE)

# Extract fit info and residuals
inspect(fit, 'R2')
fitMeasures(fit)
lavResiduals(fit)

# Plot it
lavaanPlot(
  model = fit,
  node_options = list(shape = "box", fontname = "Helvetica"),
  edge_options = list(color = "grey"),
  coefs = FALSE
)

# Calculate indirect effects
model <-'lwage ~ b*educ + exper + unem
         exper ~ age
         educ ~ a*motheduc + fatheduc + age
         motheduc_educ := a
         educ_wage := b
         indirect := a*b'
fit <- sem(model, data = mroz)
summary(fit, standardized = TRUE, fit.measures = TRUE)

# Check modification indices
modificationindices(fit)
