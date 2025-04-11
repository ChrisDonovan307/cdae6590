# Lab 12 Demo


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  systemfit,
  wooldridge,
  ivreg
)

data('mroz', package = 'wooldridge')



# -------------------------------------------------------------------------


# keep only working women
mroz <- filter(mroz, inlf == 1)



## HOURS -------------------------------------------------------------------


# Regression for hours using OLS estimation
model1 <- lm(hours ~ lwage + educ + age + kidslt6 + nwifeinc, mroz)
summary(model1)

# Regression for hours using 2SLS estimation
# lwage is instrumented by variables from the other equation
model2 <- ivreg(
  hours ~ lwage + educ + age + kidslt6 + nwifeinc |
    . - lwage + exper + expersq,
  data = mroz
)
summary(model2)



## LWAGE -------------------------------------------------------------------


# Regression for lwage using OLS estimation
model3 <- lm(lwage ~ hours + educ + exper + expersq, mroz)
summary(model3)

# Regression for lwage using 2SLS estimation
# hours is instrumented by variables from the other equation
model4 <- ivreg(lwage ~ hours + educ + exper + expersq |
                  ~ . - hours + age + kidslt6 + nwifeinc,
                data = mroz)
summary(model4, diagnostics = TRUE)



# SYSTEMFIT ---------------------------------------------------------------



hours_eq <- hours ~ lwage + educ + age + kidslt6 + nwifeinc
lwage_eq <- lwage ~ hours + educ + exper + expersq
system <- list(hours = hours_eq, lwage = lwage_eq)
instruments <- ~ educ + age + kidslt6 + nwifeinc + exper + expersq

model <- systemfit(
  formula = system,
  inst = instruments,
  method = '2SLS',
  data = mroz
)
summary(model)



# Testing for rank condition ----------------------------------------------


# Testing for rank condition involves estimating the reduced form equation
# and testing for significance of the instrument variables.

# Reduced form equation for lwage, identifying equation for hours
model5 <- lm(lwage ~ educ + age + kidslt6 + nwifeinc + exper + expersq, mroz)
test <- lm(lwage ~ educ + age + kidslt6 + nwifeinc, mroz)
# Could use update
summary(model5)
anova(model5, test)
linearHypothesis(model5, c("exper = 0", "expersq = 0"))

# Reduced form equation for hours, identifying equation for lwage
model6 <- lm(hours ~ educ + age + kidslt6 + nwifeinc + exper + expersq, mroz)
summary(model6)
linearHypothesis(model6, c("age = 0", "kidslt6 = 0", "nwifeinc = 0"))

