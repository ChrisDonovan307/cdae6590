# Lab 8 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  AER,
  dplyr,
  performance,
  snakecase,
  tibble,
  pscl,
  sampleSelection,
  fitdistrplus
)



# Datasets ----------------------------------------------------------------


data(Affairs)
str(Affairs)
hist(Affairs$affairs)

glm1 <- glm(
  affairs ~ gender + age + yearsmarried + religiousness,
  data = Affairs,
  family = 'poisson'
)
summary(glm1)
performance::check_model(glm1)
# This is no good


data("Medicaid1986")
str(Medicaid1986)

glm1 <- glm(
  visits ~ health1 + health2 + age + income + children,
  data = Medicaid1986,
  family = 'poisson'
)
summary(glm1)
performance::check_model(glm1)
# This is not much good either



data("bioChemists")
str(bioChemists)

glm1 <- glm(
  art ~ fem + mar + kid5 + phd + ment,
  data = bioChemists,
  family = 'poisson'
)
summary(glm1)
performance::check_model(glm1)



# Poisson -----------------------------------------------------------------


asthma <- read.csv('datasets/asthma.csv')
str(asthma)
# GHQ-12 is the score from the General Health Questionnaire
# res_inf is whether they have been admitted to a hospital
# attack is the number of astham attacks they have had in the last year

# Check out the attack variable
hist(asthma$attack)

# Try a linear model
lm1 <- glm(attack ~ gender + res_inf + ghq12, data = asthma)
performance::check_model(lm1)
# Normality of residuals look particularly bad!

# Try it with a poisson regression
pois1 <- glm(
  attack ~ gender + res_inf + ghq12,
  family = 'poisson',
  data = asthma
)
performance::check_model(pois1)
# Notice the difference in the normality of the residuals!

# Now we can interpret it
summary(pois1)



## Pseudo R2 ---------------------------------------------------------------


# McFadden's Pseudo R2 = 1 - (loglikelihood of model / loglikelihood of null)

# Check log likelihood of model
logLik(pois1)

# But we need a null model to divide it by
pois_null <- glm(
  attack ~ 1,
  family = 'poisson',
  data = asthma
)
summary(pois_null)

# Now we can manually calculate McFadden's R2
1 - (logLik(pois1)[1]) / (logLik(pois2)[1])

# Alternatively, we can just use a function from pscl
pscl::pR2(pois1)



# Logistic ----------------------------------------------------------------
## Wrangling TTIR Data ----------------------------------------------------


# Dataset from USDA Agricultural Marketing Service
# Average Truck Travel Time Reliability Index (TTIR) for select corridors
# https://agtransport.usda.gov/Truck/Average-Truck-Travel-Time-Reliability-Index-TTIR-f/eg7p-mw34

volpe <- read.csv('datasets/volpe.csv')
str(volpe)

# Clean up the variable names with snakecase package
names(volpe) <- snakecase::to_snake_case(names(volpe))
str(volpe)

# Clean up some variable names
volpe <- dplyr::rename(
  volpe,
  dest_mile = post_calibration_destination_milepost,
  origin_mile = post_calibration_origin_milepost,
  ttir = truck_travel_time_reliability_index,
  tti = travel_time_index,
  highway = highway_segment
)
str(volpe)

# Select only the variables we are interested in
volpe <- select(
  volpe,
  dest_mile,
  origin_mile,
  ttir,
  tti,
  highway
)
str(volpe)

# Check out distribution of reliability index
hist(volpe$ttir)

# Create a state variable
volpe$state_abb <- stringr::str_split_i(volpe$highway, '_', 2)
str(volpe)

# Get a dataframe with the region each state is in
data(state)
state_regions <- data.frame(
  state.name,
  state.abb,
  state.region
)
str(state_regions)

# Join state_data and state_regions to make state_info
state_info <- inner_join(
  state_data,
  state_regions,
  by = join_by(state_name == state.name
))
str(state_info)

# Now add state_info to volpe
volpe <- left_join(volpe, state_info, by = join_by(state_abb == state.abb))
str(volpe)

# Just join volpe with state regions for the region
volpe <- left_join(volpe, state_regions, by = join_by(state_abb == state.abb))
str(volpe)

# Let's just select the variables we care about now. Note that we can select
# columns and rename them at the same time with select()
volpe <- volpe %>%
  select(
    state_name = state.name,
    dest_mile,
    origin_mile,
    ttir,
    tti,
    region = state.region
  )
str(volpe)

# Create a dummy variable with cutoff at 1.5
# 1.5 or greater is unreliable according to OKI
# https://performance.oki.org/mobility-congestion/federal-performance-measures/
volpe$reliable <- ifelse(volpe$ttir > 1.5, 0, 1)
table(volpe$reliable)

# Get difference between destination mile and origin mile
volpe$distance <- abs(volpe$dest_mile - volpe$origin_mile)
hist(volpe$distance)



## Regressions ------------------------------------------------------------


# Try a linear regression
lm1 <- lm(reliable ~ distance + region, data = volpe)
performance::check_model(lm1)

# Try logistic regression
logit1 <- glm(
  reliable ~ distance + region,
  data = volpe,
  family = binomial(link = 'logit')
)
performance::check_model(logit1)
summary(logit1)
# North Central region has significantly higher logit of ttir from Northeast.
# This means it is more reliable than Northeast.



## Interpreting Coefficients -----------------------------------------------


# Coefficients are in log-odds (logits)
# To transform back to odds, we exponentiate with exp()

# Original coefficients
coef(logit1)

# Exponentiated coefficients
(odds <- exp(coef(logit1)))
# Odds of unreliable roads in North Central region are 103% higher than in
# the Northeast.

# But this is not probability. Remember that odds = p/(1-p), so to solve for p:
# odds*(1-p) = p
# odds - p*odds = p
# odds = p + p*odds
# odds = p(1 + odds)
# p = odds/(1 + odds)

(prob <- odds/(1 + odds))
# These are the probabilities of having unreliable roads in each region.
# Not the change in probability!

# Make a function to make this easier
logit_to_prob <- function(logit) {
  odds <- exp(logit)
  prob = odds/(1 + odds)
  return(prob)
}

# Test it
(logit <- coef(logit1)[['regionNorth Central']])
logit_to_prob(logit)
# Same as above



## Model Comparison --------------------------------------------------------


# Run a reduced logit model to compare to our logit1 model
logit2 <- glm(
  reliable ~ region,
  data = volpe,
  family = binomial(link = 'logit')
)


## Compare the two models with LR test.
lrtest(logit1, logit2)
# Just like with F test in ANOVA - if significant, complex model is better


## Information Criteria: AIC and BIC
# Both balance fit from likelihood with model complexity
# Lower numbers are better
# With small samples, AIC favors complex models
# BIC has a stronger penalty, so favors simpler models
AIC(logit1, logit2)
BIC(logit1, logit2)

# More info on these:
# https://bookdown.rstudioconnect.com/mike/data_analysis/sec-linear-mixed-models.html?q=aicc#information-criteria-for-model-selection



# More Models -------------------------------------------------------------
## Probit ------------------------------------------------------------------


# Nearly same as logit, just change link function
probit1 <- glm(
  reliable ~ distance + region,
  data = volpe,
  family = binomial(link = 'probit')
)

# Compare with AIC
lrtest(logit1, probit1)
AIC(logit1, probit1)
BIC(logit1, probit1)
# Note that we can compare non-nested models now, unlike with F-test



## Tobit -------------------------------------------------------------------


data("Affairs")
?Affairs
str(Affairs)

# Right censor at 4
tobit <- AER::tobit(
  affairs ~ age + yearsmarried + religiousness + occupation + rating,
  right = 4,
  data = Affairs
)
summary(tobit)



## Heckit ------------------------------------------------------------------


## Greene (2003). Econometric Analysis, Fifth Edition, Prentice Hall.
# example 22.8, page 786
data(Mroz87)

# Come two kids variables into 1
Mroz87$kids <- (Mroz87$kids5 + Mroz87$kids618 > 0)

# Heckit two step procedure
# 1. Probability of a woman participating in work force
# 2. If selected, predict wage as a function of other variables
heckit1 <- sampleSelection::heckit(
  selection = lfp ~ age + I( age^2 ) + faminc + kids + educ,
  outcome = wage ~ exper + I( exper^2 ) + educ + city, Mroz87
)
summary(heckit1)



# Fit a Distribution ------------------------------------------------------


# With the fitdistrplus package
data("groundbeef")
fitdistrplus::descdist(groundbeef$serving, boot = 1000)

# With the gamlss package. This is pretty neat but often more
fit_out <- gamlss::fitDist(groundbeef$serving, type = 'realAll')
# Use type argument to choose between real numbers, counts, positive numbers,
# etc. Check documentation for details

# Check output for best fitting distribution
fit_out

# See ranked list of all distributions fit
fit_out$fits
# Check the documentation to identify these abbreviations. Note that this can be
# more trouble than it's worth, as many are quite obscure and hard to find or
# use in the wild.



# Resources ---------------------------------------------------------------


# Quantitude Podcast, S2E17: Logistic Regression: 2 Logit 2 Quit
#   https://quantitudepod.org/?s=logistic+regression
# Quantitude Podcast: S4E02: Underachievers, Overachievers, & Maximum Likelihood
#   estimation
#   https://quantitudepod.org/?s=maximum
# DataCamp: Generalized Linear Models in R
#   https://app.datacamp.com/learn/courses/generalized-linear-models-in-r
# Principles of Econometrics with R, Chapter 6: Qualitative and LDV Models
#   https://bookdown.org/ccolonescu/RPoE4/qualitative-and-ldv-models.html
# Introduction to Econometrics with R, Chapter 11: Regression with a binary
#   variable.
#   https://www.econometrics-with-r.org/11-rwabdv.html
