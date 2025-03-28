# Lab 8 Demo
# Limited Dependent Variables


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

options(scipen = 999)



# Poisson -----------------------------------------------------------------


# Load data from Figure 1 - trends over time
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/asthma.rds'
con <- gzcon(url(github_url, 'rb'))
asthma <- readRDS(con)
close(con)

str(asthma)
# GHQ-12 is the score from the General Health Questionnaire
# res_inf is whether they have been admitted to a hospital
# attack is the number of astham attacks they have had in the last year

# Check out the attack variable
hist(asthma$attack)

# Try a linear model
lm1 <- lm(attack ~ gender + res_inf + ghq12, data = asthma)
performance::check_model(lm1)

# Use glm to run the same linear regression
lm2 <- glm(
  attack ~ gender + res_inf + ghq12,
  # family = gaussian(link = 'identity'),
  # family = gaussian,
  data = asthma
)

# Compare this to the lm
summary(lm1)
summary(lm2)

# Try it with a poisson regression
pois1 <- glm(
  attack ~ gender + res_inf + ghq12,
  family = poisson(link = 'log'),
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
1 - (logLik(pois1)[1]) / (logLik(pois_null)[1])

# Alternatively, we can just use a function from pscl
pscl::pR2(pois1)



# Logistic ----------------------------------------------------------------
## Wrangling TTIR Data ----------------------------------------------------


# Dataset from USDA Agricultural Marketing Service
# Average Truck Travel Time Reliability Index (TTIR) for select corridors
# https://agtransport.usda.gov/Truck/Average-Truck-Travel-Time-Reliability-Index-TTIR-f/eg7p-mw34

# Load data from Figure 1 - trends over time
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/volpe.rds'
con <- gzcon(url(github_url, 'rb'))
volpe_raw <- readRDS(con)
close(con)

str(volpe_raw)

# Clean up the variable names with snakecase package
names(volpe_raw) <- snakecase::to_snake_case(names(volpe_raw))
str(volpe_raw)

# Clean up some variable names
volpe_renamed <- dplyr::rename(
  volpe_raw,
  dest_mile = post_calibration_destination_milepost,
  origin_mile = post_calibration_origin_milepost,
  ttir = truck_travel_time_reliability_index,
  tti = travel_time_index,
  highway = highway_segment
)
str(volpe_renamed)

# Select only the variables we are interested in
volpe_selection <- dplyr::select(
  volpe_renamed,
  dest_mile,
  origin_mile,
  ttir,
  tti,
  highway
)
str(volpe_selection)

# Check out distribution of reliability index
hist(volpe_selection$ttir)

# Create a state abbrevation variable by splitting on _ and keeping second piece
volpe_selection$state_abb <- stringr::str_split_i(
  volpe_selection$highway,
  '_',
  2
)
str(volpe_selection)

# Get a dataframe with the region each state is in
data(state)
str(state.region)
str(state.name)

state_regions <- data.frame(
  state.name,
  state.abb,
  state.region
)
str(state_regions)

# Just join volpe with state regions for the region
volpe_join <- dplyr::left_join(
  volpe_selection,
  state_regions,
  by = join_by(state_abb == state.abb)
)
str(volpe_join)

# Let's just select the variables we care about now. Note that we can select
# columns and rename them at the same time with select()
volpe <- volpe_join %>%
  dplyr::select(
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
# This is how long the highway segment is
volpe$distance <- abs(volpe$dest_mile - volpe$origin_mile)
hist(volpe$distance)
str(volpe)
# Now we are ready for regressions



## Regressions ------------------------------------------------------------


# Try a linear regression
lm1 <- lm(reliable ~ distance + region, data = volpe)
performance::check_model(lm1)
# This is beyond bad

# Try logistic regression
logit1 <- glm(
  reliable ~ distance + region,
  family = binomial(link = 'logit'),
  # family = binomial,
  data = volpe
)
performance::check_model(logit1)
summary(logit1)



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
# First check the logLik of the models:
logLik(logit1)
logLik(logit2)

# We can run LR test manually:
(-2 * (logLik(logit1)[1] - logLik(logit2)[1]))

# Or we can do it automatically:
lrtest(logit1, logit2)
# Just like with F test in ANOVA - if significant, complex model is better


## Information Criteria: AIC and BIC
# Both balance fit from likelihood with model complexity
# Lower numbers are better
# With small samples, AIC favors complex models (weaker penalty)
# BIC has a stronger penalty, so favors simpler models
AIC(logit1, logit2)
BIC(logit1, logit2)



# More Models -------------------------------------------------------------
## Probit ------------------------------------------------------------------


# Nearly same as logit, just change link function
probit1 <- glm(
  reliable ~ distance + region,
  family = binomial(link = 'probit'),
  data = volpe
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
str(Mroz87)
# All respondents are married women

# Heckit two step procedure
# 1. Probability of labor force participation
# 2. If selected, predict wage as a function of experiencem, education, city
heckit1 <- sampleSelection::heckit(
  selection = lfp ~ age + I(age^2) + faminc + kids5 + kids618 + educ,
  outcome = wage ~ exper + I(exper^2) + educ + city,
  data = Mroz87
)
summary(heckit1)



## Even More Models --------------------------------------------------------


## Quasipoisson
quasipois1 <- glm(
  attack ~ gender + res_inf + ghq12,
  family = quasipoisson,
  data = asthma
)
summary(quasipois1)


## Zero inflated poisson
zip1 <- pscl::zeroinfl(
  attack ~ gender + res_inf + ghq12,
  dist = 'poisson',
  data = asthma
)
summary(zip1)


## Negative binomial
negbin1 <- glm.nb(
  attack ~ gender + res_inf + ghq12,
  link = log,
  data = asthma
)
summary(negbin1)



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
