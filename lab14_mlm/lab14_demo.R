# Lab 14 Demo
# 2025-04-23


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  lme4,
  lmerTest,
  httr,
  GET,
  jsonlite,
  stringr,
  glue,
  tidyr,
  performance,
  rnassqs
)

options(scipen = 999)


# Using APIs --------------------------------------------------------------


# APIs are how programs communicate
# GET, POST, PUT, DELETE, etc.

# Guidelines
#   Read the documentation
#   Only take what you need
#   Cache with memoise()
#   Consider adding timers with Sys.sleep()
#   Don't hard code your API key

# Great resources for APIs in R:
#   https://paulcbauer.github.io/apis_for_social_scientists_a_review/



## Search by Category ------------------------------------------------------


# Search for public safety datasets
url <- "http://api.us.socrata.com/api/catalog/v1?domains=data.vermont.gov&categories=Public%20Safety&limit=10"

meta <- httr::GET(url) %>%
  httr::content("text") %>%
  jsonlite::fromJSON(flatten = TRUE)
str(meta)
str(meta$results)
dataset_id <- meta$results$resource.id[1]



## Download Dataset --------------------------------------------------------


# Download the dataset we found
dataset_id

url <- paste0("https://data.vermont.gov/resource/", dataset_id, ".json")
data_out <- httr::GET(url) %>%
  httr::content("text") %>%
  jsonlite::fromJSON(flatten = TRUE)
data_out
str(data_out)
head(data_out)



## More Programmatic Method ------------------------------------------------


get_meta <- function(domain, limit) {
    "http://api.us.socrata.com/api/catalog/v1?domains=${domain}&limit=${limit}" %>%
        stringr::str_interp() %>%
        httr::GET() %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten = TRUE)
}

out <- get_meta(domain = 'data.vermont.gov', limit = 10)
str(out)

# Remove objects from environment
rm(list = ls())



# NASS Dataset ------------------------------------------------------------
## API ---------------------------------------------------------------------


# Load API key. This won't work for you - get your own key!
readRenviron('.Renviron')
api_key <- Sys.getenv('NASS_API_KEY')

# Authorize API key
nassqs_auth(api_key)

# Check available parameters
nassqs_params()

# Check options for parameters
nassqs_param_values('sector_desc')

# Set variables to query
(short_descs <- nassqs_param_values(param = 'short_desc'))
vars <- c(
  str_subset(short_descs, 'TOURISM.*IN \\$$'),
  str_subset(short_descs, 'NO-TILL - ACRES$'),
  str_subset(short_descs, '^INCOME.*RECEIPTS.*/ OPERATION')[1]
)

# Make list of parameters to query
params <- list(
  short_desc = vars,
  year = 2022,
  agg_level_desc = 'COUNTY',
  domain_desc = 'TOTAL'
)

# Check record count, has to be < 50000
records <- nassqs_record_count(params)
assertthat::assert_that(as.integer(records$count) <= 50000)

# Send query through NASS API
out <- nassqs(params)
get_str(out)

# Save it
saveRDS(out, 'datasets/nass_mlm_out.rds')



## Clean NASS Data ---------------------------------------------------------


# Pull saved data from API out
out <- readRDS('datasets/nass_mlm_out.rds')

# Check variables
unique(out$short_desc)
get_str(out)

# Clean dataset
dat <- out %>%
  select(
    value = Value,
    state = state_name,
    county = county_name,
    var = short_desc
  ) %>%
  mutate(
    across(c(state, county), ~ str_to_title(.x)),
    var = case_when(
      str_detect(var, 'NO-TILL') ~ 'no_till',
      str_detect(var, 'TOURISM') ~ 'tourism',
      str_detect(var, 'RECEIPTS') ~ 'income'
    )
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = value
  )
get_str(dat)



## Wrangle State Data ------------------------------------------------------


# Pull BEA data
gdp <- read.csv('datasets/bea_gdp_by_state.csv', skip = 3)
get_str(gdp)

gdp <- gdp %>%
  select(
    desc = Description,
    state = GeoName,
    value = X2024
  ) %>%
  filter(str_detect(desc, '\\(GDP\\)')) %>%
  mutate(desc = 'state_gdp') %>%
  filter(!state %in% c('United States', 'District of Columbia')) %>%
  pivot_wider(
    names_from = desc,
    values_from = value
  ) %>%
  mutate(state_gdp = as.numeric(state_gdp))
get_str(gdp)

# Join with state area
state_area <- bind_cols(state.name, state.area) %>%
  setNames(c('state', 'area'))
state_dat <- gdp %>%
  left_join(state_area)
get_str(state_dat)



## Joins -------------------------------------------------------------------


str(dat)
str(state_dat)

dat <- inner_join(dat, state_dat)
str(dat)


## Imputation --------------------------------------------------------------


get_str(dat)

# Has to be base data.frame, not tibble
# Only takes numeric and factor variables
input <- dat %>%
  select(-county) %>%
  mutate(state = factor(state)) %>%
  as.data.frame()
str(input)

# Impute
pacman::p_load(missForest)
out <- missForest(
  input,
  ntree = 100,
  variablewise = TRUE,
  verbose = TRUE
)

out$OOBerror
str(out$ximp)

# Join back with counties
clean <- bind_cols(dat['county'], out$ximp)
str(clean)

# Save to dataset folder
saveRDS(clean, 'datasets/lab14_nass_data.rds')
rm(list = ls())



# Multilevel Models -------------------------------------------------------


# Pull clean dataset from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/fsci_data.rds'
con <- gzcon(url(github_url, 'rb'))
fsci <- readRDS(con)
close(con)

# Pull from datasets folder
nass <- readRDS('datasets/lab14_nass_data.rds')
str(nass)

#' no-till: Total acres in no-till conservation practice
#' tourism: Total income from Ag tourism and recreation
#' income: Total income from farm-related activities in $ per operation



## Context -----------------------------------------------------------------


# FE/RE remove unobserved effects, entity specific characteristics
# FE: model within effect only, control for between
# RE: include random intercept, but interpret as blend of within and between
# MLM: explicitly model BOTH the within effects AND between effects
#   Does the slope vary by group?

# Assumption of OLS: IID Residuals (independent and identical)
#   OLS is naive to groupings
#   Ecological fallacy - apply group level results to individual level
#   Atomistic fallacy - apply individual level results to group level

# Limitations:
#   Need large sample size
#   Convergence problems
#   Scaling
#   Worth mentioning spatial regression

# If you don't have a multi-level question, don't use a multi-level model
#   Cluster robust standard errors
#   Groups as fixed effects

# Extensions
#   Well suited for repeated measure data
#   MRPS: Multi-level regression with post-stratification



## Model Progression -------------------------------------------------------
### Null Model --------------------------------------------------------------


str(df)

## Null model, intercept only
null <- lme4::lmer(income ~ 1 + (1 | state), data = nass)

# Check intra class correlation
performance::icc(null)
# 25% of variation is at state level, 75% at county level
# Adjusted: only random effect variances
# Unadjusted: includes fixed effect variances (but there are none)




### Add Level 1 Predictor ---------------------------------------------------


## Add predictor to level 1
lmer1 <- lme4::lmer(income ~ no_till + (1 | state), data = nass)
# Scaling issues

# Rescale
df <- nass %>%
  mutate(across(where(is.numeric), scale))

# Try again
lmer1 <- lme4::lmer(income ~ no_till + (1 | state), data = df)
summary(lmer1)
# Notice there are no p-values. Great resource on this and much more with GLMMs:
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# Other packages can give us p-values though:
lmer1 <- lmerTest::lmer(income ~ no_till + (1 | state), data = df)

# Compare null model to no_till model
anova(null, lmer1)
summary(lmer1)

# Check R2
performance::r2(lmer1)
# Marginal: Fixed effects only
# Conditional: Both fixed and random effects


## Plot it
df %>%
  ggplot(aes(x = no_till, y = income, color = state)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(group = state), method = "lm", se = FALSE, fullrange = TRUE) +
  coord_cartesian(xlim = c(0, 100000), ylim = c(0, 200000)) +
  theme_classic() +
  theme(legend.position = 'None')



### Add More Predictors -----------------------------------------------------


## Add tourism as a first-level predictor
lmer2 <- lmerTest::lmer(income ~ no_till + tourism + (1 | state), data = df)

# Compare models
anova(lmer1, lmer2)
summary(lmer2)
# Level 2 intercept: average value of income
# variance: how much states vary around average income


## Add tourism as a random slope
lmer3 <- lmerTest::lmer(income ~ no_till + tourism + (tourism | state), data = df)

# Compare
anova(lmer2, lmer3)
summary(lmer3)
# Correlation is between slope of tourism and intercept of tourism
