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
  performance
)

options(scipen = 999)


# API Example -------------------------------------------------------------
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



# NASS API ----------------------------------------------------------------


# Load API key. This won't work for you - get your own key!
readRenviron('.Renviron')
api_key <- Sys.getenv('NASS_API_KEY')

# Authorize API key
nassqs_auth(api_key)

# Check available parameters
nassqs_params()

# Check options for parameters
nassqs_param_values('sector_desc')
nassqs_param_values('short_desc')

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



# Wrangle State Data ------------------------------------------------------


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



# Joins -------------------------------------------------------------------


str(dat)
str(state_dat)

dat <- inner_join(dat, state_dat)
str(dat)

# Rescale
dat <- dat %>%
  mutate(across(c(no_till, tourism, income), ~ scale(.x)))



# Model Progression -------------------------------------------------------


get_str(dat)

## Null model, intercept only
null <- lmer(income ~ 1 + (1 | state), data = dat)

# Check intra class correlation
icc(null)
# 22% of variation is at state level, 78% at county level


## Add predictors to level 1
lmer1 <- lme4::lmer(income ~ no_till + (1 | state), data = dat)
summary(lmer1)
# Notice there are no p-values

# Other packages will give us those
lmer1 <- lmerTest::lmer(income ~ no_till + (1 | state), data = dat)
summary(lmer1)


## Add random slope
lmer2 <- lmerTest::lmer(income ~ no_till + (1 + tourism | state), data = dat)
summary(lmer2)

anova(lmer1, lmer2)
