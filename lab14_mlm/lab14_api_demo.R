# Lab 14 API Demo


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  httr,
  GET,
  jsonlite,
  stringr,
  glue,
  tidyr,
  rnassqs
)



# Using APIs --------------------------------------------------------------


# APIs are how programs communicate
# GET, POST, PUT, DELETE, etc.

# Guidelines
#   Read the documentation
#   Only take what you need, limit search results
#   Cache with memoise()
#   Separate API calls from rest of script
#   Consider adding timers with Sys.sleep()
#   Don't hard code your API key

# Great resources for APIs in R:
#   https://paulcbauer.github.io/apis_for_social_scientists_a_review/



## Search by Category ------------------------------------------------------


# Pulling data from Vermont Open Data Portal: https://data.vermont.gov/
# API documentation: https://dev.socrata.com/docs/endpoints

# Search for metadata for public safety datasets
url <- "http://api.us.socrata.com/api/catalog/v1?domains=data.vermont.gov&categories=Public%20Safety&limit=10"
# URL works in browser

meta <- httr::GET(url) %>%
  httr::content("text") %>%
  jsonlite::fromJSON(flatten = TRUE)

# Check results
str(meta$results)

# Check names
meta$results$resource.name

# Save resource ID
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


get_meta <- function(domain, category, limit) {
    "http://api.us.socrata.com/api/catalog/v1?domains=${domain}&limit=${limit}&categories=${category}" %>%
        stringr::str_interp() %>%
        httr::GET() %>%
        httr::content("text") %>%
        jsonlite::fromJSON(flatten = TRUE)
}

out <- get_meta(domain = 'data.vermont.gov', category = 'Education', limit = 10)
str(out)



# NASS API ----------------------------------------------------------------


# Note that the rest of the script won't work on your machine

# Using NASS QuickStat database: https://quickstats.nass.usda.gov/
# QuickStat documentation: https://quickstats.nass.usda.gov/api
# Pre-defined queries:
#   https://www.nass.usda.gov/Data_and_Statistics/Pre-Defined_Queries/index.php

# Load API key. This won't work for you - get your own key!
# For guide on how to use Renviron:
#   https://paulcbauer.github.io/apis_for_social_scientists_a_review/

# Read API key from your Renviron file - don't hard code
readRenviron('.Renviron')
api_key <- Sys.getenv('NASS_API_KEY')

# Authorize API key
nassqs_auth(api_key)

# Check available parameters
nassqs_params()

# Check options for parameters
nassqs_param_values('sector_desc')

# Get list of variables
(short_descs <- nassqs_param_values(param = 'short_desc'))

# Regex to pull out a few specific ones
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
str(out)

# Save it
saveRDS(out, 'datasets/nass_mlm_out.rds')



# Cleaning and Wrangling --------------------------------------------------


# Pull saved data from API out
out <- readRDS('datasets/nass_mlm_out.rds')

# Check variables
str(out)
unique(out$short_desc)

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
str(dat)



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

# Impute with missForest. Consider missRanger if if takes too long.
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
