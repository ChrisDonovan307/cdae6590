pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr,
  conflicted,
  janitor,
  tidyr,
  rnassqs
)

conflicted::conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  stats::lag(),
  .quiet = TRUE
)

# NASS api key
readRenviron('.Renviron')
api_key <- Sys.getenv('NASS_API_KEY')

# Short fips key
fips_key <- data.frame(
  state = c('VT', 'NH', 'NY', 'ME', 'MA', 'CT'),
  fips = c(50, 33, 36, 23, 25, 09)
)

# State key
state_key <- readRDS('temp/state_key.rds') %>%
  filter(state != 'DC')
state_key



# HTTR --------------------------------------------------------------------


# # Base request
# base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
#
# # Other Parameters
# source_desc <- 'CENSUS'
# years <- c(2017, 2022)
# years <- c(2022)
# state_fips <- paste0(state_key$state_code, collapse = ',')
# agg_level_desc <- 'STATE'
#
# # For each year, pull data from each state
# out <- map(years, \(year) {
#   map(state_fips, \(state) {
#
#     tryCatch({
#       url <- glue(
#         base_get,
#         '?key={api_key}',
#         '&state_fips_code={state}',
#         '&agg_level_desc={agg_level_desc}',
#         '&year={year}',
#         '&source_desc={source_desc}',
#         '&domain_desc=TOTAL'
#       )
#       print(url)
#       GET(url) %>%
#           content(as = 'text') %>%
#           fromJSON() %>%
#           .$data
#     },
#     error = function(e) {
#       message('Call failed')
#       print(e)
#     })
#
#   }) %>%
#     setNames(c(paste0('state', state_fips))) %>%
#     discard(is.null)
# }) %>%
#   setNames(c(paste0('year_', years)))
#
# get_str(out)
#
# # year_2017 <- out$year_2017
#
# # combine
# flat <- out %>%
#   flatten() %>%
#   bind_rows()
# get_str(flat)
#
# saveRDS(flat, 'datasets/nass_out.rds')



# rnassqs -----------------------------------------------------------------


# Authorize API key
nassqs_auth(api_key)

# Check parameters
nassqs_params()
nassqs_param_values('sector_desc')
nassqs_param_values('prodn_practice_desc')
nassqs_param_values('domain_desc')
nassqs_param_values('domaincat_desc')
nassqs_param_values('commodity_desc')

# Get all short descriptions of variables
(short_descs <- nassqs_param_values(param = 'short_desc'))

nassqs_param_values(param = 'group_desc')
nassqs_param_values(param = 'sector_desc')

nassqs_param_values(
  "short_desc",
  agg_level_desc = 'STATE',
  domain_desc = 'TOTAL',
  source_desc = 'CENSUS',
  state_alpha = 'VT'
)

str_subset(short_descs, 'INCOME')
str_subset(short_descs, 'PRACTICES')
str_subset(short_descs, 'PRACTICES.*MEASURED IN PCT')
str_subset(short_descs, 'INCOME.*MEASURED IN PCT')
str_subset(short_descs, 'INCOME.*\\$')

# Set parameters for query
params <- list(
  short_desc = vars[['short_desc']],
  year = c(2012, 2017, 2022),
  agg_level_desc = 'STATE'
)

# Check record count, has to be < 50000
records <- nassqs_record_count(params)
assertthat::assert_that(as.integer(records$count) <= 50000)

# Get query
out <- nassqs(params)
get_str(out)

# Save
saveRDS(out, 'temp/nassqs_out.rds')

# Why only 4 coming through



# Wrangle -----------------------------------------------------------------


# dat <- readRDS('temp/nassqs_out.rds')
practices <- read.csv('temp/practices.csv')
income <- read.csv('temp/income.csv')

dat <- bind_rows(practices, income) %>%
  janitor::clean_names()
get_str(dat)

dat$data_item %>% unique
dat <- dat %>%
  mutate(
    data_item = case_when(
      str_detect(data_item, 'EASEMENT') ~ 'easements',
      str_detect(data_item, 'EXCL NO') ~ 'cons',
      str_detect(data_item, 'TILLAGE, NO') ~ 'no_till',
      str_detect(data_item, 'COVER') ~ 'cover',
      str_detect(data_item, 'RECEIPTS') ~ 'income'
    )
  )
get_str(dat)

# dat$data_item %>% unique()
# unique(dat$state_alpha)
# unique(dat$group_desc)
# unique(dat$sector_desc)

# vars <- data.frame(
#   short_desc = c(
#     "INCOME, FARM-RELATED, AG TOURISM & RECREATIONAL SERVICES - RECEIPTS, MEASURED IN $ / OPERATION",
#     "INCOME, FARM-RELATED, AG TOURISM & RECREATIONAL SERVICES - RECEIPTS, MEASURED IN PCT OF OPERATIONS (YES)",
#     "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $ / OPERATION",
#
#     "LABOR, HIRED - EXPENSE, MEASURED IN PCT OF OPERATING EXPENSES",
#     "PRACTICES, MEMBERSHIP IN A COOPERATIVE - OPERATIONS, MEASURED IN PCT OF OPERATIONS (YES)",
#     "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - AREA, MEASURED IN PCT OF FARM OPERATIONS",
#     "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - AREA, MEASURED IN PCT OF FARM OPERATIONS",
#
#     "PRACTICES, CERTIFIED NATURALLY GROWN - OPERATIONS, MEASURED IN PCT OF OPERATIONS (YES)",
#     "PRACTICES, CERTIFIED PASTURE-BASED MGMT - OPERATIONS, MEASURED IN PCT OF OPERATIONS (YES)",
#     "PRACTICES, ORGANIC - OPERATIONS, MEASURED IN PCT OF OPERATIONS (YES)",
#
#     "PRACTICES, FARM MGMT RECORDS, SEPARATE MARKETING PLAN - OPERATIONS, MEASURED IN PCT OF OPERATIONS (YES)",
#     "PRACTICES, FARM MGMT RECORDS, WRITTEN BUSINESS PLAN - OPERATIONS, MEASURED IN PCT OF OPERATIONS (YES)"
#   ),
#   name = c(
#     'tour_rec',
#     'tour_rec_perfarm',
#     'income_per_op',
#     'labor',
#     'coop',
#     'cons',
#     'no_till',
#     'natural',
#     'pasture',
#     'organic',
#     'marketing',
#     'business'
#   )
# )

get_str(dat)
dat <- dat %>%
  select(state, year, value, data_item) %>%
  mutate(value = value %>%
           str_remove_all(',') %>%
           as.numeric())
get_str(dat)

# Pivot wider
dat <- dat %>%
  pivot_wider(
    names_from = data_item,
    values_from = value
  )
get_str(dat)



# Save it -----------------------------------------------------------------


saveRDS(dat, 'temp/nass_panel.rds')



# Wrangle Time Series -----------------------------------------------------



nassqs_auth(api_key)
