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
  tidyr
)

pacman::p_load(
  dplyr,
  AER,
  plm,
  tidyr,
  ggplot2,
  wooldridge
)

conflicted::conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
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
state_key <- readRDS('state_key.rds') %>%
  filter(state != 'DC')
state_key

# Base request
base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Other Parameters
source_desc <- 'CENSUS'
years <- c(2017, 2022)
years <- c(2022)
state_fips <- paste0(state_key$state_code, collapse = ',')
agg_level_desc <- 'STATE'

# For each year, pull data from each state
out <- map(years, \(year) {
  map(state_fips, \(state) {

    tryCatch({
      url <- glue(
        base_get,
        '?key={api_key}',
        '&state_fips_code={state}',
        '&agg_level_desc={agg_level_desc}',
        '&year={year}',
        '&source_desc={source_desc}',
        '&domain_desc=TOTAL'
      )
      print(url)
      GET(url) %>%
          content(as = 'text') %>%
          fromJSON() %>%
          .$data
    },
    error = function(e) {
      message('Call failed')
      print(e)
    })

  }) %>%
    setNames(c(paste0('state', state_fips))) %>%
    discard(is.null)
}) %>%
  setNames(c(paste0('year_', years)))

get_str(out)

# year_2017 <- out$year_2017

# combine
flat <- out %>%
  flatten() %>%
  bind_rows()
get_str(flat)

saveRDS(flat, 'datasets/nass_out.rds')



# rnassqs -----------------------------------------------------------------


pacman::p_load(
  rnassqs
)

nassqs_auth(api_key)

nassqs_params()

short_descs <- nassqs_param_values(param = 'short_desc')
short_descs

params <- list(
  short_desc = vars[['short_desc']],
  year = c(2017, 2022),
  agg_level_desc = 'STATE'
)

records <- nassqs_record_count(params)
assertthat::assert_that(as.integer(records$count) <= 50000)

out <- nassqs(params)
get_str(out)

saveRDS(out, 'datasets/nassqs_out.rds')


# Explore -----------------------------------------------------------------


dat <- readRDS('datasets/nassqs_out.rds')


get_str(dat)
dat$short_desc %>% unique()
unique(dat$state_alpha)
unique(dat$group_desc)
unique(dat$sector_desc)

dat %>%
  janitor::clean_names() %>%
  filter(
    domain_desc == 'TOTAL',
    # group_desc == 'VEGETABLES',
    sector_desc == 'ECONOMICS'
  ) %>%
  select(state_alpha, group_desc, sector_desc, short_desc, value) %>%
  pull(short_desc) %>%
  unique()

vars <- data.frame(
  short_desc = c(
    "EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $ / OPERATION",
    "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - AREA, MEASURED IN ACRES / OPERATION",
    "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - AREA, MEASURED IN ACRES / OPERATION",
    "LABOR, HIRED - EXPENSE, MEASURED IN PCT OF OPERATING EXPENSES",
    "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $ / OPERATION",
    "INCOME, FARM-RELATED, AG TOURISM & RECREATIONAL SERVICES - RECEIPTS, MEASURED IN $ / OPERATION"
  ),
  name = c(
    'expenses_total',
    'acres_no_till',
    'acres_cover_crop',
    'expenses_pct_labor',
    'income_net',
    'income_ag_tour_rec'
  )
)

dat <- dat %>%
  janitor::clean_names() %>%
  filter(
    domain_desc == 'TOTAL',
    sector_desc == 'ECONOMICS'
  ) %>%
  select(state_alpha, group_desc, sector_desc, short_desc, value, unit_desc, year) %>%
  filter(short_desc %in% vars$short_desc) %>%
  mutate(value = value %>%
           str_remove_all(',') %>%
           as.numeric()) %>%
  filter(year != 2012) %>%
  left_join(state_key, by = join_by(state_alpha == state)) %>%
  left_join(vars)
get_str(dat)

# Clean names
dat <- dat %>%
  select(
    state = state_alpha,
    var = name,
    value,
    year
  )
get_str(dat)

# Pivot wider
piv <- dat %>%
  pivot_wider(
    names_from = var,
    values_from = value
  )
get_str(piv)

