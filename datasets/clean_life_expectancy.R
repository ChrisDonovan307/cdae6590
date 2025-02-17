# Clean Life Expectancy
# 2025-02-17


# Description -------------------------------------------------------------


# Script to clean life expectancy data for lab6 assignment, pulled from:
# https://ourworldindata.org/life-expectancy



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  readr,
  janitor,
  dplyr
)

dat <- readr::read_csv('datasets/life-expectancy-vs-health-expenditure.csv') %>%
  janitor::clean_names()
get_str(dat)



# Clean  ------------------------------------------------------------------


dat <- dat %>%
  dplyr::select(
    code,
    year,
    population = population_historical,
    life_exp = life_expectancy_sex_all_age_0_variant_estimates,
    health_expenditure = health_expenditure_per_capita_total
  ) %>%
  dplyr::filter(year >= 2000, !is.na(health_expenditure))
get_str(dat)

# Save
saveRDS(dat, 'datasets/life_exp.rds')
write_csv(dat, 'datasets/life_exp.csv')



