# Clean Survey
# 2025-01-23


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  skimr,
  readxl,
  janitor,
  tibble,
  readr
)

pacman::p_load_gh('ChrisDonovan307/projecter')

raw <- read_csv('surveys/Econometrics Data Survey Conner Spring25_January 23, 2025_04.40.csv')
get_str(raw)



# Clean -------------------------------------------------------------------


# Put column names in snake case, fix a hinky column name
# Also removing a row of Qualtrics gunk (second row)
dat <- select(raw, Student:last_col()) %>%
  janitor::clean_names() %>%
  rename(hh_earn = h_hearn) %>%
  slice(-2)
get_str(dat)

# Save crosswalk of var names and question text
crosswalk <- dat[1,] %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c('var', 'text'))

# Variable classes:
#   month_rent as numeric midpoints AND as ordinal
#   own as factor
#   live_years as numeric
#   likely_move as factor
#   where_live as character
#   employ (select all) as dummies
#   race (select all) as dummies

# get unique employment responses
employ_types <- dat[-1,]$employ %>%
  str_split(',') %>%
  unlist() %>%
  unique()

# Remove first row which contains question text, then clean
dat <- dat %>%
  slice(-1) %>%
  mutate(
    month_rent = case_when(
      month_rent == 'Less than 500' ~ 250,
      month_rent == '500-749' ~ 625,
      month_rent == '750-999' ~ 875,
      month_rent == '1,000-1,499' ~ 1250,
      month_rent == '1,500-1,999' ~ 1750,
      month_rent == '2,000-2,400' ~ 2250,
      month_rent == 'More than 2,500' ~ 3000,
      .default = NA_real_
    ),
    own = case_when(
      str_detect(own, 'Rent \\(house') ~ 'rent',
      str_detect(own, 'Rent \\(res') ~ 'res',
      str_detect(own, 'Own') ~ 'own',
      .default = NA
    ) %>%
      as.factor(),
    live_years = as.numeric(live_years),
    likely_move = factor(
      likely_move,
      levels = c(
        'Extremely unlikely',
        'Somewhat unlikely',
        'Somewhat likely',
        'Extremely likely'
      ),
      ordered = TRUE
    ),
    employ_full = case_when(
      str_detect(employ, 'full') ~ 1,
      .default = 0
    ),
    employ_retired = case_when(
      str_detect(employ, 'retired') ~ 1,
      .default = 0
    ),
    employ_student = case_when(
      str_detect(employ, 'student') ~ 1,
      .default = 0
    ),
    employ_not = case_when(
      str_detect(employ, 'not employed') ~ 1,
      .default = 0
    ),
    employ_part = case_when(
      str_detect(employ, 'part') ~ 1,
      .default = 0
    )
  )
get_str(dat)

# Recode race different this time
# Get unique inputs for race
races <- dat$race %>%
  str_split(',') %>%
  unlist %>%
  str_remove('\\.') %>%
  str_trim() %>%
  unique

# Recode NA race ass race_na
# races[is.na(races)] <- 'na'

# Recode all of them at once
dat <- dat %>%
  mutate(
    across(
      .cols = all_of("race"),
      .fns = list(
        !!!setNames(
          lapply(races, function(race) {
            ~ as.numeric(str_detect(.x, race))
          }),
          snakecase::to_snake_case(races)
        )
      )
    )
  )
get_str(dat)

# Fix up the NAs for people who did not respond to race question
dat <- dat %>%
  mutate(
    race_NA = ifelse(is.na(race), 1, 0),
    across(starts_with('race_'), ~ ifelse(is.na(.x), 0, .x))
  )
get_str(dat)

# Continue with more:
#   gender as dummies
#   income as numeric midpoints AND as ordinal
#   age as numeric
#   hh as numeric
#   hh_earn as numeric
#   educ as factor
#   letters as numeric
dat <- dat %>%
  mutate(
    gender_female = case_when(
      gender == 'Female' ~ 1,
      gender == 'Male' ~ 0,
      .default = NA
    ),
    income = case_when(
      str_detect(income, '^Less') ~ 12500,
      str_detect(income, '^25,000') ~ 37500,
      str_detect(income, '^50,000') ~ 75000,
      str_detect(income, '^100') ~ 125000,
      str_detect(income, '^150') ~ 175000,
      str_detect(income, '^More') ~ 250000,
      .default = NA
    ),
    across(c(age:hh_earn), as.numeric),
    educ_num = case_when(
      str_detect(educ, '^None') ~ 0,
      str_detect(educ, '^High') ~ 1,
      str_detect(educ, '^Assoc') ~ 2,
      str_detect(educ, '^Bach') ~ 3,
      str_detect(educ, '^Master') ~ 4,
      str_detect(educ, '^Doct') ~ 5,
      .default = NA_real_
    ),
    educ_ord = factor(educ_num, ordered = TRUE),
    letters = as.numeric(letters)
  ) %>%
  select(-race_NA)
get_str(dat)

# Make the race_ variables shorter
dat <- dat %>%
  setNames(c(names(.) %>%
               str_split_i('_or_', 1) %>%
               str_replace('american_indian', 'ai') %>%
               str_replace('native_hawaiian', 'nh')
             ))
get_str(dat)



# Save --------------------------------------------------------------------


# Save as both csv and rds
readr::write_csv(dat, 'surveys/clean_survey.csv')
saveRDS(dat, 'surveys/clean_survey.rds')

# Save another version where we add a letters var value for use in lab3
# Note that there is an error in coding income leading to 16 NA values here
# We are leaving it for posterity
# dat$letters <- ifelse(is.na(dat$letters), 5, dat$letters)
# readr::write_csv(dat, 'surveys/lab3_survey.csv')
# saveRDS(dat, 'surveys/lab3_survey.rds')

# Save a version for lab4
readr::write_csv(dat, 'surveys/lab4_survey.csv')
saveRDS(dat, 'surveys/lab4_survey.rds')

# Save the crosswalk as a csv
readr::write_csv(crosswalk, 'surveys/crosswalk.csv')

# Save a version without dummy variables for lab5
lab5_survey <- dat %>%
  select(-c(employ_full:last_col()))
readr::write_csv(lab5_survey, 'surveys/lab5_survey.csv')
saveRDS(lab5_survey, 'surveys/lab5_survey.rds')
