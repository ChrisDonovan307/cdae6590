# Clean Base Pay
# 2025-02-14



# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  janitor,
  skimr,
  stringr,
  readxl,
  tidyr,
  purrr,
  readr,
  tictoc
)
pacman::p_load_gh('ChrisDonovan307/projecter')

raw <- read_excel('base_pay/2025-02-14_base_pay_paste.csv.xlsx')[[1]]
class(raw)
head(raw, 50)
length(raw)



# Wrangling ---------------------------------------------------------------


vec_dat <- raw[22:length(raw)] %>%
  na.omit() %>%
  str_subset(
    '^University of Vermont List|^Name|^Primary|List of|^Page [0-9]|^Base Pay$',
    negate = TRUE
  )
length(vec_dat)
vec_dat
# Order is name, title, salary. seems to hold all the way through now

# Every third element is names, jobs, salaries, etc
df <- data.frame(
  vec_dat = vec_dat,
  index = 1:length(vec_dat)
)
all <- list(
  name = df$vec_dat[df$index %% 3 == 1],
  job = df$vec_dat[df$index %% 3 == 2],
  salary = df$vec_dat[df$index %% 3 == 0]
)
map(all, length)

# Bind into a single data frame
dat <- bind_cols(all)
get_str(dat)
# This is it



# Names Dataset -----------------------------------------------------------


# Load names dataset from https://archive.ics.uci.edu/dataset/591/gender+by+name
names <- read_csv('base_pay/2025-02-15_name_gender_dataset.csv') %>%
  janitor::clean_names()
get_str(names)
get_table(names$gender)


## First remove the last name before the comma to get the first names only
# Then split first names by the sapce and keep first first name
get_str(dat)
dat <- dat %>%
  mutate(
    first_name = str_split_i(name, ',', 2) %>%
      str_split_i(' ', 1)
  )
get_str(dat)


## Have to deal with duplicate names in names dataset
get_str(names)

# Want to just keep the one with the higher count
(dup_names <- names$name[duplicated(names$name)])

# For each dup_name, remove the one with smaller count
get_str(names)

tic()
fixed_dup_names <- map(dup_names, \(dup_name) {
  selection <- filter(names, name == dup_name) %>%
    arrange(count)

  if ((selection$count[2] / selection$count[1]) > 1.1) {
    out <- selection[2, c('name', 'gender')]
  } else {
    out <- selection[2, c('name', 'gender')]
    out[1, 2] <- 'NB'
  }
  return(out)
}) %>%
  bind_rows()
toc()

get_str(fixed_dup_names)
length(unique(fixed_dup_names$name))

# Remove dup names from names dataset
indices <- which(duplicated(names$name))
names_singles <- names[-indices, ] %>%
  select(name, gender)
get_str(names_singles)
length(unique(names_singles$name))

# Combine our new dupes with the names singles
name_genders <- bind_rows(fixed_dup_names, names_singles) %>%
  unique() %>%
  arrange(name)
get_str(name_genders)
dim(unique(name_genders))

# Join to our dat df to get genders
get_str(dat)
out <- left_join(dat, name_genders, by = join_by(first_name == name))
get_str(out)

# Check out NA genders
out %>%
  filter(is.na(gender)) %>%
  print(n = 1000)
sum(is.na(out$gender))
# Looks reasonable. Couple of one letter abbreviations, this checks out

# Turn remaining NAs into NBs
out <- out %>%
  mutate(gender = ifelse(is.na(gender), 'NB', gender))
get_str(out)
print(out, n = 1000)

# Get proportions of each gender
get_table(out$gender)
# Looks pretty good

# Leave out intact for now, but resave as dat to keep moving
dat <- out
get_str(dat)



# Dummies -----------------------------------------------------------------


dat <- dat %>%
 mutate(

   # Binaries for certain categories
   com = ifelse(str_detect(job, '(COM)'), 1, 0),
   # job = str_remove(job, ' \\(COM\\)'),
   research = ifelse(str_detect(job, 'Research|Analyst'), 1, 0),
   # job = str_remove(job, regex('research', ignore_case = TRUE)) %>%
     # str_trim(),
   exec = ifelse(str_detect(job, 'Dir|President|Exec|VP|Dean|Provost'), 1, 0),
   athletic = ifelse(str_detect(job, 'Athletic'), 1, 0),
   specialist = ifelse(str_detect(job, 'Specialist'), 1, 0),
   mechanic = ifelse(str_detect(job, 'Mechanic'), 1, 0),
   lecturer = ifelse(str_detect(job, 'Lecturer'), 1, 0),
   professor = ifelse(str_detect(job, 'Professor'), 1, 0)

   # Reduce the unique jobs
   # job = case_when(
   #   str_detect(job, 'Lecturer') ~ 'lecturer',
   #   str_detect(job, 'Prof') ~ 'professor',
   #   str_detect(job, 'Specialist') ~ 'specialist',
   #   str_detect(job, 'Provost') ~ 'provost',
   #   str_detect(job, 'Technician|Tech') ~ 'technician',
   #   str_detect(job, 'Dean') ~ 'dean',
   #   str_detect(job, 'Admin') ~ 'admin',
   #   str_detect(job, 'Counselor') ~ 'counselor',
   #   str_detect(job, 'Scientist') ~ 'scientist',
   #   str_detect(job, 'Mechanic') ~ 'mechanic',
   #   str_detect(job, 'Custodial') ~ 'custodial',
   #   str_detect(job, 'Generalist') ~ 'generalist',
   #   str_detect(job, 'Director') ~ 'director',
   #   str_detect(job, 'Coach') ~ 'coach',
   #   .default = job
   # )
)
get_str(dat)
# unique(out$job)
# get_table(out$job)
# get_table(out$exec)


# Clean up columns, remove names before saving
dat <- dat %>%
  select(job, salary, gender, com:last_col()) %>%
  mutate(salary = as.numeric(salary))
get_str(dat)

# Save this
saveRDS(dat, 'base_pay/base_pay_clean.rds')



