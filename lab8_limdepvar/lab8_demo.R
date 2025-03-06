# Lab 8 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  AER,
  dplyr,
  performance,
  snakecase,
  tibble
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
get_str(asthma)

# Check out the attack variable
hist(asthma$attack)

# Try a linear model
lm1 <- glm(attack ~ gender + res_inf + ghq12, data = asthma)
performance::check_model(lm1)
# Normality of residuals look particularly bad!

# Try it with a poisson regression
glm1 <- glm(
  attack ~ gender + res_inf + ghq12,
  family = 'poisson',
  data = asthma
)
summary(glm1)
performance::check_model(glm1)
# Notice the difference in the normality of the residuals!



# Logistic ----------------------------------------------------------------


# Dataset from USDA Agricultural Marketing Service
# Average Truck Travel Time Reliability Index (TTIR) for select corridors
# https://agtransport.usda.gov/Truck/Average-Truck-Travel-Time-Reliability-Index-TTIR-f/eg7p-mw34

volpe <- read.csv('datasets/volpe.csv')
str(volpe)

# Clean up the variable names with snakecase package
names(volpe) <- snakecase::to_snake_case(names(volpe))
str(volpe)

# Clean up some variable names
volpe <- select(
  volpe,
  dest_mile = post_calibration_destination_milepost,
  origin_mile = post_calibration_origin_milepost,
  ttir = truck_travel_time_reliability_index,
  tti = travel_time_index,
  highway = highway_segment
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

# Get a dataframe with the number of days of frost (1931-1960)
state_data <- as.data.frame(state.x77)
str(state_data)
# Note that there are no state names

# Try printing the first few rows
head(state_data)
# Rownames show the states

# To get rownames into the data frame, use tibble package
state_data <- tibble::rownames_to_column(state_data, var = 'state_name')
str(state_data)

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

# Let's just select the variables we care about now
volpe <- volpe %>%
  select(
    state_name,
    dest_mile,
    origin_mile,
    ttir,
    tti,
    frost = Frost,
    region = state.region
  )
str(volpe)

# Create a dummy variable with cutoff at 1.5
# 1.5 or greater is unreliable according to OKI
# https://performance.oki.org/mobility-congestion/federal-performance-measures/
volpe$reliable <- ifelse(volpe$ttir > 1.5, 0, 1)
str(volpe)

# Get difference between destination mile and origin mile
volpe$distance <- abs(volpe$dest_mile - volpe$origin_mile)
str(volpe)

# Try linear regression
lm1 <- lm(reliable ~ distance + frost + region, data = volpe)
summary(lm1)
performance::check_model(lm1)

# Try logistic regression
logit1 <- glm(
  reliable ~ distance + frost + region,
  data = volpe,
  family = binomial(link = 'logit')
)
summary(logit1)
performance::check_model(logit1)

# Get R2
pR2(logit1)
