# Lab 8 Demo
# Limited Dependent Variables


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  AER,
  dplyr,
  plm
)

options(scipen = 999)

# github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/asthma.rds'
# con <- gzcon(url(github_url, 'rb'))
# asthma <- readRDS(con)
# close(con)



# Explore -----------------------------------------------------------------


data(Fatalities)
get_str(Fatalities)

raw_fatalities <- Fatalities

# Make it panel data
Fatalities <- pdata.frame(Fatalities)
get_str(Fatalities)

# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)
coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")
# Both show positive relationship
# in 88, beer tax coefficient is way bigger than 92


## First difference
# compute the differences
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)
summary(fatal_diff_mod)
coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

# Get 1982 and 1988 only dataset
# Test whether plm gives same results
test <- raw_fatalities %>%
  filter(year %in% c('1982', '1984', '1988')) %>%
  pdata.frame()
test$fatal_rate <- test$fatal / test$pop * 10000
get_str(test)
summary(plm(fatal_rate ~ beertax, data = test, model = 'fd'))



# Structure ---------------------------------------------------------------
## Setup -------------------------------------------------------------------


data(Fatalities)
str(Fatalities)

# Make it panel data
Fatalities <- pdata.frame(Fatalities)
pdim(Fatalities)
get_str(Fatalities)

# Note structure of data
head(Fatalities, 15)[, 1:10]

# Define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000



## Individual Regressions --------------------------------------------------


# Subset the data to 1982 and 1988
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

summary(lm(fatal_rate ~ beertax, data = Fatalities1982))
summary(lm(fatal_rate ~ beertax, data = Fatalities1988))
# Both show positive relationship between tax and fatality rate
# It gets stronger with time.
# Unobserved variables - characteristics of states



## First Differenced -------------------------------------------------------


# Difference out the state characteristics (alpha_i)

# Get difference in fatality rate and beer tax for each year
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# Estimate a regression using differenced data
model_diff <- lm(diff_fatal_rate ~ diff_beertax)
summary(model_diff)
# Raising beer tax by $1 drops fatalities by 1.04 per 10,000


## Now with plm
# First get subset of data
Fatalities_82_88 <- Fatalities[Fatalities$year %in% c('1982', '1988'), ]
model_fd <- plm(
  fatal_rate ~ beertax,
  data = Fatalities_82_88,
  model = 'fd'
)
summary(model_fd)



## Pooled  -----------------------------------------------------------------


# Don't even do this?

pooled_data <- subset(Fatalities, year %in% c('1982', '1988'))

summary(lm(fatal_rate ~ beertax, data = pooled_data))
summary(lm(fatal_rate ~ beertax + year, data = pooled_data))
summary(plm(fatal_rate ~ beertax, data = pooled_data, model = 'pooling'))
summary(plm(fatal_rate ~ beertax + year, data = pooled_data, model = 'pooling'))

summary(lm(fatal_rate ~ beertax, data = Fatalities))
summary(plm(fatal_rate ~ beertax, data = Fatalities, model = 'pooling'))
# This is same as plm with pool

summary(lm(fatal_rate ~ beertax + year, data = Fatalities))
summary(plm(fatal_rate ~ beertax + year, data = Fatalities, model = 'pooling'))




# Fixed Effects -----------------------------------------------------------


# Allows for individual differences (different intercepts)
model_fe <- plm(
  fatal_rate ~ beertax,
  data = Fatalities,
  # index = c('state', 'year'),
  model = 'within'
)
summary(model_fe)
# Demeaned

# summary(lm(fatal_rate ~ beertax + state - 1, data = Fatalities))

model_fe <- plm(
  fatal_rate ~ beertax + year,
  data = Fatalities,
  # index = c('state', 'year'),
  model = 'within'
)
summary(model_fe)
#

model_fe <- plm(
  fatal_rate ~ beertax + year,
  data = Fatalities,
  index = c('state'),
  model = 'within'
)
summary(model_fe)




# Test --------------------------------------------------------------------



pool <- plm(fatal_rate ~ beertax, data = Fatalities, model = 'pooling')
within <- plm(fatal_rate ~ beertax, data = Fatalities, model = 'within')
fd <- plm(fatal_rate ~ beertax, data = Fatalities, model = 'fd')
summary(pool)
summary(within)
summary(fd)
coeftest(pool_test, vcov. = vcovHC, type = "HC1")



