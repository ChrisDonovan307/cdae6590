# Lab 10 - Panel Data 2


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  AER,
  plm,
  tidyr,
  ggplot2
)

options(scipen = 999)




# -------------------------------------------------------------------------


data(Crime)
str(Crime)

summary(lm(crmrte ~ taxpc + density + avgsen, data = Crime))

# pool test
plm::pooltest(fatal_rate ~ beertax, data = Fatalities, model = 'pooling')

plm::pooltest(crmrte ~ taxpc + density + avgsen, data = Crime, model = 'pooling')
plm::pooltest(crmrte ~ taxpc + density + avgsen, data = Crime, model = 'within')

# Compare to models
pooling <- plm::plm(
  crmrte ~ taxpc + density + avgsen,
  data = Crime,
  model = 'pooling'
)

within <- plm::plm(
  crmrte ~ taxpc + density + avgsen,
  data = Crime,
  model = 'within'
)

# Compare to manual demeaning
demean <- Crime %>%
  group_by(county) %>%
  mutate(mean_crime = mean(crmrte),
         mean_prob = mean(prbarr)) %>%
  mutate(demeaned_crime = crmrte - mean_crime,
         demeaned_prob = prbarr - mean_prob)
str(demean)

# Do it both ways, will be the same
orig_data <- plm::plm(crmrte ~ prbarr, data = Crime, model = 'within')
de_mean <- plm::plm(demeaned_crime ~ demeaned_prob, data = demean)
summary(orig_data)
summary(de_mean)
# An increase of prob of arrest by 100% >> crime per person drops by 0.002

plm::pooltest(pooling, within)
plm::pooltest(within, pooling)


# Compare to LSDV - lose this
summary(plm::plm(crmrte ~ prbarr + county, data = Crime))
summary(plm::plm(crmrte ~ prbarr + factor(county), data = Crime))
# Same? why doesnt it chagne DF and R2









data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
gasw <- plm(form, data = Gasoline, model = "within")
gasp <- plm(form, data = Gasoline, model = "pooling")
gasnp <- pvcm(form, data = Gasoline, model = "within")

# First two identical to last two
pooltest(gasw, gasnp)
pooltest(gasp, gasnp)

pooltest(form, data = Gasoline, effect = "individual", model = "within")
pooltest(form, data = Gasoline, effect = "individual", model = "pooling")



# Process -----------------------------------------------------------------


data(crime4, package = "wooldridge")
crime = crime4 %>%
  filter(county %in% c(1, 3, 145, 23), prbarr < 0.5)

# Points only
css %>%
  ggplot(aes(x = prbarr, y = crmrte, color = factor(county))) +
    geom_point(size = 3) +
    theme_classic() +
    labs(
      x = 'Probability of Arrest',
      y = 'Crime Rate',
      color = 'County'
    )



## Pooling -----------------------------------------------------------------


pooling <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'pooling'
)
summary(pooling)

crime %>%
  ggplot(aes(x = prbarr, y = crmrte)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() +
    labs(x = 'Probability of Arrest', y = 'Crime Rate')




## First Difference --------------------------------------------------------


difference <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'fd'
)
summary(difference)



## Fixed Effects -----------------------------------------------------------


# Fixed effects model
within <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'within'
)
summary(within)

# 'Within' effects in graph
crime %>%
  ggplot(aes(x = prbarr, y = crmrte, group = county, color = factor(county))) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() +
    labs(
      x = 'Probability of Arrest',
      y = 'Crime Rate',
      color = 'County'
    )
# Show animation


## Test whether there are fixed effects
plm::pFtest(within, pooling)
# plm::pooltest(pooling, within)
# null: no individual effects


## Test whether there is serial correlation of idiosyncratic errors
# Help decide whether to use FD or FE
pbgtest(crmrte ~ prbarr, data = crime)
# null: no serial correlation present



## Random Effects ----------------------------------------------------------


random <- plm::plm(
  crmrte ~ prbarr,
  data = crime,
  model = 'random'
)
summary(random)


## Hausman test - compare fixed effects to random
phtest(within, random)
# Null: x_it uncorrelated with alpha_i
# If significant, reject, there are unobserved effects, use FE
# If not significant, fail to reject, assume no unobserved effects, use RE
