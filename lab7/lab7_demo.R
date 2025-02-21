# Lab 7 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  AER,
  MASS,
  gapminder,
  performance,
  ggplot2
)

get_res_plots <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
}

# data(Boston)
# dat <- Boston
# get_str(dat)

dat <- gapminder



# Explore -----------------------------------------------------------------


get_str(dat)
lm <- lm(lifeExp ~ log(gdpPercap) + pop + year + continent, data = dat)
lm <- lm(lifeExp ~ gdpPercap + year + continent, data = dat)
summary(lm)
get_res_plots(lm)
performance::check_model(lm)
lmtest::bptest(lm)

# WLS
lm2 <- lm(lifeExp ~ gdpPercap + year + continent, data = dat, weights = 1/pop)
lm2 <- lm(lifeExp ~ log(gdpPercap) + year + continent, data = dat, weights = 1/pop)
summary(lm2)
get_res_plots(lm2)
performance::check_model(lm2)
lmtest::bptest(lm2)

dat %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(size = 2) +
  theme_classic()

dat %>%
  filter(year == 2007) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(size = 2) +
  theme_classic()

# -------------------------------------------------------------------------


HousePrices
data(HousePrices)
dat <- HousePrices
get_str(dat)

lm <- lm(price ~ lotsize + bedrooms + bathrooms + garage, data = dat)
lm <- lm(lotsize ~ price + stories + bedrooms + garage, data = dat)
summary(lm)
get_res_plots(lm)
performance::check_model(lm)
bptest(lm)

summary(lm, robust = TRUE)
