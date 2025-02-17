# CPSSW8

pacman::p_load(
  dplyr,
  AER,
  readr
)

# Load dataset from AER package. Note that data() function loads directly into
# the environment
data('CPSSW8')
str(CPSSW8)

# Sample 1000 rows out of the dataset. We're just doing this so it runs a little
# bit faster. Note that we need to set a seed for reproducibility.
set.seed(42)
indices <- sample(1:nrow(CPSSW8), size = 1000, replace = FALSE)
df <- CPSSW8[indices, ]
str(df)

# Also create population weights
pops <- table(df$region) %>%
  as.data.frame() %>%
  setNames(c('region', 'freq'))
df$weights <- case_when(
  df$region == 'Northeast' ~ floor(57609148 / pops$freq[pops$region == 'Northeast']),
  df$region == 'Midwest' ~ floor(68985454 / pops$freq[pops$region == 'Midwest']),
  df$region == 'South' ~ floor(126266107 / pops$freq[pops$region == 'South']),
  df$region == 'West' ~ floor(68985454 / pops$freq[pops$region == 'West'])
)
get_str(df)

# Save it in dataset folder
saveRDS(df, 'datasets/CPSSW8_mod.rds')
write_csv(df, 'datasets/CPSSW8_mod.csv')
