# Lab 7 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  AER,
  performance,
  lmtest,
  skedastic,
  flextable,
  stargazer,
  writexl,
  broom
)

options(scipen = 999)



# WLS ---------------------------------------------------------------------


# Data from Food Systems Countdown Initiative

# Paper:
# https://www.nature.com/articles/s43016-024-01109-4#Sec12

# GitHub Repository
# https://github.com/KateSchneider-FoodPol/FSCI_2024Interactions_Replication

# Load data from Figure 1 - trends over time
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/datasets/fsci_data.rds'
con <- gzcon(url(github_url, 'rb'))
fsci <- readRDS(con)
close(con)

# Explore dataset
str(fsci)
# Note that we have many countries, many years, many variables

# Explore all our variables
unique(fsci$short_label)

# Filter to a single variable - undernourishment
df <- fsci[fsci$short_label == 'Prevalence of undernourishment', ]
str(df)


## Unweighted
lm <- lm(normvalue ~ year + FSCI_region, data = df)
summary(lm)
# Reference group is Central Asia

# Check model
performance::check_model(lm)
lmtest::bptest(lm)
skedastic::breusch_pagan(lm)
skedastic::white(lm)

## Weighted
lm_wls <- lm(normvalue ~ year + FSCI_region, data = df, weights = weight)
summary(lm_wls)

# Check model
performance::check_model(lm_wls)
skedastic::breusch_pagan(lm_wls)
skedastic::white(lm_wls)



# Exporting Results -------------------------------------------------------


# First convert our regression output into a data frame
wls_df <- broom::tidy(lm_wls) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
wls_df


## To CSV
write.csv(wls_df, file = 'csv_wls.csv')


## To Excel
writexl::write_xlsx(wls_df, path = 'excel_wls.xlsx')


## To Word
flex_wls <- flextable::flextable(wls_df)
flextable::save_as_docx(flex_wls, path = 'flex_wls.docx')

# Add a caption and a footnote, fix formatting
my_caption <- as_paragraph('Table 2. This is our wicked caption for our table.')
my_footnote <- as_paragraph('This is a footnote where we explain some things.')

flex_wls_clean <- flex_wls %>%
  flextable::set_caption(caption = my_caption) %>%
  flextable::footnote(i = 1, j = 1, value = my_footnote) %>%
  flextable::autofit()

flextable::save_as_docx(flex_wls_clean, path = 'flex_wls_clean.docx')

# Save in APA format
flex_wls_apa <- theme_apa(flex_wls_clean)
flextable::save_as_docx(flex_wls_apa, path = 'flex_wls_apa.docx')


## To LateX
stargazer::stargazer(
  lm_wls,
  type = 'latex',
  out = 'latex_wls.tex',
  notes = 'These are the notes.'
)
# Note that stargazer takes the model itself as an input, not the DF
