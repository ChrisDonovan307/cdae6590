# Lab 7 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  AER,
  performance,
  lmtest,
  flextable,
  stargazer,
  writexl,
  broom,
  gapminder,
  ggplot2
)

options(scipen = 999)



# Weighted Least Squares (WLS) --------------------------------------------


# Dataset from the Food Systems Countdown Initiative
# Paper: https://www.nature.com/articles/s43016-024-01109-4#Sec12

# GitHub repository:
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

# Remove ampersands from out region names (problematic in some cases)
unique(df$FSCI_region)
df$FSCI_region <- gsub('&', 'and', df$FSCI_region)
unique(df$FSCI_region)


## Unweighted regression
lm <- lm(normvalue ~ year + FSCI_region, data = df)
summary(lm)
# Reference group is Central Asia

# Check model
performance::check_model(lm)
lmtest::bptest(lm)

## Weighted
lm_wls <- lm(normvalue ~ year + FSCI_region, data = df, weights = weight)
summary(lm_wls)

# Check model
performance::check_model(lm_wls)
lmtest::bptest(lm)



# Exporting Regression Outputs --------------------------------------------


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
  notes = 'These are the notes.',
  font.size = 'footnotesize',
  covariate.labels = gsub("FSCI_region", "", names(coef(lm_wls)))
)
# Note that stargazer takes the model itself as an input, not the DF



# Exporting Plots ---------------------------------------------------------


# Filter gapminder to the year 2007 only
gapminder_2007 <- gapminder[gapminder$year == 2007, ]


## Snipping
# For a quick and dirty plot, you can just use your computer's snip feature!


## Export with GUI
# Click on the Export button toward the top left of the plot window


## Save base plot to png
# Define png output
png(
  filename = 'gapminder_plot.png',
  width = 600,
  height = 500,
  units = 'px',
  res = 72
)

# Create plot
plot(
  x = gapminder_2007$gdpPercap,
  y = gapminder_2007$lifeExp,
  col = gapminder_2007$continent,
  pch = 16,
  cex = sqrt(gapminder_2007$pop) / 10000,
  ylab = 'Life Expectancy',
  xlab = 'GDP per Capita',
  main = 'Life Expectancy against GDP per Capita (2007)'
)

# Add a legend to existing plot
legend(
  "bottomright",
  legend = levels(gapminder_2007$continent),
  col = 1:5,
  pch = 16,
  title = "Continent"
)

# Turn off graphing device
dev.off()


## Save ggplot2 to png
# Save a plot to an object
gapminder_plot <- gapminder %>%
  dplyr::filter(year == 2007) %>%
  ggplot2::ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  theme_classic() +
  labs(
    x = 'GDP per Capita',
    y = 'Life Expectancy',
    title = 'Life Expectancy against GDP per Capita (2007)'
  )

# Print it to check it out (not required for saving it)
gapminder_plot

# Save the plot object to a .png file
ggsave(
  filename = 'gapminder_ggplot2.png',
  plot = gapminder_plot,
  width = 600,
  height = 500,
  units = 'px',
  dpi = 300
)
