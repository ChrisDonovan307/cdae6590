# Lab 3 Test
# 2025-01-31

# Load survey .rds file from GitHub
github_url <- 'https://raw.githubusercontent.com/ChrisDonovan307/cdae6590/refs/heads/main/surveys/clean_survey.rds'
download.file(github_url, 'clean_survey.rds', method = 'curl')
df <- readRDS('clean_survey.rds')

# Check df
str(df)
