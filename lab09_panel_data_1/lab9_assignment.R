# Lab 9 - Panel Data 1


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  plm,
  tidyr
)

options(scipen = 999)

data(Cigar, package = 'plm')
str(Cigar)
?plm::Cigar



# Assignment --------------------------------------------------------------


## 1. Reshaping data

# a. Convert the Cigar df into 'long' format and save it to a new object. The
# first two columns should be country and year, and everything else should be
# pivoted longer.


# b. Convert your 'long' Cigar df back into a 'wide' df. It should be the same
# as the original Cigar data.



## 2. Use a first differenced model to explore the effect cigarette prices on
# cigarette sales (sales ~ price), and interpret the results. You can use either
# the manual method with lm() or the plm() method. Some set up is done for you
# below.

# Convert Cigar to pdata.frame
Cigar <- plm::pdata.frame(Cigar)
str(Cigar)

# Set up two separate DFs for different years
Cigar_91 <- Cigar[Cigar$year == '91', ]
Cigar_92 <- Cigar[Cigar$year == '92', ]

# Set up a single DF with both years
Cigar_91_92 <- Cigar[Cigar$year %in% c('91', '92'), ]



## 3. Run a fixed effect model with plm() testing sales ~ price on the whole
# Cigar dataset. Interpret the output. Feel free to add other covariates if you
# like.

