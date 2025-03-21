# Lab 9 - Panel Data 1


# Housekeeping ------------------------------------------------------------


if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  plm
)

options(scipen = 999)

# Load Cigar dataset
data(Cigar, package = 'plm')
str(Cigar)

# Filter to 76-77
Cigar <- pdata.frame(Cigar)
Cigar_76_77 <- Cigar[Cigar$year %in% c('76', '77'), ]



# Assignment --------------------------------------------------------------


summary(lm(sales ~ price + ndi, data = Cigar))

Cigar <- pdata.frame(Cigar)
str(Cigar)

summary(lm(sales ~ state + year + price, data = Cigar))
summary(plm(sales ~ state + year + price, data = Cigar, model = 'within'))


