# Lab 11 Assignment


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  ivreg
)

data('Kmenta', package = 'ivreg')
str(Kmenta)
?Kmenta

# Demand and supply equations
deq <- ivreg::ivreg(Q ~ P + D | D + F + A, data = Kmenta)
seq <- ivreg::ivreg(Q ~ P + F + A | D + F + A, data = Kmenta)
summary(deq)
summary(seq)

