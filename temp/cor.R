# Matrix ------------------------------------------------------------------


pacman::p_load(
  dplyr,
  GGally,
  Hmisc,
  flextable
)


# Use iris dataset, keep only numeric vars
df <- select(iris, -Species)
str(df)

# Get cor matrix, pull out cor coefficients and p value matrices
# Also do some formatting
(cor <- Hmisc::rcorr(as.matrix(df)))
(r <- format(round(cor$r, 3), nsmall = 3))
(p <- format(round(cor$P, 3), nsmall = 3))

# Add significance stars to p values if you like
(p <- paste0(p, ggstats::signif_stars(p)))

# Make new matrix by pasting together r and p
(mat <- matrix(paste0(r,  '\n(', p, ')'), nrow = nrow(r)))
# The \n is a line break, so the p value will be below the correlation.
# Flextable formatting should recognize it below. Remove this if not needed

# Remove lower triangle and diagonal
mat[lower.tri(mat, diag = TRUE)] <- ''

# Turn back into df
(mat <- as.data.frame(mat))

# Put row and column names back
colnames(mat) <- names(df)
mat <- mutate(mat, ' ' = names(df), .before = 1)

# Display as flextable to put into word
flextable::flextable(mat)



# ggpairs -----------------------------------------------------------------


# If plotting is on the table, ggpairs is also pretty convenient
GGally::ggpairs(df)

# Can keep just one triangle
GGally::ggpairs(
  df,
  lower = NULL,
  diag = NULL
)

# Also has capacity for custom functions for upper, lower, and diagonals, so
# you can customize it how you like

