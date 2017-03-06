## Declare choices for the sensitivity analysis

# Exclude articles (co-)written by corporate authors
excl_corporate <- FALSE

# Use longer window of Web of Science data instead of intersection with SCImago
wos_window <- FALSE

# Include an indicator variable for membership in the bibliometrics dataset
incl_indicator <- FALSE

# Proportion of dataset used to fit models
train_p <- .25
# Proportion of dataset used to test importance
train_p_aic <- 1/3
