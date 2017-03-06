### Scripts to reproduce the analysis of coauthorship in biomedical research


## Preparation

setwd("PubMed/proj/coauthor")
# Check current directiory
if (!grepl("/coauthor$", getwd())) stop("Go to 'coauthor' directory")
# Check data files
stopifnot(file.exists(paste0("calc/",
                             c("pmDat", "wosDat", "sjrDat", "jids"),
                             ".RData")))

# Create directories for figures and tables
dirs <- c("fig", "tab")
for(d in dirs) if (!file.exists(d)) dir.create(d)


## Run analyses

# Clean the data extracted from PubMed (not supplied)
#source("scr/clean.r")

# Download and clean data from Web of Science and SCImago
#source("scr/metrics.r")

# Read the file "J_Entrez" as a data.table
#source("scr/jid.r")

# Merge the PubMed data with the Web of Science and SCImago data
source("scr/merge.r")

# Subset the data according to inclusion/exclusion criteria
source("scr/subset.r")

# Construct co-occurrence matrices for MeSH terms (not used)
#source("scr/cooccurence.r")

# Transform regressors in order to linearize relationships with author count
source("scr/transform.r")

# Summary statistics, tables, and figures
source("scr/summary.r")

# Transform regressors in order to linearize relationships with author count
source("scr/baselines.r")

# Model goodness-of-fit comparisons (comparison to lognormal is inappropriate)
source("scr/goodness.r")

# Multiple NB regression with time interactions and journal random effects
source("scr/fit.r")

# Residuals analysis
source("scr/resid.r")

# Effect estimates
source("scr/effects.r")

# Assessment of predictive power of fitted models
source("scr/predict.r")

# Variable comparison using AIC (DEFAULT OPTIONS ONLY)
source("scr/importance.r")

# Re-fit models for each choice of variable measure
# (NOT TESTED WITH incl_indicator == TRUE)
source("scr/sensitivity.r")

# Subject classification-specific regression analyses
# (NOT TESTED WITH incl_indicator == TRUE)
source("scr/classifications.r")

# Subject summary and models with subject indicator variables (INTRACTABLE)
#source("scr/subjects.r")

# Fit and review lognormal models
source("scr/fit-lognormal.r")
source("scr/effects-lognormal.r")

# Generate elegant plots
source("scr/plots.r")

# Generate tables
source("scr/tables.r")
