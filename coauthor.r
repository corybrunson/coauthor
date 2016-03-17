### Scripts to reproduce the analysis of coauthorship in biomedical research


## Preparation

# Check current directiory
if (!grepl("/coauthor$", getwd())) stop("Go to 'coauthor' directory")
# Check data files
stopifnot(file.exists(paste0("calc/",
                             c("pmDat", "wosDat", "sjrDat"),
                             ".RData")))

# Create directories for figures and tables
dirs <- c("fig", "tab")
for(d in dirs) if (!file.exists(d)) dir.create(d)


## Run analyses

# Clean the data extracted from PubMed (not supplied)
#source("scr/clean.r")

# Download and clean data from Web of Science and SCImago
source("scr/metrics.r")

# Merge the PubMed data with the Web of Science and SCImago data
source("scr/merge.r")

# Subset the data according to inclusion/exclusion criteria
source("scr/subset.r")

# Construct co-occurrence matrices for MeSH terms (not used)
#source("scr/cooccurence.r")

source("scr/transform.R")

source("scr/summary.R")

source("scr/baselines.R")

source("scr/select.R")

source("scr/univariate.R")

source("scr/lasso.R")

source("scr/multivariate.R")

source("scr/multislope.R")

source("scr/interact.R")

source("scr/results.R")

source("scr/importance.R")

source("scr/shrinkage.R")

source("scr/predict.R")

source("scr/sensitivity.R")

source("scr/subjects.R")

source("scr/plots.R")
