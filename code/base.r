## Setup

# Load packages from CRAN
pkgs <- c("devtools", "zoo", "ggplot2")
for(pkg in pkgs) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg, repos = 'http://cran.us.r-project.org')
        stopifnot(require(pkg, character.only = TRUE))
    }
}
# Load packages from GitHub
gh.pkgs <- c("pubmed.network")
for(pkg in gh.pkgs) {
    if(!require(pkg, character.only = TRUE)) {
        install_github(paste0("corybrunson/", pkg))
        stopifnot(require(pkg, character.only = TRUE))
    }
}

# Identify range of years and 3-year intervals
# (before electronic publishing takeover)
years <- 1982:2008  # omit sketchy earlier years
years <- setdiff(years, 2008)  # omit incomplete year
years <- setdiff(years, 1800:1990)  # 5 yrs before 1st yr w/ 25-author limit


## Transforms

# Anscombe transformation
anscombeFun <- function(x) 2 * sqrt(x + 3/8)
# Log+1 function
logPlus1 <- function(x) log(x + 1)
