### Calculate ambient MeSH co-occurrence rates

# Source base code & choices
source("code/base.R")
source("code/choices.R")

for (pkg in c("Matrix")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}

# Load cleaned data
load("calc/pmDat0.RData")


## Further cleaning

# SUBSET FOR EXPERIMENTAL PURPOSES
pmDat <- pmDat[sample(nrow(pmDat), size = 10000)]

# Calculate yearmon (taken from clean.r)
pmDat[, ms := substr(dp, 6, 8)]
pmDat[ms == "Aut"]$ms <- "Fal"
pmDat[!(ms %in% c(month.abb, c("Win", "Spr", "Sum", "Fal")))]$ms <- NA
pmDat[, ym := as.yearmon(paste(ms, yr))]
pmDat <- pmDat[!is.na(ym)]

# Split MeSH terms into a list of vectors
pmDat[, mesh := strsplit(mh, split = "\\|")]

# Subset to identifiers and MeSH terms
meshDat <- pmDat[, .(pmid, ym,
                     issn = gsub("^.*\\((.*)\\)$", "\\1", is),
                     mesh)]
rm(pmDat)

# Headings (with primacy indicators)
mesh_h1 <- function(x) gsub("^(.*)\\[.*\\]$", "\\1", x)
meshDat[, h1 := lapply(mesh, mesh_h1)]
# Subheadings (with primacy indicators)
mesh_h2 <- function(x) {
    y <- gsub("^.*\\[(.*)\\]$", "\\1", x[grep("^.*\\[.*\\]$", x)])
    unlist(strsplit(y, split = ":"))
}

meshDat[, h2 := lapply(mesh, mesh_h2)]
mesh_ph <- function(x) gsub("^(.*)\\(Y\\)$", "\\1", x[grep("\\(Y\\)$", x)])
# Primary headings
meshDat[, ph1 := lapply(h1, mesh_ph)]
# Primary subheadings (with duplicates)
meshDat[, ph2 := lapply(h2, mesh_ph)]

# Headings and subheadings (without primacy indicators)
mesh_h <- function(x) gsub("^(.*)\\((Y|N)\\)$", "\\1", x)
meshDat[, h1 := lapply(h1, mesh_h)]
meshDat[, h2 := lapply(h2, mesh_h)]

# Remove mesh field (to save space)
meshDat[, mesh := NULL]


## Article-level co-occurrence matrices

for (var in c("h1", "h2", "ph1", "ph2")) {
    
    # Data table for identifiers and this variable
    dat <- meshDat[, c("pmid", "ym", "issn", var), with = FALSE]
    # "Melt" variable field
    lens <- sapply(meshDat[[var]], length)
    vars <- unlist(meshDat[[var]])
    mdat <- data.table(pmid = rep(meshDat$pmid, lens),
                       ym = rep(meshDat$ym, lens),
                       issn = rep(meshDat$issn, lens),
                       var = vars)
    
    # Cast by variable and coerce to sparse matrix
    cmat <- Matrix(acast(mdat, var ~ pmid, value.var = "pmid", fun = length))
    # Co-occurrence matrix
    comat <- cmat %*% t(cmat)
    
    # Save!
    save(comat, file = paste0("calc/comat-", var, ".RData"))
}


rm(list = ls())
