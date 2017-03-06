### Subject summary and models with subject indicator variables

# Source base code & choices
source("code/base.r")
source("code/choices.r")

for(pkg in c("MASS", "plyr", "lme4", "xtable", "data.table")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 99, digits = 8)


## Frequency table

# Load original data
load("calc/subDat.RData")

# Assign "" value to missing frequencies
subDat[is.na(freq)]$freq <- ""
# Order frequencies
subDat$freq <- factor(subDat$freq,
                      c("Quarterly", "Bimonthly", "Monthly",
                        "Semimonthly", "Biweekly", "Weekly", ""))

# Table of category frequencies across journals
catTab <- sort(table(unlist(unique(subDat[, .(issn, cats)]$cats))),
               decreasing = TRUE)


## Models with indicator variables

# Journal and categories only
firsts <- which(!duplicated(subDat[, .(issn)]))
catDat <- subDat[firsts, .(issn, cats)]
rm(subDat)

# Load regression data
load("calc/measChoice.RData")
load("calc/repDat.RData")
load("calc/inTrain.RData")
load("calc/baseChoice.RData")

# Merge categories into regression data by journal
repDat <- merge.data.frame(repDat, catDat,
                           by.x = "journal", by.y = "issn",
                           all.x = TRUE, all.y = FALSE)
# Which categories to include
wh <- which(catTab >= 5)
# Add indicator variables
for (i in wh) {
    print(names(catTab)[i])
    nm <- tolower(gsub("_&|,", "", gsub(" ", "_", names(catTab)[i])))
    repDat[[nm]] <- grepl(names(catTab)[i], repDat$cats)
}
repDat <- as.data.table(repDat)
repDat[, cats := NULL]


## Training and testing sets

trainDat <- repDat[inTrain[, 1]]
testDat <- repDat[-inTrain[, 1]]


## Standardize

# Center
for (v in names(centers)) {
    trainDat[[v]] <- trainDat[[v]] - centers[v]
    testDat[[v]] <- testDat[[v]] - centers[v]
}
# Scale
for (v in names(scales)) {
    trainDat[[v]] <- trainDat[[v]] / scales[v]
    testDat[[v]] <- testDat[[v]] / scales[v]
}


## Fixed effects models

vars <- setdiff(names(repDat), c("journal", "coau", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
cats <- tolower(gsub("_&|,", "", gsub(" ", "_", names(catTab)[wh])))
fit <- glm.nb(
    paste("coau ~", paste(c(vars, ints, cats), collapse = " + ")),
    data = trainDat
)
# Save!
save(fit, file = paste0("calc/fitFreqCats1.RData"))


## Random intercept & time slope models

vars <- setdiff(names(repDat), c("journal", "coau", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
cats <- tolower(gsub("_&|,", "", gsub(" ", "_", names(catTab)[wh])))
fit <- glmer.nb(
    paste("coau ~",
          paste(c(vars, ints, cats), collapse = " + "), "+ (time | journal)"),
    data = trainDat
)
save(fit, file = paste0("calc/fitFreqCats2.RData"))


## Random intercept, time slope, & category models

vars <- setdiff(names(repDat), c("journal", "coau", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
cats <- tolower(gsub("_&|,", "", gsub(" ", "_", names(catTab)[wh])))
fit <- glmer.nb(
    paste("coau ~",
          paste(c(vars, ints), collapse = " + "), "+ (time + ",
          paste(cats, collapse = " + "),
          " | journal)"),
    data = trainDat
)
# Save!
save(fit, file = paste0("calc/fitFreqCats3.RData"))


rm(list = ls())
