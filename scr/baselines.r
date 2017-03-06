### Transform regressors in order to linearize relationships with author count

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/subDat.RData")
load("calc/measChoice.RData")

for (pkg in c("caret")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}

options(scipen = 9, digits = 8)


## Prep

# Construct dataset
stopifnot(length(measures) == length(transforms))
repDat <- subDat[, .(issn)]
setnames(repDat, "issn", "journal")
for (j in 1:length(measures)) {
    fun <- transforms[[j]][[1]]
    repDat[, field := fun(subDat[, measures[[j]][[1]], with = FALSE])]
    setnames(repDat, "field", names(measures)[[j]][[1]])
}
if (incl_indicator) repDat[is.na(prestige)]$prestige <- 0

# Numericize time variable
repDat$time <- as.numeric(repDat$time)

# Save
save(repDat, file = "calc/repDat.RData")


## Partition dataset

# Training set with the highest typical number of articles per journal-year
seed <- 2795
minreps <- c()
repeat {
    set.seed(seed)
    newTrain <- createDataPartition(repDat$coau, p = train_p, list = FALSE)
    seed <- seed + 1
    minrep <- min(table(repDat[newTrain[, 1], .(journal, floor(time))]))
    print(c(seed, minrep))
    if (minrep > max(0, minreps)) inTrain <- newTrain
    minreps <- c(minreps, minrep)
    if (length(minreps) == 20) break
}

# Save!
save(inTrain, file = "calc/inTrain.RData")

load("calc/inTrain.RData")
## Standardize

# Training set
trainDat <- repDat[inTrain[, 1]]

# Center
centers <- c(
    time = as.numeric(median(unique(floor(trainDat$time)))),
    funding = 0, review = 0, trial = 0,
    breadth = median(trainDat$breadth),
    prestige = mean(unique(trainDat[, .(journal, time, prestige)])$prestige,
                    na.rm = TRUE)
)
if (incl_indicator) centers <- c(centers, viz = 0)

# Scale
scales <- c(
    # Time: next-to-first and next-to-last years
    time = 1,
    # Funding, review, trial: Treat as binary
    funding = 1, review = 1, trial = 1,
    # Topical scope: 2 standard deviations
    breadth = 2 * sd(trainDat$breadth),
    prestige = 2 * sd(unique(trainDat[, .(journal, prestige)])$prestige)
)
if (incl_indicator) scales <- c(scales, viz = 1)

# Characteristic extreme parameter values
extremes <- list(
    funding = 0:1, review = 0:1, trial = 0:1,
    breadth = centers["breadth"] + c(-1, 1) * .5 * scales["breadth"],
    prestige = centers["prestige"] + c(-1, 1) * .5 * scales["prestige"]
)
if (incl_indicator) extremes <- c(extremes, list(viz = 0:1))

save(centers, scales, extremes, file = "calc/baseChoice.RData")


rm(list = ls())
