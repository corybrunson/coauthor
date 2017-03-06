### Variable comparison using AIC

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/measChoice.RData")
load("calc/repDat.RData")
load("calc/baseChoice.RData")

for (pkg in c("MASS", "glmnet", "lme4")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 9, digits = 8)


## Partition dataset

library(caret)
# Training set with the highest typical number of articles per journal-year
seed <- 2795
minreps <- c()
repeat {
    set.seed(seed)
    newTrain <- createDataPartition(repDat$coau, p = train_p_aic, list = FALSE)
    seed <- seed + 1
    minrep <- min(table(repDat[newTrain[, 1], .(journal, floor(time))]))
    print(c(seed, minrep))
    if (minrep > max(0, minreps)) inTrainImp <- newTrain
    minreps <- c(minreps, minrep)
    if (length(minreps) == 20) break
}


## Data frame for each combination of terms

# Terms
vars <- setdiff(names(repDat), c("journal", "coau", "obs", "time"))
ints <- paste0(vars, ":time")
terms1 <- c(vars, "(1 | journal)")
terms2 <- c(paste(vars, "+", ints), "(1 + time | journal)")
cols <- rbind(c(vars, "1|journal"), c(ints, "time|journal"))

# Data table for each set of contingent terms
mats <- lapply(1:ncol(cols), function(i) {
    mat <- cbind(
        rep(c(FALSE, TRUE, TRUE),
            each = 3 ^ (ncol(cols) - i), times = 3 ^ (i - 1)),
        rep(c(FALSE, FALSE, TRUE),
            each = 3 ^ (ncol(cols) - i), times = 3 ^ (i - 1))
    )
    colnames(mat) <- cols[, i]
    mat
})
# Data table for storing AICs
termMat <- do.call(cbind, mats)

# Load any previous data; otherwise start fresh
if (file.exists("calc/aic.RData")) {
    load("calc/aic.RData")
    start <- max(which(aicMat[, 2] == 0))
    # Restore partition
} else {
    sysMat <- matrix(0, nrow = nrow(termMat), ncol = 3)
    aicMat <- matrix(0, nrow = nrow(termMat), ncol = 2)
    start <- nrow(termMat)
}


## Training and testing sets

trainDat <- repDat[inTrainImp[, 1]]
testDat <- repDat[-inTrainImp[, 1]]

rm(repDat)


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


# Fit models and calculate AICs
for (i in start:1) {
    # Print i
    print(i)
    
    # Write formula
    termIncl <- c("1",
                  terms1[termMat[i, 2 * 1:ncol(cols) - 1] &
                             !termMat[i, 2 * 1:ncol(cols)]],
                  terms2[termMat[i, 2 * 1:ncol(cols)]])
    form <- paste("coau ~", paste(termIncl, collapse = " + "))
    
    # Fit model
    sysMat[i, ] <- system.time(
        fit <- if (any(termMat[i, grep("\\|journal", colnames(termMat))])) {
            glmer.nb(formula = form, data = trainDat)
        } else {
            glm.nb(formula = form, data = trainDat)
        }
    )[1:3]
    
    # Extract AIC
    aicMat[i, ] <- extractAIC(fit)
    
    # Save every 3 models or so
    if ((nrow(termMat) + 1 - i) %% 3 == 0) {
        save(inTrainImp, termMat, aicMat, sysMat,
             file = "calc/aic.RData")
    }
}


## Calculate importance of each term

# Akaike weights
likVec <- exp(-(aicMat[, 2] - min(aicMat[, 2])) / 2)
akaikeVec <- likVec / sum(likVec)

# Calculate predictor (term) weights
termVec <- apply(termMat, 2, function(x) sum(akaikeVec[x]))
names(termVec) <- colnames(termMat)


## Save

save(inTrainImp, termMat, aicMat, sysMat, termVec, file = "calc/aic.RData")

rm(list = ls())
