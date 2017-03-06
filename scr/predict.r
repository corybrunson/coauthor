### Assessment of predictive power of fitted models

# Source base code & choices
source("code/base.r")
source("code/choices.r")

# Load original data
load("calc/repDat.RData")
# Load measurement transforms
load("calc/measChoice.RData")
# Load baseline choices
load("calc/baseChoice.RData")
# Load training indices
load("calc/inTrain.RData")

for (pkg in c("MASS", "glmnet", "lme4")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 9, digits = 8)

# Single-confounder models
load("calc/fitFreq0.RData")
# Multivariate fixed-effects model with interactions
load("calc/fitFreq1.RData")
fit1 <- fit
# Multivariate model with interactions and random effects
load("calc/fitFreq2.RData")
fit2 <- fit
rm(fit)

# Make cheap plots directory
if (!file.exists("fig/res")) {
    dir.create("fig/res")
}


## Predictions table for article-level variables

# Prestige across journal-years
prestigeDat <- unique(repDat[, .(journal, time, prestige)])
# Quantile values
q.prestige <- exp(quantile(prestigeDat$prestige, probs = c(.15, .5, .85)))
print(q.prestige)
# One standard deviation removed
s.prestige <- exp(mean(prestigeDat$prestige) +
                    c(-1, 0, 1) * sd(prestigeDat$prestige))
names(s.prestige) <- c(-1, 0, 1)
print(s.prestige)

# Convert data to model form
stdFn <- function(dat) {
    # Transform
    for (v in intersect(names(transforms), names(dat))) {
        fun <- transforms[[v]][[1]]
        dat[[v]] <- fun(dat[[v]])
    }
    # Center
    for (v in intersect(names(centers), names(dat))) {
        dat[[v]] <- dat[[v]] - centers[v]
    }
    # Scale
    for (v in intersect(names(scales), names(dat))) {
        dat[[v]] <- dat[[v]] / scales[v]
    }
    # Return
    dat
}
# Predict for new data
predFn <- function(dat) {
    # Convert to usable form
    dat <- stdFn(dat)
    # Model 1 coauthor count predictions
    pred1 <- predict(object = fit1, newdata = dat, type = "response")
    # Model 2 coauthor count predictions
    pred2 <- predict(object = fit2, newdata = dat, type = "response",
                     re.form = NA)
    # Data frame of author count (NOT coauthor count) predictions
    pred <- data.frame("Fixed" = pred1 + 1, "Mixed" = pred2 + 1)
    # Return
    cbind(dat, pred)
}

# Sample author count predictions for discussion
dat <- data.frame(trial = rep(c(F, T, F), each = 8),
                  review = rep(c(F, F, T), each = 8),
                  breadth = rep(c(2, 6), each = 4, times = 3),
                  funding = rep(c(F, T), each = 2, times = 6),
                  prestige = rep(s.prestige[c(1, 3)], times = 12))
if (incl_indicator == TRUE) {
  dat <- rbind(dat, dat)
  dat$viz <- rep(c(F, T), each = nrow(dat) / 2)
}
predDat <- do.call(cbind, lapply(c(1999, 2007), function(yr) {
    predFn(cbind(time = yr, dat))[, 7:8]
}))
print(round(t(cbind(dat, predDat)), 1))


rm(list = ls())
