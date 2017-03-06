### Multiple lognormal regression

# test whether being used on server
if (file.exists("~/PubMed")) setwd("~/PubMed/proj/coauthor/")

# Source base code & choices
source("code/base.r")
source("code/choices.r")
source("code/specs.r")

load("calc/measChoice.RData")
load("calc/repDat.RData")
load("calc/inTrain.RData")
load("calc/baseChoice.RData")

for (pkg in c("lme4")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 9, digits = 8)


## Transform coauthor counts

repDat[, coau1log := log(coau + 1)]


## Training and testing sets

trainDat <- repDat[inTrain[, 1]]
testDat <- repDat[-inTrain[, 1]]

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


## Fixed effects models

vars <- setdiff(names(trainDat), c("journal", "coau", "coau1log", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
fit <- lm(
    paste("coau1log ~", paste(c(vars, ints), collapse = " + ")),
    data = trainDat
)
# Save!
save(fit, file = "calc/fit1logFreq1.RData")

# Test for normality of errors
pdf(height = bodwid * .5, width = colwid,
    file = "fig/1logQqplot1.pdf")
step <- 1 / (length(resid(fit)) + 1)
qqplot(x = qnorm(seq(step, 1 - step, step)), y = resid(fit),
       pch = 1, cex = .5,
       xlab = "Normal quantiles",
       ylab = "Residuals",
       main = "Normal Q-Q plot: fixed-effects lognormal model")
grid(lty = 3)
dev.off()


## Random intercept & time slope models

vars <- setdiff(names(trainDat), c("journal", "coau", "coau1log", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
fit <- lmer(
    paste("coau1log ~",
          paste(c(vars, ints), collapse = " + "), "+ (time | journal)"),
    data = trainDat
)
# Save!
save(fit, file = "calc/fit1logFreq2.RData")

# Test for normality of errors
pdf(height = bodwid * .5, width = colwid,
    file = "fig/1logQqplot2.pdf")
step <- 1 / (length(resid(fit)) + 1)
qqplot(x = qnorm(seq(step, 1 - step, step)), y = resid(fit),
       pch = 16, cex = .5,
       xlab = "Normal quantiles",
       ylab = "Residuals",
       main = "Q-Q plot: mixed-effects lognormal model")
grid(lty = 3)
dev.off()


rm(list = ls())
