### Multiple NB regression with time interactions and journal random effects

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/measChoice.RData")
load("calc/repDat.RData")
load("calc/inTrain.RData")
load("calc/baseChoice.RData")

for (pkg in c("rstanarm")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
source("code/priors-draft.r")
options(scipen = 9, digits = 8)


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


## Single-confounder models

vars <- setdiff(names(trainDat), c("journal", "coau", "obs", "time"))
dats <- list()
for (var in vars) {
    fit <- stan_glm.nb(
        paste0("coau ~ time + ", var, " + ", var, ":time"),
        data = trainDat,
        prior_intercept = int_prior,
        prior = var_priors[[var]]
    )
    sum <- summary(fit)
    # Assess predictions (MSE)
    pred <- predict(object = fit, newdata = trainDat, type = "response")
    train.mse <- mean((trainDat$coau - pred) ^ 2)
    pred <- predict(object = fit, newdata = testDat, type = "response")
    test.mse <- mean((testDat$coau - pred) ^ 2)
    # Store
    dats <- c(dats, list(data.table(Var = rownames(coef(sum))[-(1:2)],
                                    Est = coef(sum)[-(1:2), 1],
                                    SE = coef(sum)[-(1:2), 2],
                                    Dev = unname(sum$deviance),
                                    DfRes = unname(sum$df.residual),
                                    AIC = sum$aic,
                                    TrainMSE = train.mse,
                                    TestMSE = test.mse)))
}
fitDat0 <- rbindlist(dats)
save(fitDat0, file = paste0("calc/fitBayes0.RData"))


## Fixed effects models

vars <- setdiff(names(trainDat), c("journal", "coau", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
fit <- stan_glm.nb(
    paste("coau ~", paste(c(vars, ints), collapse = " + ")),
    data = trainDat,
    prior_intercept = int_prior,
    prior = var_priors[[vars]]
)
sum <- summary(fit)
# Assess predictions (MSE)
pred <- predict(object = fit, newdata = trainDat, type = "response")
train.mse <- mean((trainDat$coau - pred) ^ 2)
pred <- predict(object = fit, newdata = testDat, type = "response")
test.mse <- mean((testDat$coau - pred) ^ 2)
# Save!
fitDat1 <- data.table(Var = rownames(coef(sum))[-1],
                        Est = coef(sum)[-1, 1],
                        SE = coef(sum)[-1, 2],
                        Dev = unname(sum$deviance),
                        DfRes = unname(sum$df.residual),
                        AIC = sum$aic,
                        TrainMSE = train.mse,
                        TestMSE = test.mse)
save(fitDat1, file = paste0("calc/fitBayes1.RData"))


## Random intercept & time slope models

vars <- setdiff(names(trainDat), c("journal", "coau", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")
fit <- stan_glmer.nb(
    paste("coau ~",
          paste(c(vars, ints), collapse = " + "), "+ (time | journal)"),
    data = trainDat,
    prior_intercept = int_prior, prior = var_priors[[vars]]
)
sum <- summary(fit)
# Assess predictions (MSE)
pred <- predict(object = fit, newdata = trainDat, type = "response")
train.mse <- mean((trainDat$coau - pred) ^ 2)
pred <- predict(object = fit, newdata = testDat, type = "response")
test.mse <- mean((testDat$coau - pred) ^ 2)
# Save!
fitDat2 <- data.table(Var = rownames(coef(sum))[-1],
                        Est = coef(sum)[-1, 1],
                        SE = coef(sum)[-1, 2],
                        Dev = unname(sum$AICtab["deviance"]),
                        DfRes = unname(sum$AICtab["df.resid"]),
                        AIC = sum$AICtab["AIC"],
                        TrainMSE = train.mse,
                        TestMSE = test.mse)
ranefDat2 <- as.data.table(ranef(fit)$journal)
save(fitDat2, ranefDat2, file = paste0("calc/fitBayes2.RData"))


rm(list = ls())
