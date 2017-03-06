### Model selection process

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/measChoice.RData")
load("calc/repDat.RData")
load("calc/inTrain.RData")
load("calc/baseChoice.RData")

for(pkg in c("MASS", "glmnet", "lme4")) {
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


## Model selection

# Fit evaluations
evalFitGLMM <- function(fit) {
  sum <- summary(fit)
  pred <- predict(object = fit, newdata = trainDat, type = "response")
  train.mse <- mean((trainDat$coau - pred) ^ 2)
  pred <- predict(object = fit, newdata = testDat, type = "response")
  test.mse <- mean((testDat$coau - pred) ^ 2)
  data.frame(Dev = deviance(fit),
             DfRes = df.residual(fit),
             AIC = AIC(fit),
             TrainMSE = train.mse,
             TestMSE = test.mse)
}
evalFitLN <- function(fit) {
  sum <- summary(fit)
  pred <- predict(object = fit, newdata = trainDat)
  train.mse <- mean((trainDat$coau1log - pred) ^ 2)
  pred <- predict(object = fit, newdata = testDat)
  test.mse <- mean((testDat$coau1log - pred) ^ 2)
  data.frame(Dev = deviance(fit),
             DfRes = df.residual(fit),
             AIC = AIC(fit),
             TrainMSE = train.mse,
             TestMSE = test.mse)
}
evalFitLNinv <- function(fit) {
  sum <- summary(fit)
  pred <- exp(predict(object = fit)) - 1
  train.mse <- mean((trainDat$coau - pred) ^ 2)
  pred <- exp(predict(object = fit, newdata = testDat)) - 1
  test.mse <- mean((testDat$coau - pred) ^ 2)
  data.frame(Dev = sum((trainDat$coau - pred) ^ 2),
             DfRes = df.residual(fit),
             AIC = NA,
             TrainMSE = train.mse,
             TestMSE = test.mse)
}

# Initialize model data
modDat <- data.frame()

# Model terms
vars <- setdiff(names(trainDat), c("journal", "coau", "obs"))
ints <- paste0(setdiff(vars, "time"), ":time")

# Poisson with log link
fit <- glm(paste("coau ~", paste(c(vars, ints), collapse = " + ")),
           data = trainDat, family = poisson(link = log))
modDat <- rbind(modDat,
                data.frame(Family = "Poisson", evalFitGLMM(fit)))

# Negative binomial with log link
fit <- glm.nb(paste("coau ~", paste(c(vars, ints), collapse = " + ")),
              data = trainDat, link = log)
modDat <- rbind(modDat,
                data.frame(Family = "NegBin", evalFitGLMM(fit)))

# Lognormal
fit <- lm(paste("coau1log ~", paste(c(vars, ints), collapse = " + ")),
          data = trainDat)
modDat <- rbind(modDat,
                data.frame(Family = "LogNorm", evalFitLN(fit)),
                data.frame(Family = "LogNormInv", evalFitLNinv(fit)))

# Print!
print(modDat)


rm(list = ls())
