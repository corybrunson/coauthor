### Effect estimates

# Source base code & choices
source("code/base.r")
source("code/choices.r")

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


## Single-confounder models

vars <- setdiff(names(trainDat), c("journal", "coau", "obs", "time"))
dats <- list()
for (var in vars) {
  fit <- lm(
    paste0("coau ~ time + ", var, " + ", var, ":time"),
    data = trainDat
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
                                  Dev = deviance(fit),
                                  DfRes = df.residual(fit),
                                  AIC = AIC(fit),
                                  TrainMSE = train.mse,
                                  TestMSE = test.mse)))
}
fitDat0 <- rbindlist(dats)


## Multiple regression models

# load fixed effects model fit
load("calc/fit1logFreq1.RData")
sum <- summary(fit)
# assess predictions (MSE)
pred <- predict(object = fit, newdata = trainDat, type = "response")
train.mse <- mean((trainDat$coau - pred) ^ 2)
pred <- predict(object = fit, newdata = testDat, type = "response")
test.mse <- mean((testDat$coau - pred) ^ 2)
# data frame
fitDat1 <- data.table(Var = rownames(coef(sum))[-1],
                      Est = coef(sum)[-1, 1],
                      SE = coef(sum)[-1, 2],
                      Dev = deviance(fit),
                      DfRes = df.residual(fit),
                      AIC = AIC(fit),
                      TrainMSE = train.mse,
                      TestMSE = test.mse)
# fit name
fit1 <- fit

# load mixed effects model fit
load("calc/fit1logFreq2.RData")
sum <- summary(fit)
# Assess predictions (MSE)
pred <- predict(object = fit, newdata = trainDat, type = "response")
train.mse <- mean((trainDat$coau - pred) ^ 2)
pred <- predict(object = fit, newdata = testDat, type = "response")
test.mse <- mean((testDat$coau - pred) ^ 2)
# data frame
fitDat2 <- data.table(Var = rownames(coef(sum))[-1],
                      Est = coef(sum)[-1, 1],
                      SE = coef(sum)[-1, 2],
                      Dev = REMLcrit(fit),
                      DfRes = df.residual(fit),
                      AIC = AIC(fit),
                      TrainMSE = train.mse,
                      TestMSE = test.mse)
# fit name
fit2 <- fit


## effects tables

# main effects
mainDat <- rbindlist(list(
  cbind(Model = "Single", fitDat0[!grep(":", fitDat0$Var)]),
  cbind(Model = "Fixed", fitDat1[!grep(":", fitDat1$Var)]),
  cbind(Model = "Mixed", fitDat2[!grep(":", fitDat2$Var)])
))
mainDat$Var <- factor(mainDat$Var, levels = names(scales))
mainDat$Model <- factor(mainDat$Model,
                        levels = c("Single", "Fixed", "Mixed"))
setkey(mainDat, Var, Model)
# un-scale
mainDat$Est <- mainDat$Est / scales[mainDat$Var]
mainDat$SE <- mainDat$SE / scales[mainDat$Var]
# round
mainDat[, Entry := paste0(round(Est, 3), " (", round(SE, 3), ")")]
mainCast <- dcast(mainDat, Var ~ Model, value.var = "Entry")
# append standard deviations of journal-level random effects
sigma_1 <- sd(ranef(fit2)$journal$"(Intercept)")
mainCast <- rbind(mainCast, data.frame(
  Var = "journal", Single = NA, Fixed = NA,
  Mixed = round(sigma_1, 3)
))

# interaction effects
timeDat <- rbindlist(list(
  cbind(Model = "Single", fitDat0[grep("time", fitDat0$Var)]),
  cbind(Model = "Fixed", fitDat1[grep("time", fitDat1$Var)]),
  cbind(Model = "Mixed", fitDat2[grep("time", fitDat2$Var)])
))
timeDat$Var <- factor(timeDat$Var,
                      levels = c("time", paste0("time:", names(scales))))
timeDat$Model <- factor(timeDat$Model,
                        levels = c("Single", "Fixed", "Mixed"))
setkey(timeDat, Var, Model)
# un-scale
interScales <- c(1, scales)
names(interScales) <- c("time", paste0("time:", names(scales)))
timeDat$Est <- timeDat$Est / (interScales[timeDat$Var] * scales["time"])
timeDat$SE <- timeDat$SE / (interScales[timeDat$Var] * scales["time"])
# round
timeDat[, Entry := paste0(round(Est, 3), " (", round(SE, 3), ")")]
timeCast <- dcast(timeDat, Var ~ Model, value.var = "Entry")
# append standard deviations of journal-level random effects
sigma_t <- sd(ranef(fit2)$journal$"time")
timeCast <- rbind(timeCast, data.frame(
  Var = "journal", Single = NA, Fixed = NA,
  Mixed = round(sigma_t, 3)
))

# merge data
bothCast <- cbind(mainCast[, .(Var, Single)],
                  timeCast[, .(Single)],
                  mainCast[, .(Fixed)],
                  timeCast[, .(Fixed)],
                  mainCast[, .(Mixed)],
                  timeCast[, .(Mixed)])
print(bothCast)


rm(list = ls())
