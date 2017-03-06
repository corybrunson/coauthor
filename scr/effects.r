### Effect estimates

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/measChoice.RData")
load("calc/repDat.RData")
load("calc/inTrain.RData")
load("calc/baseChoice.RData")

for (pkg in c("MASS", "glmnet", "lme4")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
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
    fit <- glm.nb(
        if (incl_indicator & var == "prestige")
            paste0("coau ~ time + ", var, " + ", var, ":time",
                   " + viz + viz:time")
        else
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
                                    Dev = unname(sum$deviance),
                                    DfRes = unname(sum$df.residual),
                                    AIC = sum$aic,
                                    TrainMSE = train.mse,
                                    TestMSE = test.mse)))
}
fitDat0 <- rbindlist(dats)


## Multiple regression models

# load fixed effects model fit
load("calc/fitFreq1.RData")
sum <- summary(fit)
print(sum)
# assess predictions (MSE)
pred <- predict(object = fit, newdata = trainDat, type = "response")
train.mse <- mean((trainDat$coau - pred) ^ 2)
pred <- predict(object = fit, newdata = testDat, type = "response")
test.mse <- mean((testDat$coau - pred) ^ 2)
# data frame
fitDat1 <- data.table(Var = rownames(coef(sum))[-1],
                      Est = coef(sum)[-1, 1],
                      SE = coef(sum)[-1, 2],
                      Dev = unname(sum$deviance),
                      DfRes = unname(sum$df.residual),
                      AIC = sum$aic,
                      TrainMSE = train.mse,
                      TestMSE = test.mse)
# fit name
fit1 <- fit

# load mixed effects model fit
load("calc/fitFreq2.RData")
sum <- summary(fit)
print(sum)
# Assess predictions (MSE)
pred <- predict(object = fit, newdata = trainDat, type = "response")
train.mse <- mean((trainDat$coau - pred) ^ 2)
pred <- predict(object = fit, newdata = testDat, type = "response")
test.mse <- mean((testDat$coau - pred) ^ 2)
# data frame
fitDat2 <- data.table(Var = rownames(coef(sum))[-1],
                      Est = coef(sum)[-1, 1],
                      SE = coef(sum)[-1, 2],
                      Dev = unname(sum$AICtab["deviance"]),
                      DfRes = unname(sum$AICtab["df.resid"]),
                      AIC = sum$AICtab["AIC"],
                      TrainMSE = train.mse,
                      TestMSE = test.mse)
# fit name
fit2 <- fit


## effects tables

# variance estimates for random effects
varcor <- as.data.frame(VarCorr(fit2))
sigmas <- varcor[is.na(varcor$var2), ]$sdcor
names(sigmas) <- varcor[is.na(varcor$var2), ]$var1

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
mainCast <- rbind(mainCast, data.frame(
    Var = "journal", Single = NA, Fixed = NA,
    Mixed = round(sigmas["(Intercept)"], 3)
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
timeCast <- rbind(timeCast, data.frame(
    Var = "journal", Single = NA, Fixed = NA,
    Mixed = round(sigmas["time"], 3)
))

# merge data
bothCast <- cbind(mainCast[, .(Var, Single)],
                  timeCast[, .(Single)],
                  mainCast[, .(Fixed)],
                  timeCast[, .(Fixed)],
                  mainCast[, .(Mixed)],
                  timeCast[, .(Mixed)])
print(bothCast)


## Growth rate

# Coauthor count growth rate by year
annual_growth_rate <- c(
    Fixed = exp(mainDat[Model == "Fixed" & Var == "time"]$Est),
    Mixed = exp(mainDat[Model == "Mixed" & Var == "time"]$Est)
) - 1
print("Annual growth rate:")
print(annual_growth_rate)
# Coauthor count growth rate by decade
decadal_growth_rate <- c(
    Fixed = exp(10 * mainDat[Model == "Fixed" & Var == "time"]$Est),
    Mixed = exp(10 * mainDat[Model == "Mixed" & Var == "time"]$Est)
) - 1
print("Decadal growth rate:")
print(decadal_growth_rate)


rm(list = ls())
