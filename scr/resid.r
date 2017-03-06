### Residuals analysis

# Source base code & choices
source("code/base.r")
source("code/choices.r")
source("code/specs.r")

load("calc/fitFreq1.RData")
fit1 <- fit
load("calc/fitFreq2.RData")
fit2 <- fit
rm(fit)

for (pkg in c("MASS", "glmnet", "lme4")) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    stopifnot(require(pkg, character.only = TRUE))
  }
}
options(scipen = 9, digits = 8)


## Residual QQ plots

# Fixed-effects model
step <- 1 / (length(resid(fit1)) + 1)
qqplot(x = qnorm(seq(step, 1 - step, step)),
       y = resid(fit1),
       pch = 1, cex = .5, col = "#00000033",
       xlab = "Normal quantiles",
       ylab = "Residuals",
       main = "Normal Q-Q plot: fixed-effects model")
grid(lty = 3)

# Mixed-effects model
step <- 1 / (length(resid(fit2)) + 1)
qqplot(x = qnorm(seq(step, 1 - step, step)),
       y = resid(fit2),
       pch = 1, cex = .5, col = "#00000033",
       xlab = "Normal quantiles",
       ylab = "Residuals",
       main = "Normal Q-Q plot: mixed-effects model")
grid(lty = 3)


## Residuals versus main predictors

# Fixed-effects model
for (var in setdiff(names(fit1$model), c("coau"))) {
  plot(x = fit1$model[[var]], y = resid(fit1),
       pch = 1, cex = .5, col = "#00000033",
       xlab = var, ylab = "Residuals")
}

# Mixed-effects model
for (var in setdiff(names(fit2@frame), c("coau", "journal"))) {
  plot(x = fit2@frame[[var]], y = resid(fit2),
       pch = 1, cex = .5, col = "#00000033",
       xlab = var, ylab = "Residuals")
}
plot(x = ranef(fit2)$journal[fit2@frame$journal, "(Intercept)"],
     y = resid(fit2),
     pch = 1, cex = .5, col = "#00000033",
     xlab = "Journal conditional mode", ylab = "Residuals")


## Residuals versus predictions

# Fixed-effects model
plot(x = predict(fit1), y = resid(fit1),
     pch = 1, cex = .5, col = "#00000033",
     xlab = "Predicted values", ylab = "Residuals")

# Mixed-effects model
plot(x = predict(fit2), y = resid(fit2),
     pch = 1, cex = .5, col = "#00000033",
     xlab = "Predicted values", ylab = "Residuals")


rm(list = ls())
