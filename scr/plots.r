### Generate elegant plots

# Source base code & choices
source("code/base.r")
source("code/choices.r")
source("code/specs.r")

for (pkg in c("data.table", "ggplot2", "directlabels",
              "MASS", "lme4", "gridExtra")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 9, digits = 8)


## Should need only run section of code to produce desired plot


## Modeling occurrence and growth

# Training data
load("calc/repDat.RData")
load("calc/inTrain.RData")
trainDat <- repDat[inTrain[, 1]]

# Name substitutions
varNames <- c(funding = "Support",
              trial = "Clinical trial",
              review = "Review",
              breadth = "Major MeSH",
              prestige = "Impact factor",
              journal = "Journal")
if (incl_indicator == TRUE) varNames <- c(varNames, viz = "Indexed")
vars <- setdiff(names(trainDat), c("journal", "coau", "obs"))

# Time series of cross-sectional effect estimates
effdat <- rbindlist(lapply(sort(unique(floor(trainDat$time))), function(t) {
    subdat <- trainDat[floor(time) == t]
    fit <- glm.nb(
        paste("coau ~", paste(setdiff(vars, "time"), collapse = " + ")),
        data = subdat
    )
    sum <- summary(fit)
    data.table(time = t,
               var = rownames(coef(sum)),
               est = coef(sum)[, 1] / 2,
               se = coef(sum)[, 2] / 2)
}))
# Adjust factor
varintNames <- c("(Intercept)" = "Intercept", varNames)
effdat[, var := factor(varintNames[gsub("TRUE|FALSE", "", var)],
                       levels = varintNames)]
effdat[, time := factor(time)]
# Plot
z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)
wrap_ncol <- 2
effplot <- ggplot(data = effdat, aes(x = time, y = est)) +
    geom_point() +
    geom_segment(aes(x = time, xend = time,
                     y = est - se * z, yend = est + se * z)) +
    facet_wrap(~ var, ncol = wrap_ncol, scales = "free_y") +
    xlab("Year") + ylab("Effect estimate") +
    theme_bw()
stack_height <- ceiling(length(unique(effdat$var)) / wrap_ncol)
pdf(height = maxwid * stack_height / 4, width = maxwid,
    file = "fig/plot-cs.pdf")
print(effplot)
dev.off()


## Effect estimates

# Scales
load("calc/baseChoice.RData")
# Multiple fixed-effects model
load("calc/fitFreq1.RData")
fit_fixed <- fit
# Multiple model with interactions and random effects
load("calc/fitFreq2.RData")
fit_mixed <- fit
rm(fit)

# Master data table
modelDat <- rbindlist(list(
  data.table(model = "Fixed",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_fixed))[, 1],
             se = coef(summary(fit_fixed))[, 2]),
  data.table(model = "Mixed",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_mixed))[, 1],
             se = coef(summary(fit_mixed))[, 2]),
  data.table(model = "Mixed",
             var = "journal",
             est = sd(ranef(fit_mixed)$journal$"(Intercept)") * 2,
             se = 0),
  data.table(model = "Mixed",
             var = "time:journal",
             est = sd(ranef(fit_mixed)$journal$time) * 2,
             se = 0)
))

# Numerical estimate and standard error values
modelDat$est <- as.numeric(modelDat$est)
modelDat$se <- as.numeric(modelDat$se)

# Introduce interaction indicator
modelDat[, type := factor(
  ifelse(!grepl("^time:", var), "Main", "Interaction with publication date"),
  levels = c("Main", "Interaction with publication date")
)]
modelDat$var <- gsub("^time:", "", modelDat$var)

# Introduce random effect indicator
modelDat[, level := factor(ifelse(var == "journal", "Second", "First"),
                           levels = c("First", "Second"))]

# Factorize
modelDat <- modelDat[!(var %in% c("(Intercept)", "time"))]
modelDat$model <- factor(modelDat$model, levels = c("Fixed", "Mixed"))
modelDat$var <- factor(modelDat$var,
                       levels = c("trial", "review", "breadth", "funding",
                                  "prestige", "journal"))

# Sort
setkey(modelDat, type, level, var, model)

# Name substitutions
varNames <- c(funding = "Support",
              trial = "Clinical trial",
              review = "Review",
              breadth = "Major MeSH",
              prestige = "Impact factor",
              journal = "Journal")
if (incl_indicator == TRUE) varNames <- c(varNames, viz = "Indexed")

# Absolute values
neg.var <- intersect(
  modelDat[model == "Fixed" & type == "Main" & est < 0]$var,
  modelDat[model == "Mixed" & type == "Main" & est < 0]$var
)
modelDat[var %in% neg.var]$est <- -modelDat[var %in% neg.var]$est
varNames[neg.var] <- paste(varNames[neg.var], "(-)")

# Vertical placement by model and variable
dat <- modelDat[type == "Main"]
ys <- -(1:nrow(dat) + .5 * (cumsum(
  c(TRUE, dat$var[1:(nrow(dat) - 1)] != dat$var[2:nrow(dat)])
) - 1))
ygap <- ys[which(
  c(TRUE, dat$var[1:(nrow(dat) - 1)] != dat$var[2:nrow(dat)])
)[-1]] + .75
ymid <- sapply(levels(dat$var), function(v) {
  median(ys[which(dat$var == v)])
})
modelDat$ys <-  rep(ys, times = 2)
# Confidence interval
z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)

# Construct plot
effPlot <- ggplot(data = modelDat) +
  geom_point(aes(x = est, y = ys, shape = model,
                 fill = factor(ifelse(level == "First", NA, FALSE))),
             size = 2.5) +
  scale_shape_manual(values = c(21, 24), name = "") +
  scale_fill_manual(values = c("black", "black"), na.value = NA,
                    guide = "none") +
  geom_segment(aes(x = est - se * z, xend = est + se * z,
                   y = ys, yend = ys)) +
  geom_segment(linetype = 2, size = .15,
               aes(x = 0, xend = 0, y = -Inf, yend = Inf)) +
  xlab("Standardized effect estimate") + ylab("") +
  facet_wrap(~ type, scales = "free_x") +
  scale_y_continuous(breaks = ymid, labels = varNames[names(ymid)],
                     minor_breaks = ygap) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.major = element_line(color = rgb(.95, .95, .95)),
        panel.grid.minor = element_line(color = rgb(.95, .95, .95)))
pdf(height = maxwid * .4, width = maxwid,
    file = "fig/plot-eff.pdf")
print(effPlot)
dev.off()


## Effect estimates (including prestige as a random effect)

# Scales
load("calc/baseChoice.RData")
# Multiple fixed-effects model
load("calc/fitFreq1.RData")
fit_fixed <- fit
# Multiple model with interactions and random effects
load("calc/fitFreq2.RData")
fit_mixed1 <- fit
# Multiple model with interactions and random effects (including prestige)
load("calc/fitFreq3.RData")
fit_mixed2 <- fit
rm(fit)

# Master data table
modelDat <- rbindlist(list(
  data.table(model = "Fixed",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_fixed))[, 1],
             se = coef(summary(fit_fixed))[, 2]),
  data.table(model = "Mixed 1",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_mixed1))[, 1],
             se = coef(summary(fit_mixed1))[, 2]),
  data.table(model = "Mixed 1",
             var = "journal",
             est = sd(ranef(fit_mixed1)$journal$"(Intercept)") * 2,
             se = 0),
  data.table(model = "Mixed 1",
             var = "time:journal",
             est = sd(ranef(fit_mixed1)$journal$time) * 2,
             se = 0),
  data.table(model = "Mixed 2",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_mixed2))[, 1],
             se = coef(summary(fit_mixed2))[, 2]),
  data.table(model = "Mixed 2",
             var = "journal",
             est = sd(ranef(fit_mixed2)$journal$"(Intercept)") * 2,
             se = 0),
  data.table(model = "Mixed 2",
             var = "time:journal",
             est = sd(ranef(fit_mixed2)$journal$time) * 2,
             se = 0)
))

# Numerical estimate and standard error values
modelDat$est <- as.numeric(modelDat$est)
modelDat$se <- as.numeric(modelDat$se)

# Introduce interaction indicator
modelDat[, type := factor(
  ifelse(!grepl("^time:", var), "Main", "Interaction with publication date"),
  levels = c("Main", "Interaction with publication date")
)]
modelDat$var <- gsub("^time:", "", modelDat$var)

# Introduce random effect indicator
modelDat[, level := factor(ifelse(var == "journal", "Second", "First"),
                           levels = c("First", "Second"))]

# Factorize
modelDat <- modelDat[!(var %in% c("(Intercept)", "time"))]
modelDat$model <- factor(modelDat$model,
                         levels = c("Fixed", "Mixed 1", "Mixed 2"))
modelDat$var <- factor(modelDat$var,
                       levels = c("trial", "review", "breadth", "funding",
                                  "prestige", "journal"))

# Sort
setkey(modelDat, type, level, var, model)

# Name substitutions
varNames <- c(funding = "Support",
              trial = "Clinical trial",
              review = "Review",
              breadth = "Major MeSH",
              prestige = "Impact factor",
              journal = "Journal")
if (incl_indicator == TRUE) varNames <- c(varNames, viz = "Indexed")

# Absolute values
neg.var <- intersect(
  modelDat[model == "Fixed" & type == "Main" & est < 0]$var,
  modelDat[model == "Mixed" & type == "Main" & est < 0]$var
)
modelDat[var %in% neg.var]$est <- -modelDat[var %in% neg.var]$est
varNames[neg.var] <- paste(varNames[neg.var], "(-)")

# Vertical placement by model and variable
dat <- modelDat[type == "Main"]
ys <- -(1:nrow(dat) + .5 * (cumsum(
  c(TRUE, dat$var[1:(nrow(dat) - 1)] != dat$var[2:nrow(dat)])
) - 1))
ygap <- ys[which(
  c(TRUE, dat$var[1:(nrow(dat) - 1)] != dat$var[2:nrow(dat)])
)[-1]] + .75
ymid <- sapply(levels(dat$var), function(v) {
  median(ys[which(dat$var == v)])
})
modelDat$ys <-  rep(ys, times = 2)
# Confidence interval
z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)

# Construct plot
effPlot <- ggplot(data = modelDat) +
  geom_point(aes(x = est, y = ys, shape = model,
                 fill = factor(ifelse(level == "First", NA, FALSE))),
             size = 2.5) +
  scale_shape_manual(values = c(21, 24, 25), name = "") +
  scale_fill_manual(values = c("black", "black"), na.value = NA,
                    guide = "none") +
  geom_segment(aes(x = est - se * z, xend = est + se * z,
                   y = ys, yend = ys)) +
  geom_segment(linetype = 2, size = .15,
               aes(x = 0, xend = 0, y = -Inf, yend = Inf)) +
  xlab("Standardized effect estimate") + ylab("") +
  facet_wrap(~ type, scales = "free_x") +
  scale_y_continuous(breaks = ymid, labels = varNames[names(ymid)],
                     minor_breaks = ygap) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.major = element_line(color = rgb(.95, .95, .95)),
        panel.grid.minor = element_line(color = rgb(.95, .95, .95)))
pdf(height = maxwid * .55, width = maxwid,
    file = "fig/plot-eff-im-issn.pdf")
print(effPlot)
dev.off()


## Prediction curves

# Scales
load("calc/baseChoice.RData")
# Load measurement transforms
load("calc/measChoice.RData")
# Load original data
load("calc/repDat.RData")
# Multiple fixed-effects model
load("calc/fitFreq1.RData")
fit_fixed <- fit
rm(fit)

# Time sequence
timeVec <- sort(unique(repDat$time))
# Calculate mean for each predictor
means <- sapply(repDat[, .(trial, review, funding, breadth, prestige)], mean)
if (incl_indicator) means <- c(means, viz = mean(repDat$viz))
# Representative values of each predictor
reps <- list(
  trial = c(F, T), funding = c(F, T),
  breadth = 2:6, prestige = 1:5
)
if (incl_indicator == TRUE) reps <- c(reps, list(viz = c(F, T)))
# Name substitutions
varNames <- c(trial = "Clinical trial report",
              review = "Review article",
              breadth = "Major MeSH terms",
              funding = "Research support",
              prestige = "Impact factor")
if (incl_indicator == TRUE) varNames <- c(varNames, viz = "Indexed")

# Input variable centers
centers <- c(
  time = 2003,
  funding = 0, review = 0, trial = 0,
  breadth = median(repDat$breadth),
  prestige = mean(unique(repDat[, .(journal, time, prestige)])$prestige)
)
if (incl_indicator) centers <- c(centers, viz = 0)

# Convert data to model form
stdFn <- function(dat) {
  # Transform
  for (v in intersect(names(transforms), names(dat))) {
    fun <- transforms[[v]][[1]]
    dat[[v]] <- fun(dat[[v]])
  }
  # Center
  for (v in names(centers)) {
    dat[[v]] <- dat[[v]] - centers[v]
  }
  # Standardize
  for (v in intersect(names(scales), names(dat))) {
    dat[[v]] <- dat[[v]] / scales[v]
  }
  # Return
  dat
}

# Data frame of stratified predictions
predDat <- rbindlist(lapply(names(reps), function(var) {
  
  # Construct predictor dataset that only varies in var and time
  dat <- data.table(time = rep(timeVec, times = length(reps[[var]])))
  dat[[var]] <- rep(reps[[var]], each = length(timeVec))
  for (v in setdiff(names(means), var)) dat[[v]] <- means[v]
  
  # Indicate which variable was varied
  dat[["var"]] <- varNames[var]
  dat[["value"]] <- dat[[var]]
  dat
}), use.names = TRUE)
print(head(predDat))

# Reorder input variables
predDat$var <- factor(predDat$var,
                      levels = setdiff(varNames, varNames["review"]))

# Calculate and bind predictions
newDat <- stdFn(predDat)
print(head(newDat))
pred <- predict(object = fit_fixed, newdata = newDat,
                type = "response", re.form = NA) + 1
predDat[["pred"]] <- pred

# Change binary values from numeric to Y/N
predDat[, value := as.character(value)]
predDat[var %in% c("Clinical trial report",
                   "Review article",
                   "Research support") & value == "0"]$value <- "N"
predDat[var %in% c("Clinical trial report",
                   "Review article",
                   "Research support") & value == "1"]$value <- "Y"
# Plot!
predPlot <- ggplot(data = predDat,
                   aes(x = time, y = pred, group = value)) +
  geom_line() +
  geom_dl(aes(label = value),
          method = list("maxvar.qp", rot = 0)) +
  xlab("Date of publication") + ylab("Predicted author count") +
  facet_wrap(~ var, nrow = 2) +
  theme_bw()
pdf(height = maxwid * 3/4, width = maxwid,
    file = "fig/plot-pred.pdf")
print(predPlot)
dev.off()


## Random effects

# Load original data
load("calc/repDat.RData")

# Journal volumes
volDat <- dcast(repDat, journal ~ ., value.var = "coau",
                fun.aggregate = length)
vols <- volDat$.
names(vols) <- volDat$journal

# Journal prestige (mean across years)
prestigeDat <- dcast(repDat[, .(journal, time, prestige)], journal ~ .,
                     value.var = "prestige", fun.aggregate = mean)
prestige <- prestigeDat$.
names(prestige) <- prestigeDat$journal

# Multiple model with interactions and random effects
load("calc/fitFreq2.RData")
fit_mixed <- fit
rm(fit)

# Data frame for random effects
ranefDat <- ranef(fit_mixed)[["journal"]]
ranefDat$journal <- rownames(ranefDat)

# Melt by effect type
ranefDat <- melt(ranefDat, value.name = "eff",
                 id.vars = "journal", measure.vars = c("(Intercept)", "time"))

# Volume and influence
ranefDat$vol <- vols[ranefDat$journal]
ranefDat$prestige <- prestige[ranefDat$journal]

# Effect predictions
coefMat <- coef(summary(fit_mixed))[names(ranef(fit_mixed)$journal), 1:2]
ranefDat$pred <- coefMat[ranefDat$variable, 1] + ranefDat$eff

# Quantiles
len <- nrow(ranef(fit_mixed)$journal)
quants <- qnorm(seq(0, 1, length.out = len + 2)[-c(1, len + 2)])
ranefDat$quant <- NA
for (var in rownames(coefMat)) {
  wh <- which(ranefDat$variable == var)
  ranefDat$quant[wh] <- quants[order(order(ranefDat$eff[wh]))]
}

# Clean
levels(ranefDat$variable) <- c("Intercept", "Date of publication")

# Annotate
mainDat <- data.frame(
  variable = as.factor(c("Intercept", "Date of publication")),
  est = coefMat[, "Estimate"],
  se = coefMat[, "Std. Error"]
)
z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)
mainDat$lower <- mainDat$est - mainDat$se * z
mainDat$upper <- mainDat$est + mainDat$se * z

# Estimated standard deviations
sigma <- sapply(ranef(fit_mixed)$journal, sd)

# Construct plot
ranefPlot <- ggplot(data = ranefDat,
                    aes(x = quant, y = pred, size = vol, group = variable)) +
  geom_point(shape = 1) +
  scale_size_area(name = "Volume") +
  xlab("Theoretical quantile") + ylab("Predicted effect") +
  geom_segment(data = mainDat, size = .5, linetype = "dashed",
               aes(x = min(quants), xend = max(quants),
                   y = est, yend = est),
               show.legend = FALSE) +
  geom_segment(data = mainDat, size = .5, linetype = "dotted",
               aes(x = min(quants), xend = max(quants),
                   y = lower, yend = lower),
               show.legend = FALSE) +
  geom_segment(data = mainDat, size = .5, linetype = "dotted",
               aes(x = min(quants), xend = max(quants),
                   y = upper, yend = upper),
               show.legend = FALSE) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pdf(height = bodwid * 1, width = colwid,
    file = "fig/plot-ranef.pdf")
print(ranefPlot)
dev.off()

# Un-transformed plot
expRanefPlot <- ggplot(data = ranefDat,
                       aes(x = quant, y = exp(pred),
                           size = vol, group = variable)) +
  geom_point(shape = 1) +
  scale_size_area(name = "Volume") +
  xlab("Theoretical quantile") + ylab("exp(Predicted effect)") +
  geom_segment(data = mainDat, size = .5, linetype = "dashed",
               aes(x = min(quants), xend = max(quants),
                   y = exp(est), yend = exp(est)),
               show.legend = FALSE) +
  geom_segment(data = mainDat, size = .5, linetype = "dotted",
               aes(x = min(quants), xend = max(quants),
                   y = exp(lower), yend = exp(lower)),
               show.legend = FALSE) +
  geom_segment(data = mainDat, size = .5, linetype = "dotted",
               aes(x = min(quants), xend = max(quants),
                   y = exp(upper), yend = exp(upper)),
               show.legend = FALSE) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pdf(height = bodwid * 1, width = colwid,
    file = "fig/plot-ranef-exp.pdf")
print(expRanefPlot)
dev.off()


## Effect estimates (lognormal)

# Scales
load("calc/baseChoice.RData")
# Multiple fixed-effects model
load("calc/fit1logFreq1.RData")
fit_fixed <- fit
# Multiple model with interactions and random effects
load("calc/fit1logFreq2.RData")
fit_mixed <- fit
rm(fit)

# Master data table
modelDat <- rbindlist(list(
  data.table(model = "Fixed",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_fixed))[, 1],
             se = coef(summary(fit_fixed))[, 2]),
  data.table(model = "Mixed",
             var = rownames(coef(summary(fit_fixed))),
             est = coef(summary(fit_mixed))[, 1],
             se = coef(summary(fit_mixed))[, 2]),
  data.table(model = "Mixed",
             var = "journal",
             est = sd(ranef(fit_mixed)$journal$"(Intercept)") * 2,
             se = 0),
  data.table(model = "Mixed",
             var = "time:journal",
             est = sd(ranef(fit_mixed)$journal$time) * 2,
             se = 0)
))

# Numerical estimate and standard error values
modelDat$est <- as.numeric(modelDat$est)
modelDat$se <- as.numeric(modelDat$se)

# Introduce interaction indicator
modelDat[, type := factor(
  ifelse(!grepl("^time:", var), "Main", "Interaction with publication date"),
  levels = c("Main", "Interaction with publication date")
)]
modelDat$var <- gsub("^time:", "", modelDat$var)

# Introduce random effect indicator
modelDat[, level := factor(ifelse(var == "journal", "Second", "First"),
                           levels = c("First", "Second"))]

# Factorize
modelDat <- modelDat[!(var %in% c("(Intercept)", "time"))]
modelDat$model <- factor(modelDat$model, levels = c("Fixed", "Mixed"))
modelDat$var <- factor(modelDat$var,
                       levels = c("trial", "review", "breadth", "funding",
                                  "prestige", "journal"))

# Sort
setkey(modelDat, type, level, var, model)

# Name substitutions
varNames <- c(funding = "Support",
              trial = "Clinical trial",
              review = "Review",
              breadth = "Major MeSH",
              prestige = "Impact factor",
              journal = "Journal")

# Absolute values
neg.var <- intersect(
  modelDat[model == "Fixed" & type == "Main" & est < 0]$var,
  modelDat[model == "Mixed" & type == "Main" & est < 0]$var
)
modelDat[var %in% neg.var]$est <- -modelDat[var %in% neg.var]$est
varNames[neg.var] <- paste(varNames[neg.var], "(-)")

# Vertical placement by model and variable
dat <- modelDat[type == "Main"]
ys <- -(1:nrow(dat) + .5 * (cumsum(
  c(TRUE, dat$var[1:(nrow(dat) - 1)] != dat$var[2:nrow(dat)])
) - 1))
ygap <- ys[which(
  c(TRUE, dat$var[1:(nrow(dat) - 1)] != dat$var[2:nrow(dat)])
)[-1]] + .75
ymid <- sapply(levels(dat$var), function(v) {
  median(ys[which(dat$var == v)])
})
modelDat$ys <-  rep(ys, times = 2)
# Confidence interval
z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)

# Construct plot
effPlot <- ggplot(data = modelDat) +
  geom_point(aes(x = est, y = ys, shape = model,
                 fill = factor(ifelse(level == "First", NA, FALSE))),
             size = 2.5) +
  scale_shape_manual(values = c(21, 24), name = "") +
  scale_fill_manual(values = c("black", "black"), na.value = NA,
                    guide = "none") +
  geom_segment(aes(x = est - se * z, xend = est + se * z,
                   y = ys, yend = ys)) +
  geom_segment(linetype = 2, size = .15,
               aes(x = 0, xend = 0, y = -Inf, yend = Inf)) +
  xlab("Standardized effect estimate") + ylab("") +
  facet_wrap(~ type, scales = "free_x") +
  scale_y_continuous(breaks = ymid, labels = varNames[names(ymid)],
                     minor_breaks = ygap) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.major = element_line(color = rgb(.95, .95, .95)),
        panel.grid.minor = element_line(color = rgb(.95, .95, .95)))
pdf(height = maxwid * .4, width = maxwid,
    file = "fig/plot-eff-ln.pdf")
print(effPlot)
dev.off()


## Sensitivity analysis effect estimates

# Scales
load("calc/baseChoice.RData")
load("calc/measChoice.RData")
# Sensitivity analysis data
load("calc/sensDat.RData")

# Add field for choices of measure
sensDat$Meas <- paste0(sensDat$breadth, "-",
                       sensDat$funding, "-",
                       sensDat$prestige)

# Omit date of publication effect
sensDat <- sensDat[sensDat$Var != "time", ]

# Introduce interaction indicator
sensDat$Type <- factor(
    ifelse(!grepl("^time:", sensDat$Var),
           "Main", "Interaction with publication date"),
    levels = c("Main", "Interaction with publication date")
)
sensDat$Var <- gsub("^time:", "", sensDat$Var) # un-factorizes

# Factorize
sensDat$Meas <- factor(sensDat$Meas, levels = sort(unique(sensDat$Meas)))
vars <- c("trial", "review", "breadth", "funding", "prestige")
sensDat$Var <- factor(sensDat$Var, levels = vars)

# Sort
sensDat <- as.data.table(sensDat)
setkey(sensDat, Model, Type, Var, Meas)

# Name substitutions
varNames <- c(funding = "Research support",
              trial = "Clinical trial",
              review = "Review",
              breadth = "Topical scope",
              prestige = "Influence metric")
if (incl_indicator == TRUE) varNames <- c(varNames, viz = "Indexed")

# For each model...
for (model in c("Fixed", "Mixed")) {
    
    dat <- sensDat[Model == model]
    subdat <- dat[Type == "Main"]
    
    # Vertical placement by model and variable
    ys <- -(1:nrow(subdat) + .5 * (cumsum(
        c(TRUE, subdat$Var[1:(nrow(subdat) - 1)] != subdat$Var[2:nrow(subdat)])
    ) - 1))
    ygap <- ys[which(
        c(TRUE, subdat$Var[1:(nrow(subdat) - 1)] != subdat$Var[2:nrow(subdat)])
    )[-1]] + .75
    ymid <- sapply(levels(subdat$Var), function(v) {
        median(ys[which(subdat$Var == v)])
    })
    dat$ys <- rep(ys, times = 2)
    # Confidence interval
    z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)
    
    # Construct plot
    effPlot <- ggplot(data = dat) +
        geom_point(aes(x = Est, y = ys, shape = Meas), size = 2.5) +
        scale_shape_manual(name = "",
                           values = c(1, 0, 2, 5, 4, 7, 16, 15, 17, 18, 8, 9)) +
        geom_segment(aes(x = Est - SE * z, xend = Est + SE * z,
                         y = ys, yend = ys)) +
        geom_segment(linetype = 2, size = .15,
                     aes(x = 0, xend = 0, y = -Inf, yend = Inf)) +
        xlab("Standardized effect estimate") + ylab("") +
        facet_wrap(~ Type, scales = "free_x") +
        scale_y_continuous(breaks = ymid,
                           labels = varNames[gsub("^time:", "", names(ymid))],
                           minor_breaks = ygap) +
        guides(shape = guide_legend(nrow = 3)) +
        theme_bw() +
        theme(legend.position = "top",
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(angle = 90, hjust = .5),
              panel.grid.major = element_line(color = rgb(.95, .95, .95)),
              panel.grid.minor = element_line(color = rgb(.95, .95, .95)))
    combination_count <- length(unique(dat$Meas)) # adjust for no. plots
    pdf(height = maxwid * combination_count / 10, width = maxwid,
        file = paste0("fig/plot-sens-",
                      tolower(model), ".pdf"))
    print(effPlot) # SHRINK LEFT MARGIN
    dev.off()
}


## Subject-specific analysis effect estimates

# Scales
load("calc/baseChoice.RData")
load("calc/measChoice.RData")
# SC-specific analysis data
load("calc/fitFreqCats.RData")

# Omit date of publication effect
catsDat <- catsDat[catsDat$Var != "time", ]

# Introduce interaction indicator
catsDat$Type <- factor(
    ifelse(!grepl("^time:", catsDat$Var),
           "Main", "Interaction with publication date"),
    levels = c("Main", "Interaction with publication date")
)
catsDat$Var <- gsub("^time:", "", catsDat$Var) # un-factorizes

# Factorize
catsDat$Var <- gsub("TRUE$", "", catsDat$Var)
vars <- c("trial", "review", "breadth", "funding", "prestige")
catsDat$Var <- factor(catsDat$Var, levels = vars)

# Sort
setkey(catsDat, Model, Type, Var, Category)

# Name substitutions
varNames <- c(funding = "Research support",
              trial = "Clinical trial",
              review = "Review",
              breadth = "Major MeSH",
              prestige = "Impact factor")
if (incl_indicator == TRUE) varNames <- c(varNames, viz = "Indexed")

# Manual labeling scale
shape_scale <- c(LETTERS, as.character(0:9), letters)

# For each model...
for (model in c("Fixed", "Mixed")) {
    
    dat <- catsDat[Model == model]
    subdat <- dat[Type == "Main"]
    
    # Vertical placement by model and variable
    ys <- -(1:nrow(subdat) + .5 * (cumsum(
        c(TRUE, subdat$Var[1:(nrow(subdat) - 1)] != subdat$Var[2:nrow(subdat)])
    ) - 1))
    ygap <- ys[which(
        c(TRUE, subdat$Var[1:(nrow(subdat) - 1)] != subdat$Var[2:nrow(subdat)])
    )[-1]] + .75
    ymid <- sapply(levels(subdat$Var), function(v) {
        median(ys[which(subdat$Var == v)])
    })
    dat$ys <- rep(ys, times = 2)
    # Confidence interval
    z <- qnorm(p = (1 - .99) / 2, lower.tail = FALSE)
    
    # Construct plot
    effPlot <- ggplot(data = dat) +
        geom_point(aes(x = Est, y = ys, shape = Category), size = 2.5) +
        scale_shape_manual(name = "", values = shape_scale) +
        geom_segment(aes(x = Est - SE * z, xend = Est + SE * z,
                         y = ys, yend = ys)) +
        geom_segment(linetype = 2, size = .15,
                     aes(x = 0, xend = 0, y = -Inf, yend = Inf)) +
        xlab("Standardized effect estimate") + ylab("") +
        facet_wrap(~ Type, scales = "free_x") +
        scale_y_continuous(breaks = ymid,
                           labels = varNames[gsub("^time:", "", names(ymid))],
                           minor_breaks = ygap,
                           limits = range(ys)) +
        guides(shape = guide_legend(ncol = 1)) +
        theme_bw() +
        theme(axis.ticks.y = element_blank(),
              axis.text.y = element_text(angle = 90, hjust = .5),
              panel.grid.major = element_line(color = rgb(.95, .95, .95)),
              panel.grid.minor = element_line(color = rgb(.95, .95, .95)))
    pdf(height = bodwid * 2, width = bodwid * 1.5,
        file = paste0("fig/plot-sc-", tolower(model), ".pdf"))
    print(effPlot)
    dev.off()
}


rm(list = ls())
