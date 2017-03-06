### Re-fit models for each choice of variable measure

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/subDat.RData")
load("calc/measChoice.RData")
load("calc/inTrain.RData")

for (pkg in c("MASS", "glmnet", "lme4")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 9, digits = 8)


## Prep

# Select elements from a list of vectors by position
vecSelec <- function(lst, pos) {
    stopifnot(length(lst) == length(pos))
    unlist(lapply(1:length(lst), function(i) lst[[i]][pos[i]]))
}


## Fixed-effects and mixed-effects analysis under all combinations of measures

# Initialize data frame
sensDat <- data.frame()
# Lengths
lens <- sapply(transforms, length)
# Fixed-effects or mixed-effects
for (model in c("Fixed", "Mixed")) {
    
    # Reset position
    pos <- sapply(transforms, function(x) 1)
    
    while (all(pos <= lens)) {
        
        # Print position
        print(pos)
        
        # Store choices
        wh <- which(lens > 1)
        lst <- as.list(pos[wh])
        names(lst) <- names(measures)[wh]
        
        # Select choices
        measure <- vecSelec(measures, pos)
        transform <- vecSelec(transforms, pos)
        
        # Construct dataset
        repDat <- subDat[, .(issn)]
        setnames(repDat, "issn", "journal")
        for (j in 1:length(measure)) {
            fun <- transform[[j]]
            repDat[, field := fun(subDat[, measure[j], with = FALSE])]
            setnames(repDat, "field", names(measures)[j])
        }
        if (incl_indicator) repDat[is.na(prestige)]$prestige <- 0
        
        # Numericize time variable
        repDat$time <- as.numeric(repDat$time)
        
        # Training set
        trainDat <- repDat[inTrain[, 1]]
        testDat <- repDat[-inTrain[, 1]]
        
        # Center
        centers <- c(
            time = as.numeric(median(unique(floor(trainDat$time)))),
            funding = 0, review = 0, trial = 0,
            breadth = median(trainDat$breadth),
            prestige = mean(unique(trainDat[, .(journal, time,
                                                prestige)])$prestige,
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
        
        # Fit fixed-effects multivariate model
        vars <- setdiff(names(repDat), c("journal", "coau", "obs"))
        ints <- paste0(setdiff(vars, "time"), ":time")
        fit <- if (model == "Mixed") {
            glmer.nb(
                paste("coau ~",
                      paste(c(vars, ints), collapse = " + "),
                      "+ (time | journal)"),
                data = trainDat
            )
        } else {
            glm.nb(
                paste("coau ~", paste(c(vars, ints), collapse = " + ")),
                data = trainDat
            )
        }
        sum <- summary(fit)
        
        # Assess predictions (MSE)
        pred <- predict(object = fit, newdata = trainDat, type = "response")
        train.mse <- mean((trainDat$coau - pred) ^ 2)
        pred <- predict(object = fit, newdata = testDat, type = "response")
        test.mse <- mean((testDat$coau - pred) ^ 2)
        
        # Bind to data frame
        dat <- data.frame(as.data.table(lst),
                          Model = model,
                          Var = rownames(coef(sum))[-1],
                          Est = coef(sum)[-1, 1],
                          SE = coef(sum)[-1, 2],
                          Dev = unname(if (model == "Fixed") {
                              sum$deviance
                          } else {
                              sum$AICtab["deviance"]
                          }),
                          DfRes = unname(if (model == "Fixed") {
                              sum$df.residual
                          } else {
                              sum$AICtab["df.resid"]
                          }),
                          AIC = if (model == "Fixed") {
                              sum$aic
                          } else {
                              AIC = sum$AICtab["AIC"]
                          },
                          TrainMSE = train.mse,
                          TestMSE = test.mse)
        sensDat <- rbind(sensDat, dat)
        
        # Update position
        if (all(pos == lens)) break
        w <- max(which(pos < lens))
        pos[w] <- pos[w] + 1
        if (w < length(pos)) pos[(w + 1):length(pos)] <- 1
    }
}

# Save!
rownames(sensDat) <- NULL
save(lens, sensDat, file = "calc/sensDat.RData")


rm(list = ls())
