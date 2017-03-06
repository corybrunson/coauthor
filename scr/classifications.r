### Subject classification-specific regression analyses

# Source base code & choices
source("code/base.r")
source("code/choices.r")

load("calc/measChoice.RData")
load("calc/baseChoice.RData")

for (pkg in c("MASS", "glmnet", "lme4")) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
options(scipen = 9, digits = 8)

# Fetch journal categories
load("calc/subDat.RData")
catDat <- subDat[, c("issn", "cats"), with = FALSE]
catDat <- catDat[!duplicated(catDat[, .(issn)])]
rm(subDat)

# Categories with at least 6 journals
catTab <- sort(table(unlist(catDat$cats)), decreasing = TRUE)
catTab <- catTab[catTab >= 6]
print(catTab)

# Initialize data list
catsDats <- list()

# Loop
for (sc in names(catTab)) {
    
    print(sc)
    
    
    ## Subset and standardize data
    
    load("calc/repDat.RData")
    repDat <- repDat[journal %in% catDat[grep(sc, cats)]$issn]
    
    # Center
    for (v in names(centers)) {
        repDat[[v]] <- repDat[[v]] - centers[v]
    }
    # Scale
    for (v in names(scales)) {
        repDat[[v]] <- repDat[[v]] / scales[v]
    }
    
    
    ## Single-confounder models
    
    vars <- setdiff(names(repDat), c("journal", "coau", "obs", "time"))
    dats <- list()
    for (var in vars) {
        fit <- glm.nb(
            paste0("coau ~ time + ", var, " + ", var, ":time"),
            data = repDat
        )
        sum <- summary(fit)
        # Store
        dats <- c(dats, list(data.table(Category = sc,
                                        Model = "Single",
                                        Var = rownames(coef(sum))[-(1:2)],
                                        Est = coef(sum)[-(1:2), 1],
                                        SE = coef(sum)[-(1:2), 2],
                                        Dev = unname(sum$deviance),
                                        DfRes = unname(sum$df.residual),
                                        AIC = sum$aic)))
    }
    dat0 <- rbindlist(dats)
    
    
    ## Fixed effects models
    
    vars <- setdiff(names(repDat), c("journal", "coau", "obs"))
    ints <- paste0(setdiff(vars, "time"), ":time")
    fit <- glm.nb(
        paste("coau ~", paste(c(vars, ints), collapse = " + ")),
        data = repDat
    )
    sum <- summary(fit)
    # Save!
    dat1 <- data.table(Category = sc,
                       Model = "Fixed",
                       Var = rownames(coef(sum))[-1],
                       Est = coef(sum)[-1, 1],
                       SE = coef(sum)[-1, 2],
                       Dev = unname(sum$deviance),
                       DfRes = unname(sum$df.residual),
                       AIC = sum$aic)
    
    
    ## Random intercept & time slope models
    
    vars <- setdiff(names(repDat), c("journal", "coau", "obs"))
    ints <- paste0(setdiff(vars, "time"), ":time")
    fit <- glmer.nb(
        paste("coau ~",
              paste(c(vars, ints), collapse = " + "), "+ (time | journal)"),
        data = repDat
    )
    sum <- summary(fit)
    # Save!
    dat2 <- data.table(Category = sc,
                       Model = "Mixed",
                       Var = rownames(coef(sum))[-1],
                       Est = coef(sum)[-1, 1],
                       SE = coef(sum)[-1, 2],
                       Dev = unname(sum$AICtab["deviance"]),
                       DfRes = unname(sum$AICtab["df.resid"]),
                       AIC = sum$AICtab["AIC"])
    
    
    ## Append to list
    catsDats <- c(catsDats, list(dat0, dat1, dat2))
}

catsDat <- rbindlist(catsDats)


## Save!

save(catsDat, file = "calc/fitFreqCats.RData")

rm(list = ls())
