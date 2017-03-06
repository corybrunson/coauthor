### Transform regressors in order to linearize relationships with author count

# Source base code & choices
source("code/base.r")
source("code/choices.r")

options(scipen = 99, digits = 8)

load("calc/subDat.RData")


## Test for correlations among predictors

# Function to numerize cng (levels 0, 1, 2)
unfactorFun <- function(x) {
  x <- as.character(x)
  ifelse(grepl("0", x), 0, ifelse(grepl("1|2", x), 1, 2))
} ^ 2

# Scatterplot with correlation coefficient
vsPlot <- function(var1, var2, fun1 = identity, fun2 = identity) {
  dat <- data.frame(fun1(subDat[, var1, with = FALSE][[1]]),
                    fun2(subDat[, var2, with = FALSE][[1]]))
  colnames(dat) <- c("x", "y")
  dat$x <- as.numeric(dat$x)
  dat$y <- as.numeric(dat$y)
  # Integer-valued?
  xInt <- all(as.integer(dat$x) - dat$x == 0)
  yInt <- all(as.integer(dat$y) - dat$y == 0)
  plot <- ggplot(data = dat, aes(x = x, y = y)) +
    geom_point(alpha = 0.05,
               position = if(xInt | yInt) "jitter" else "identity") +
    geom_point(data = aggregate(dat$y, by = list(dat$x),
                                FUN = function(x) mean(x, na.rm = TRUE)),
               aes(x = Group.1, y = x, colour = "red", size = 2))
  plot <- plot + xlab(var1) + ylab(var2)
  R2 <- round(cor(
    dat$x[!is.infinite(dat$x)],
    dat$y[!is.infinite(dat$x)],
    use = "complete.obs"
  ) ^ 2, 4)
  plot <- plot +
    annotate("text",
             x = min(dat$x[!is.infinite(dat$x)], na.rm = TRUE) +
               .2 * diff(range(dat$x[!is.infinite(dat$x)], na.rm = TRUE)),
             y = max(dat$y[!is.infinite(dat$x)], na.rm = TRUE) -
               .1 * diff(range(dat$y[!is.infinite(dat$x)], na.rm = TRUE)),
             label = paste("R^2 =", R2))
  print(paste("R^2 =", R2))
  plot
}

if (FALSE) {
  
  print(vsPlot("ym", "cr"))
  print(vsPlot("ym", "trial"))
  
  print(vsPlot("ym", "ng"))
  
  print(vsPlot("ym", "cpa", fun2 = log))
  print(vsPlot("ym", "im", fun2 = log))
  print(vsPlot("ym", "sjr", fun2 = log))
  # Recommendation: regress cpa, im, sjr by time
  
  print(vsPlot("ym", "npmt", fun2 = anscombeFun))
  print(vsPlot("ym", "nmh", fun2 = anscombeFun))
  
  print(vsPlot("cr", "ng"))
  print(vsPlot("trial", "ng"))
  
  print(vsPlot("cr", "cpa", fun2 = log))
  print(vsPlot("cr", "im", fun2 = log))
  print(vsPlot("cr", "sjr", fun2 = log))
  print(vsPlot("trial", "cpa", fun2 = log))
  print(vsPlot("trial", "im", fun2 = log))
  print(vsPlot("trial", "sjr", fun2 = log))
  
  print(vsPlot("cr", "npmt", fun2 = function(x) atan(x / 4)))
  print(vsPlot("cr", "nmh", fun2 = function(x) atan(x / 10)))
  print(vsPlot("trial", "npmt", fun2 = function(x) atan(x / 4)))
  print(vsPlot("trial", "nmh", fun2 = function(x) atan(x / 10)))
  
  print(vsPlot("ng", "cpa", fun2 = log))
  print(vsPlot("ng", "im", fun2 = log))
  print(vsPlot("ng", "sjr", fun2 = log))
  
  print(vsPlot("ng", "npmt", fun2 = anscombeFun))
  print(vsPlot("ng", "nmh", fun2 = anscombeFun))
  
  print(vsPlot("cpa", "npmt", log, anscombeFun))
  print(vsPlot("cpa", "nmh", log, anscombeFun))
  
  # Concurrent validity
  
  print(vsPlot("cpa", "im", log, log))
  print(vsPlot("cpa", "sjr", log, log))
  print(vsPlot("im", "sjr", log, log))
  print(vsPlot("npmt", "nmh", anscombeFun, anscombeFun))
  
}


## Mean author count versus transforms of each predictor

idPlot <- function(var, fun, N = 20) {
  dat <- data.frame(fun(subDat[, var, with = FALSE][[1]]),
                    subDat[, "nau2", with = FALSE][[1]])
  colnames(dat) <- c("x", "n")
  dat$x <- as.numeric(dat$x)
  dat <- dat[!is.na(dat$x) & !is.infinite(dat$x), ]
  # Bin continuous variables
  xInt <- all(as.integer(subDat$var) - subDat$var == 0)
  dat$x <- if(xInt) {
    dat$x
  } else {
    cut(dat$x, breaks = N, ordered_result = TRUE)
  }
  # Aggregate data
  agg <- aggregate(dat$n, by = list(dat$x), FUN = mean)
  colnames(agg) <- c("x", "m")
  # Plot
  plot <- ggplot(data = agg, aes(x = x, y = m)) +
    geom_point() +
    xlab(var) + ylab("mean(nau2)")
  # Explained variance
  R2 <- round(cor(as.numeric(agg$x), agg$m, use = "complete.obs") ^ 2, 4)
  print(paste("R^2 =", R2))
  plot <- plot +
    annotate("text",
             x = min(as.numeric(agg$x), na.rm = TRUE) +
               .2 * diff(range(as.numeric(agg$x), na.rm = TRUE)),
             y = max(agg$m, na.rm = TRUE) -
               .1 * diff(range(agg$m, na.rm = TRUE)),
             label = paste("R^2 =", R2))
  list(R2 = R2, plot = plot)
}

logPlot <- function(var, fun, N = 20) {
  dat <- data.frame(fun(subDat[, var, with = FALSE][[1]]),
                    subDat[, "nau2", with = FALSE][[1]])
  colnames(dat) <- c("x", "n")
  dat$x <- as.numeric(dat$x)
  dat <- dat[!is.na(dat$x) & !is.infinite(dat$x), ]
  # Bin continuous variables
  xInt <- all(as.integer(subDat$var) - subDat$var == 0)
  dat$x <- if(xInt) {
    dat$x
  } else {
    cut(dat$x, breaks = N, ordered_result = TRUE)
  }
  # Aggregate data
  agg <- aggregate(dat$n, by = list(dat$x), FUN = mean)
  colnames(agg) <- c("x", "m")
  # Plot
  plot <- ggplot(data = agg, aes(x = x, y = log(m))) +
    geom_point() +
    xlab(var) + ylab("log(mean(nau2))")
  # Explained variance
  R2 <- round(cor(as.numeric(agg$x), log(agg$m), use = "complete.obs") ^ 2, 4)
  print(paste("R^2 =", R2))
  plot <- plot +
    annotate("text",
             x = min(as.numeric(agg$x), na.rm = TRUE) +
               .2 * diff(range(as.numeric(agg$x), na.rm = TRUE)),
             y = max(log(agg$m), na.rm = TRUE) -
               .1 * diff(range(log(agg$m), na.rm = TRUE)),
             label = paste("R^2 =", R2))
  list(R2 = R2, plot = plot)
}

if (FALSE) {
  
  # Breadth, funding, and prestige
  nauFuns <- c(id = idPlot, log = logPlot)
  varFuns <- c(id = identity, sqrt = sqrt, anscombe = anscombeFun,
               log = log, log1 = logPlus1)
  varMeas <- list(breadth = c(npmt = "npmt", nmh = "nmh"),
                  funding = c(ng = "ng"),
                  prestige = c(cpa = "cpa", im = "im", sjr = "sjr"))
  funDat <- data.frame()
  for (k in 1:length(varMeas)) {
    for (i in 1:length(nauFuns)) {
      for (j in 1:length(varFuns)) {
        for (l in 1:length(varMeas[[k]])) {
          res <- nauFuns[[i]](varMeas[[k]][l], varFuns[[j]], 20)
          print(res$plot)
          funDat <- rbind(funDat, data.frame(
            nauFun = names(nauFuns)[i],
            varFun = names(varFuns)[j],
            var = names(varMeas)[k],
            varMeas = names(varMeas[[k]])[l],
            R2 = res$R2
          ))
        }
      }
    }
  }
  # Only difference in preference between link functions may be for breadth
  
}


## Best & alternate measures of each variable

# List of measures to use for each variable
measures <- list(
  coau = list("nau"),
  time = list("ym"),
  funding = list("supp", "g"),
  review = list("rev"),
  trial = list("trial"),
  breadth = list("npmt", "nmh"),
  prestige = if (wos_window) list("im") else list("im", "sjr", "cpa")
)

# List of transformations to perform on each measure
transforms <- list(
  coau = list(function(x) x - 1),
  time = list(identity),
  funding = list(identity, identity),
  review = list(identity),
  trial = list(identity),
  breadth = list(logPlus1, logPlus1),
  prestige = replicate(n = length(measures$prestige),
                       expr = log)
)

if (incl_indicator) {
  # Append measures and transformations for bibliometric inclusion indicators
  measures <- c(measures,
                list(viz = as.list(paste0(measures$prestige, "_avail"))))
  transforms <- c(transforms,
                  list(viz = replicate(n = length(measures$viz),
                                       expr = identity)))
}

# Save
save(measures, transforms, file = "calc/measChoice.RData")

rm(list = ls())
