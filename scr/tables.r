### Generate tables

# Source base code & choices
source("code/base.r")
source("code/choices.r")
source("code/specs.r")

for (pkg in c("data.table", "xtable2")) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    stopifnot(require(pkg, character.only = TRUE))
  }
}
options(scipen = 9, digits = 8)


## Journal-level random effects

# Load models
load("calc/fitFreq2.RData")
# Data frame of standardized random effects
ranefDat <- ranef(fit)$journal
ranefSds <- apply(ranefDat, 2, sd)
ranefStd <- sweep(ranefDat, 2, ranefSds, "/")
ranefStd$magnitude <- apply(ranefStd, 1, function(x) sum(x ^ 2))
# ISSN as a variable
ranefStd$issn <- rownames(ranefStd)
rownames(ranefStd) <- NULL
# Load journal data
load("calc/jids.RData")
jidDat <- jids[, .(`ISSN (Print)`, MedAbbr)]
setnames(jidDat, 1:2, c("issn", "abbr"))
ranefStd <- merge(ranefStd, jidDat, by = "issn", all.x = TRUE, all.y = FALSE)
# Sort by sum of squares
ranefStd <- ranefStd[order(ranefStd$magnitude, decreasing = TRUE), ]
# De-standardize random effects and arrange for printing
ranefDat <- sweep(ranefStd[, c("(Intercept)", "time")], 2, ranefSds, "*")
rownames(ranefDat) <- ranefStd$abbr
setnames(ranefDat, 1:2, c("Intercept", "Date of publication"))
# Print table
ranefCap <- paste(
  "Twenty journals with the largest random effects.",
  "The journals are ranked by the sum of squares of their standardized effects."
)
print(xtable(ranefDat[1:20, ],
             caption = ranefCap,
             align = c("l", "r","r")),
      size = "small",
      file = "tab/journals.txt")

rm(list = ls())
