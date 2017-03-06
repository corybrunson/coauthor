### Subset data

# Source base code & choices
source("code/base.r")
source("code/choices.r")
source("code/specs.r")

load("calc/merDat.RData")
print(range(merDat$yr))

# Begin journal & article counts
merCount <- matrix(c(length(unique(merDat$issn)), nrow(merDat)), ncol = 1)
rownames(merCount) <- c("journals", "articles")
colnames(merCount) <- "start"


## Variable transformations

# Logical variable for grant
merDat[, g := ng > 0]


## Restrict to journals with all bibliometrics defined for as long as possible

# Bibliometrics
stats <- if (wos_window) "im" else c("im", "sjr", "cpa")
# Tables of rates at which each is defined
tfTables <- lapply(stats, function(stat) {
    tab <- sapply(years, function(year) {
        table(c(is.na(merDat[yr == year,
                             stat, with = FALSE]), TRUE, FALSE)) - 1
    })
    colnames(tab) <- years
    tab
})
names(tfTables) <- stats

# Start with first year bibliometrics are defined
yrStart <- years[max(sapply(tfTables, function(tab) {
    min(which(tab[1, ] > 0))
}))]
merDat <- merDat[yr >= yrStart]


## Indicator for inclusion in bibliometric datasets (always or never)

if (incl_indicator) {
    
    # Proportions of years for which a journal has each bibliometric
    aggs <- lapply(stats, function(stat) {
        dat <- unique(merDat[, c("issn", "yr", stat), with = FALSE])
        agg <- aggregate(!is.na(dat[[stat]]), by = list(dat$issn), FUN = mean)
        hist(agg[, 2])
        agg <- as.data.table(agg)
        setnames(agg, names(agg), c("issn", paste0(stat, "_avail")))
        agg
    })
    
    # Merge
    for (agg in aggs) {
        merDat <- merge(merDat, agg, by = "issn", all.x = TRUE, all.y = FALSE)
    }
    
    # Restrict to journals for which proportion is 0 or 1
    is_binary <- apply(merDat[, paste0(stats, "_avail"), with = FALSE], 1,
                       function(x) all(x %% 1 == 0))
    merDat <- merDat[is_binary]
    
    # Logicize
    for (x in paste0(stats, "_avail")) {
        merDat[[x]] <- as.logical(merDat[[x]])
    }
    
    # Print proportions
    print(table(merDat[, rev(paste0(stats, "_avail")), with = FALSE]))
    
} else {
    
    # Require stats to be defined
    is_def <- !is.na(rowSums(merDat[, stats, with = FALSE]))
    merDat <- merDat[is_def]
    
}

# Update journal & article counts
merCount <- cbind(merCount,
                  bibliometrics = c(length(unique(merDat$issn)), nrow(merDat)))


## Article frequency

# Require at least 30 articles in each journal each year
issnYrTab <- table(merDat[, .(issn, yr)])
issnKeep <- rownames(issnYrTab)[apply(
    issnYrTab,
    1,
    function(row) all(row > 30)
)]
merDat <- merDat[issn %in% issnKeep]

# Update journal & article counts
merCount <- cbind(merCount,
                  continuity = c(length(unique(merDat$issn)), nrow(merDat)))


## Further cleaning (based on visualizations and analysis needs)

# Variable for number of "extra" authors
merDat[, nau2 := nau - 1]
# Journal frequency (requires date data, not just month/year)

# Quantiles of journal volume
ny <- length(unique(merDat$yr))
merDat[, vol := length(nau) / ny,
       by = list(issn)] # relatively little variance by yr
# Logical variable for a unique MeSH heading
merDat[, umh := nmh == 1]

# Logical variable for Nature Group journals
load("calc/jids.RData")
Nats <- jids[grepl("^Nature", JournalTitle)][,
                                             c("JournalTitle", "ISSN (Print)"),
                                             with = FALSE]
nats <- setdiff(Nats$"ISSN (Print)",
                c("0191-2941", "1342-0054", "0369-3392", "2082-0887"))
merDat[, nat := issn %in% nats]

if (!incl_indicator) {
    
    # Numerical frequency variable (REQUIRES THOMSON-REUTERS DATA)
    freq.peryr <- c(Quarterly = 4,
                    Monthly = 12, Semimonthly = 24, Bimonthly = 6,
                    Weekly = 52.18, Biweekly = 26.09)
    # ...as a data frame
    freqDat <- data.frame(freq = names(freq.peryr), peryr = unname(freq.peryr))
    # Merge into merDat
    merDat <- merge(merDat, freqDat, by = "freq", all.x = TRUE, all.y = FALSE)
    # Restrict to journals with (known) frequency >6 and ≤24 per year
    merDat <- merDat[peryr > 6 & peryr <= 24]
    
    # Update journal & article counts
    merCount <- cbind(merCount,
                      frequency = c(length(unique(merDat$issn)), nrow(merDat)))
    
}

print(merCount)


## Identify outlying journals

# Mean coauthor count versus impact factor
dat <- dcast(merDat[!is.na(im), .(im, nau2, issn, yr)], issn + yr + im ~ .,
             fun.aggregate = mean, value.var = "nau2")

# Distribution of review articles across journals
tab <- table(merDat[, .(issn, rev)])
# Logit-proportions of each type
logitProps <- qlogis(tab / rowSums(tab))

# Which journals publish > 50% review articles?
wh_rev <- rownames(tab)[which(
    tab[, "TRUE"] / rowSums(tab) > .5
)]
# Nature journals
wh_nat <- unique(merDat[nat == TRUE]$issn)
# Scientific American
wh_sciam <- "0036-8733"

# Plot!
pdf(height = bodwid * .6667, width = colwid, file = "fig/coauImPlot.pdf")
plot(x = c(), y = c(),
     xlim = range(dat$im, na.rm = TRUE),
     ylim = range(setdiff(dat$., 0)),
     log = "xy",
     xlab = "Impact factor", ylab = "Mean coauthor count",
     main = "Coauthorship and citation across journals")
points(x = dat[!(dat$issn %in% unique(c(wh_rev, wh_nat, wh_sciam))), ]$im,
       y = dat[!(dat$issn %in% unique(c(wh_rev, wh_nat, wh_sciam))), ]$.,
       pch = 16, cex = .4)
points(x = dat[dat$issn %in% wh_rev, ]$im,
       y = dat[dat$issn %in% wh_rev, ]$.,
       pch = 1, cex = .8)
points(x = dat[dat$issn %in% wh_nat, ]$im,
       y = dat[dat$issn %in% wh_nat, ]$.,
       pch = 0, cex = 1.1)
points(x = dat[dat$issn %in% wh_sciam, ]$im,
       y = dat[dat$issn %in% wh_sciam, ]$.,
       pch = 2, cex = .9)
legend("topleft", legend = c("Reviews (>50%)", "Nature", "SciAm"),
       pch = c(1, 0, 2), pt.cex = c(.8, 1.1, .9), box.lty = 0)
dev.off()

# Remove Scientific American
merDat <- merDat[issn != "0036-8733"]

# Rename
subDat <- merDat

# Save...
save(subDat, file = "calc/subDat.RData")


rm(list = ls())
