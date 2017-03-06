### Summary statistics, tables, and figures

# Source base code & choices
source("code/base.r")
source("code/choices.r")
source("code/specs.r")

for (pkg in c("gridExtra", "ineq", "xtable")) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    stopifnot(require(pkg, character.only = TRUE))
  }
}

load("calc/subDat.RData")
load("calc/measChoice.RData")


## Corporate authorship

# Print summary of corporate authorship
if (!excl_corporate) {
  cau_yr_sum <- aggregate(cau ~ yr, data = subDat, FUN = sum)
  cau_yr_prop <- aggregate(cau ~ yr, data = subDat, FUN = mean)
  cau_yr_mer <- merge(cau_yr_sum, cau_yr_prop,
                      by = "yr", suffixes = c("_sum", "_prop"))
  print(cau_yr_mer)
}


## Article frequency

# Table of article count per journal per year
nTab <- table(subDat[, .(issn, yr)])
# Summary stats
nDat <- data.frame(q1 = apply(nTab, 2, quantile, probs = .25),
                   median = apply(nTab, 2, median),
                   q3 = apply(nTab, 2, quantile, probs = .75),
                   mean = apply(nTab, 2, mean),
                   gini = apply(nTab, 2, Gini))
print(t(nDat))

# Table of ≥25-author article count per month
mDat <- aggregate(subDat$nau, by = list(as.numeric(subDat$ym)),
                  FUN = function(x) length(which(x >= 25)))
names(mDat) <- c("Month", "AuthorCount")
mPlot <- ggplot(data = mDat, aes(x = Month, y = AuthorCount)) +
  geom_point() +
  ylab("Number of articles by 26+ authors")
pdf(height = bodwid * .4, width = colwid,
    file = 'fig/nau26Plot.pdf')
plot(mPlot + theme_bw())
dev.off()


## Subject classification frequency

# Dataset of journals and their SCs
journalDat <- subDat[, .(issn, cats)]
journalDat[, cats_str := sapply(cats, paste, collapse = "|")]
journalDat[, cats := NULL]
journalDat <- unique(journalDat)
journalDat[, cats := strsplit(cats_str, "\\|")]
journalDat[, cats_str := NULL]
# Frequency table of SCs
scTab <- sort(table(unlist(journalDat$cats)), decreasing = TRUE)
scDat <- data.frame(Category = names(scTab),
                    No. = as.vector(unname(scTab)))
# Split close to middle
firsts <- which(scTab[-1] < scTab[-length(scTab)])
cap <- which.min(abs(firsts - length(scTab) / 2))
cut <- which(names(scTab) == names(cap))
# Captions
caption_pre <- "WoK Subject Classification frequencies ("
caption_mids <- paste0(c("greater than ", "at most "), unname(scTab[cut]))
caption_post <- ") for journals included in this study"
# Print tables
print(xtable(scDat[1:(cut - 1), ],
             caption = paste0(caption_pre, caption_mids[1], caption_post),
             align = c("r", "l","r")),
      size = "small",
      file = "tab/categories1.txt")
print(xtable(scDat[cut:nrow(scDat), ],
             caption = paste0(caption_pre, caption_mids[2], caption_post),
             align = c("r", "l","r")),
      size = "small",
      file = "tab/categories2.txt")


## Variance vs mean

# Coauthor count means and variances by month
mvDat <- subDat[, .(yr, ym, nau2)]
mvDat[, quarter := yr + floor(((ym %% 1) * 4)) / 4]
table(mvDat$ym - mvDat$quarter)
mvDat <- aggregate(mvDat$nau2, by = list(mvDat$quarter),
                   FUN = function(x) c(mean(x), var(x)))
mvDat <- as.data.frame(cbind(mvDat[, 1], mvDat[, 2]))
names(mvDat) <- c("Quarter", "Mean", "Variance")
mvDat <- as.data.table(mvDat)
setkey(mvDat, Quarter)
labDat <- subset(mvDat, Quarter %% 1 == 0)
labDat$Vjust <- c(1,1,1,-1,2,-1,2,-1,-1)
labDat$Hjust <- c(1,1,1,.5,0,0,.5,1,0) * (-.25)
mvPlot <- ggplot(data = mvDat, aes(x = Mean, y = Variance, label = Quarter)) +
  geom_path() +
  geom_point(data = labDat, shape = 1, size = 2) +
  geom_text(data = labDat, angle = 0, size = 2,
            aes(vjust = Vjust, hjust = Hjust)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ylim(range(c(mvDat$Mean, mvDat$Variance)))
pdf(height = bodwid * .6, width = colwid,
    file = "fig/mvPlot.pdf")
plot(mvPlot + theme_bw())
dev.off()


## Article count distribution (Poissonness and Ord plots)

# Maximum coauthor count to include
kmax <- 30
# Which years to plot
wh_yr <- range(subDat$yr)
# Array of distributions
coau_dists <- sapply(wh_yr, function(y) {
  sdat <- subDat[yr == y]
  ns <- tabulate(sdat[nau2 <= kmax]$nau2 + 1, nbins = kmax + 1)
  names(ns) <- 0:kmax
  ns
})
colnames(coau_dists) <- wh_yr
# Melt
coau_dat <- melt(coau_dists, id.vars = colnames(coau_dists))
colnames(coau_dat) <- c("k", "yr", "n_k")
coau_dat <- as.data.table(coau_dat)
# Poissonness plot response variable
coau_dat[, phi := log(n_k) + log(factorial(k)), by = "yr"]
coau_dat[, lambda := sum(k * n_k) / sum(n_k), by = "yr"]
coau_dat[, pfit := log(lambda) * k + log(sum(n_k)) - lambda, by = "yr"]
# Poissonness plots!
poisPlot <- ggplot(data = coau_dat, aes(x = k, y = phi)) +
  geom_point() +
  #    geom_abline(slope = log(lambda), intercept = log(sum(n_k)) - lambda) +
  geom_line(aes(x = k, y = pfit)) +
  geom_smooth(method = "lm", formula = y ~ x,
              size = .5, linetype = "dashed", se = FALSE, color = "black") +
  facet_wrap(~ yr, scales = "free_y", ncol = 1) +
  xlab("k = Number of coauthors") +
  ylab(expression(log(n[k])+log(Gamma(k)))) +
  theme_bw()
pdf(height = bodwid * .8, width = colwid,
    file = "fig/poisPlot.pdf")
print(poisPlot)
dev.off()
# Ord negative binomial response variables
coau_dat[, ord := k * n_k / shift(n_k, 1, type = "lag"), by = "yr"]
# Ord plots!
ordPlot <- ggplot(data = coau_dat, aes(x = k, y = ord)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              size = .5, linetype = "dashed", se = FALSE, color = "black") +
  facet_wrap(~ yr, scales = "free_y", ncol = 1) +
  xlab("k = Number of coauthors") +
  ylab(expression(kn[k]/n[k-1])) +
  theme_bw()
pdf(height = bodwid * .8, width = colwid,
    file = "fig/ordPlot.pdf")
print(ordPlot)
dev.off()


## Predictors

# Variables in subDat to be used in regression
ord <- c(4, 3, 2, 5, 6, 1)
vars <- setdiff(sapply(measures, function(x) x[1]), "ym")[ord]
# Key ranges for numerical variables
brklst <- list(nau = c(2, 5),
               npmt = c(2, 5),
               im = c(1, 3))

# Table of (largest) percentages of predictor values
yrs <- seq(min(subDat$yr), max(subDat$yr), 2)
yrTab <- do.call(cbind, lapply(yrs, function(y) {
  
  # Data
  dat <- subDat[yr == y]
  
  # Vector of proportions (and mean(sd) for numerical)
  vec <- unlist(lapply(vars, function(v) {
    
    # If binary, return proportion; if numerical, return mean and quartiles
    if (is.logical(dat[[v]])) {
      tab <- table(dat[[v]])
      names(tab) <- paste(v, names(tab))
      tab
    } else {
      brks <- brklst[[v]]
      tab <- table(cut(dat[[v]], breaks = c(-Inf, brks, Inf)))
      names(tab) <- paste(v, names(tab))
      tab
    }
  }))
  
  cbind(vec, round(vec / nrow(dat), 3))
}))
colnames(yrTab) <- paste(rep(yrs, each = 2), c("n", "p"))

# Character matrix
yrMat <- sapply(1:ncol(yrTab), function(j) {
  if (j %% 2 == 1) {
    paste0("\\(", yrTab[, j], "\\)")
  } else {
    paste0("\\(", sprintf("%.3f", yrTab[, j]), "\\)")
  }
})
rownames(yrMat) <- rownames(yrTab)
colnames(yrMat) <- rep(yrs, each = 2)

capt <- paste0("Biannual distributions of variables across all articles ",
               "in our sample.")
xtab <- xtable(yrMat, caption = capt, label = "summary",
               align = paste0("l",
                              paste(rep("r", 2 * length(yrs)), collapse = "")))
print(xtab, size = "\\small\\sf", floating.environment = "table*",
      sanitize.text.function = identity,
      file = "tab/summary.txt")
## EDIT AFTER PRINTING


# Countries of publication
#summary(subDat)
length(unique(subDat$issn))
for(col in c("co", "type")) {
  print(table(subDat[, col, with = FALSE][[1]]))
}


## Histograms of author count, stratified by interval

# Cup point
cup <- min(subDat$nau)
# Cap point
cap <- 12
# Dataset
nauDat <- subDat[, .(yr, nau)]
yr1 <- min(nauDat$yr)
nauDat[, fl := floor((yr - yr1) / 3) * 3 + yr1]
nauDat[, Interval := paste0(fl, "-", substr(fl + 2, 3, 4))]
# Cap
nauDat[nau > cap]$nau <- cap
nauDat[, nau := factor(nau, as.character(cup:cap))]
levels(nauDat$nau)[length(levels(nauDat$nau))] <- paste0(cap, "+")
# Aggregate count histogram
nauHist <- ggplot(nauDat, aes(nau)) + geom_bar() +
  xlab("Number of authors") + ylab("Number of articles")
pdf(height = bodwid * .4, width = colwid,
    file = 'fig/nauHist.pdf')
plot(nauHist + theme_bw() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(angle = 90, hjust = .5)))
dev.off()
# Stratified count histogram
strHist <- ggplot(nauDat, aes(nau, fill = Interval)) +
  geom_bar(position = "dodge") +
  xlab("Number of authors") + ylab("Number of articles")
pdf(height = bodwid * .4, width = colwid,
    file = 'fig/nauStrHist.pdf')
plot(strHist + theme_bw() +
       theme(legend.position = "top",
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(angle = 90, hjust = .5)) +
       guides(fill = guide_legend(title = NULL)))
dev.off()


## Histograms of top-level MeSH term count, stratified by interval

# Cup point
cup <- min(subDat$nmh)
# Cap point
cap <- 30
# Dataset
nmhDat <- subDat[, .(yr, nmh)]
yr1 <- min(nmhDat$yr)
nmhDat[, fl := floor((yr - yr1) / 3) * 3 + yr1]
nmhDat[, Interval := paste0(fl, "-", substr(fl + 2, 3, 4))]
# Cap
nmhDat[nmh > cap]$nmh <- cap
nmhDat[, nmh := factor(nmh, as.character(cup:cap))]
levels(nmhDat$nmh)[length(levels(nmhDat$nmh))] <- paste0(cap, "+")
# Aggregate count histogram
nmhHist <- ggplot(nmhDat, aes(x = nmh)) +
  geom_bar() +
  xlab("Number of top-level MeSH terms") + ylab("Number of articles")
pdf(height = bodwid * .4, width = bodwid,
    file = 'fig/nmhHist.pdf')
plot(nmhHist + theme_bw() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(angle = 90, hjust = .5)))
dev.off()
# Stratified count histogram
strHist <- ggplot(nmhDat, aes(x = nmh, fill = Interval)) +
  geom_bar(position = "dodge") +
  xlab("Number of top-level MeSH terms") + ylab("Number of articles")
pdf(height = bodwid * .4, width = bodwid,
    file = 'fig/nmhStrHist.pdf')
plot(strHist + theme_bw() +
       theme(legend.position = "top",
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(angle = 90, hjust = .5)) +
       guides(fill = guide_legend(title = NULL)))
dev.off()


## Histograms of major MeSH term count, stratified by interval

# Cup point
cup <- min(subDat$npmt)
# Cap point
cap <- 9
# Dataset
npmtDat <- subDat[, .(yr, npmt)]
yr1 <- min(npmtDat$yr)
npmtDat[, fl := floor((yr - yr1) / 3) * 3 + yr1]
npmtDat[, Interval := paste0(fl, "-", substr(fl + 2, 3, 4))]
# Cap
npmtDat[npmt > cap]$npmt <- cap
npmtDat[, npmt := factor(npmt, as.character(cup:cap))]
levels(npmtDat$npmt)[length(levels(npmtDat$npmt))] <- paste0(cap, "+")
# Aggregate count histogram
npmtHist <- ggplot(npmtDat, aes(x = npmt)) +
  geom_bar() +
  xlab("Number of major MeSH terms") + ylab("Number of articles")
pdf(height = bodwid * .4, width = colwid,
    file = 'fig/npmtHist.pdf')
plot(npmtHist + theme_bw() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(angle = 90, hjust = .5)))
dev.off()
# Stratified count histogram
strHist <- ggplot(npmtDat, aes(x = npmt, fill = Interval)) +
  geom_bar(position = "dodge") +
  xlab("Number of major MeSH terms") + ylab("Number of articles")
pdf(height = bodwid * .4, width = colwid,
    file = "fig/npmtStrHist.pdf")
plot(strHist + theme_bw() +
       theme(legend.position = "top",
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.y = element_text(angle = 90, hjust = .5)) +
       guides(fill = guide_legend(title = NULL)))
dev.off()


## Line plots for six categories of funding, review articles, and research level

# Data frame by research type and funding
gtDat <- dcast(subDat, floor(ym) ~ trial + rev + !supp,
               fun.aggregate = length, value.var = "nau2")
labs <- c("Other, supported", "Other, unsupported",
          "Review, supported", "Review, unsupported",
          "Trial, supported", "Trial, unsupported")

pdf(height = bodwid * .6, width = colwid,
    file = "fig/fundingLevelPlot.pdf")
# Plot window
plot(x = c(), y = c(), log = "y",
     xlim = range(as.numeric(gtDat$ym)),
     ylim = range(as.matrix(gtDat[, 2:7, with = FALSE])),
     xlab = "Year", ylab = "Number of articles")
# Lines
for(i in 1:(ncol(gtDat) - 1)) {
  lines(x = as.numeric(gtDat$ym), y = gtDat[[i + 1]],
        lty = ncol(gtDat) - i)
}
# Legend
legend(x = min(subDat$yr), y = 18000, legend = labs,
       lty = (ncol(gtDat) - 1):1, pch = NA, box.lty = 0)
dev.off()


## Distribution of impact factor over time

# Subset by journal
imDat <- subDat[, .(issn, yr, im)]
setkey(imDat)
imDat <- unique(imDat)
# Range of values
yran <- range(log(imDat$im), na.rm = TRUE)
# Boxplots
pdf(height = bodwid * .5, width = colwid,
    file = "fig/imBoxplot.pdf")
boxplot(log(im) ~ yr, data = imDat,
        xlab = "Year", ylab = "Log(Impact factor)",
        outline = FALSE, ylim = yran,
        pch = 16, cex = .5)
stripchart(log(im) ~ yr, vertical = TRUE, data = imDat,
           method = "jitter", jitter = .2, add = TRUE, pch = 16, cex = .25)
dev.off()


## Distribution of journal mean major MeSH term count over time

# Journal-wise dataset
issnDat <- as.data.table(dcast(subDat, issn + yr ~ .,
                               fun.aggregate = mean, value.var = "nau2"))
setnames(issnDat, ".", "nau2")
# Range of values
yran <- range(issnDat$nau2)
# Boxplots
pdf(height = bodwid * .6, width = colwid,
    file = "fig/issnBoxplot.pdf")
boxplot(nau2 ~ yr, data = issnDat,
        xlab = "Year", ylab = "Coauthor count",
        outline = FALSE, ylim = yran, log = "y",
        pch = 16, cex = .5)
stripchart(nau2 ~ yr, vertical = TRUE, data = issnDat,
           method = "jitter", jitter = .2, add = TRUE, pch = 16, cex = .25)
dev.off()


## Distribution of mean coauthor count by journal

# Journal-wise dataset
issnDat <- as.data.table(dcast(subDat, issn + yr ~ .,
                               fun.aggregate = mean, value.var = "nau2"))
yr1 <- min(issnDat$yr)
issnDat[, fl := floor((yr - yr1) / 3) * 3 + yr1]
issnDat[, Interval := paste0(fl, "-", substr(fl + 2, 3, 4))]
# Factorize binned mean coauthor counts
issnDat[, bin := cut(., breaks = c(seq(0, 7, .5), Inf),
                     labels = c(seq(0, 6.5, .5), "7+"), right = FALSE)]
# Aggregate journal histogram
issnHist <- ggplot(issnDat, aes(x = bin)) +
  geom_bar() +
  xlab("Mean coauthor count") + ylab("Number of journals")
pdf(height = bodwid * .4, width = colwid,
    file = 'fig/issnHist.pdf')
plot(issnHist + theme_bw())
dev.off()
# Stratified journal histogram
strHist <- ggplot(issnDat, aes(x = bin, fill = Interval)) +
  geom_bar(position = "dodge") +
  xlab("Mean coauthor count") + ylab("Number of journals")
pdf(height = bodwid * .4, width = colwid,
    file = 'fig/issnStrHist.pdf')
plot(strHist + theme_bw() + theme(legend.position = "top") +
       guides(fill = guide_legend(title = NULL)))
dev.off()


## Journal trends

# Sample of ISSNs
issnIncl <- sample(unique(subDat$issn), 64)
issnDat <- subDat[issn %in% issnIncl, .(issn, yr, nau)]
issnDat <- aggregate(issnDat$nau, by = issnDat[, .(issn, yr)],
                     FUN = function(x) c(length(x), mean(x)))
issnDat <- as.data.frame(cbind(issnDat[, 1:2], issnDat[, 3]))
names(issnDat) <- c("ISSN", "Year", "Articles", "Authors")
# Plot
bwPlot <- ggplot(data = issnDat,
                 aes(x = Year, y = Authors, fill = ISSN, colour = Articles)) +
  geom_line() +
  scale_colour_gradient(low = "#eeeeee", high = "#000000", trans = "sqrt") +
  scale_x_continuous(breaks = seq(2000, 2006, 3)) +
  theme_bw()
colPlot <- ggplot(data = issnDat,
                  aes(x = Year, y = Authors, fill = ISSN)) +
  geom_line(aes(colour = ISSN)) +
  guides(colour = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2006, 3)) +
  theme_bw()
pdf(height = bodwid * .6, width = bodwid,
    file = "fig/issnPlot.pdf")
grid.arrange(colPlot, bwPlot, ncol = 2, widths = c(1, 1.2)) + theme_bw()
dev.off()


rm(list = ls())
