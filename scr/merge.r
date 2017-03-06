### Merge the PubMed data with the metrics data

# Source base code & choices
source("code/base.r")
source("code/choices.r")


## Aggregate the PubMed data by journal and year

load("calc/pmDat.RData")


## Bind the PubMed data with the Web of Science and SCImago data

# Exclude the corporate-authored articles (if electing to do so)
if (excl_corporate) {
  pmDat <- pmDat[cau == FALSE]
}

# Remove "(Print)" from ISSN field
pmDat$issn <- gsub(" \\(Print\\)$", "", pmDat$issn)
pmDat[, yr := as.numeric(floor(ym))]
setkey(pmDat, pmid, jid, issn, yr)

# Load Web of Science data
load("calc/wosDat.RData")
# Remove title field
wosDat[, title := NULL]
# Merge Web of Science data
pmDat <- merge(pmDat, wosDat, by = c("issn", "yr"), all.x = TRUE)

# Load SCImago data
load("calc/sjrDat.RData")
# Remove title field
sjrDat[, title := NULL]
# Merge Web of Science data
pmDat <- merge(pmDat, sjrDat, by = c("issn", "yr"), all.x = TRUE)

# Save!
merDat <- pmDat
print(unique(merDat$yr))
save(merDat, file = "calc/merDat.RData")

rm(list = ls())
