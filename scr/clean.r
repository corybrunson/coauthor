### Construct data tables for the PubMed

# Additional packages
library(R.utils)

# Source base code & choices
source("code/base.r")
source("code/choices.r")

## PubMed

# File with date and size info for each file
info.file <- paste0(datadir, "medline/medLineFileInfo.csv")
# Read into a data.frame
file.info <- read.csv(info.file,
                      nrows = countLines(info.file) - 3)
# Construct a two-column data.frame with begin and end years for each file
file.rans <- data.frame(
  begin = as.numeric(as.character(
    gsub('-[0-9]{4}$', '', file.info$Publication.Years)
  )),
  end = as.numeric(as.character(
    gsub('^[0-9]{4}-', '', file.info$Publication.Years)
  ))
)

# Determine first and last PubMed files for data from a given year
year.files <- function(year) {
  which(file.rans$begin <= year & file.rans$end >= year)
}

# Count the instances of a character string in a vector or list of strings
count.instances <- function(x, pattern) {
  if(mode(x) == 'list') x <- unlist(x)
  stopifnot(mode(x) == 'character')
  sapply(strsplit(paste0(x, pattern),
                  split = pattern, fixed = TRUE), length) - 1
}

# Convert read.pubmed data to different data.table format
dat2dat <- function(dat) {
  dat$pmid <- as.numeric(as.character(dat$pmid))
  # author counts
  dat[, nau := count.instances(as.character(au), '|') + 1]
  {
  wh <- which(dat$nau == 0)
  if(length(wh) > 0) {
    warning(paste("Zero author counts:", str(wh)))
  }
  nfau <- count.instances(as.character(dat$fau), "|") + 1
  wh <- which(dat$nau != nfau)
  if(length(wh) > 0) {
    warning(paste("Different author counts:", str(wh)))
  }
  }
  # corporate authors
  dat[, cau := grepl("(^|\\|) (\\||$)", as.character(au))]
  # year of publication
  dat[, yr := as.numeric(substr(dp, 1, 4))]
  # fields to keep
  dat[, c(1, 16, 4, 5, 8, 9, 18, 11, 14, 15, 17), with = FALSE]
}

# Identify the files relevant to the years of the study
files <- paste0(datadir, "medline/medline12n",
                sprintf("%04d",
                        sort(unique(unlist(lapply(years, year.files))))),
                ".xml.txt")
# Intersect with extant files
wh <- which(file.exists(files))

# Initialize data table
pmDat <- data.table()

# For each file...
for(file in files[wh]) {
  
  print(file)
  dat <- read.pubmed(file)
  dat <- dat2dat(dat)
  
  # Remove publications from unwanted years
  dat <- subset(dat, yr >= years[1] & yr <= years[length(years)])
  
  # Restrict to journal articles
  dat <- dat[grep("Journal Article", pt)]
  
  # Remove non-research articles
  excl.pt <- paste("Editorial", "Letter", "Narrative", "Historical",
                   "Addresses", "Comment", "Biography", "Guideline",
                   "Directory", "Handout", "News",
                   collapse = "|")
  dat <- dat[!grepl(excl.pt, pt)]
  # Remove scientific integrity reviews (which are not review articles)
  dat <- dat[!grepl("Scientific Integrity Review", pt)]
  
  # Append to pmDat
  pmDat <- rbindlist(list(pmDat, dat))
}

# Save (delete once whole script works)
save(pmDat, file = "calc/pmDat0.RData")

# To avoid artifacts of incompleteness, stop at first year of decline
yrTab <- table(pmDat$yr)
yrMax <- max(which(diff(yrTab) > 0)) + 1
pmDat <- pmDat[yr <= as.numeric(names(yrTab)[yrMax])]

# Clean country field
pmDat$co <-  gsub("\\b(\\w)", "\\U\\1", tolower(pmDat$co), perl = TRUE)
pmDat$co[grep("^China \\(", pmDat$co)] <- "China"
pmDat$co[grep("^Russia \\(", pmDat$co)] <- "Russia"
pmDat[co == "Korea" | co == "Korea (South)"]$co <- "Korea"

# Calculate new fields (from fields to be omitted)
# Type of article
pmDat[, rev := grepl("Review", pt)]
pmDat[, trial := grepl("Trial", pt)]
#pmDat[, cr := grepl("Case Report", pt)]
#pmDat[, comp := grepl("Comparative Study", pt)]
#pmDat[, mc := grepl("Multicenter", pt)]
# MeSH assignments
pmDat[, npmt := count.instances(as.character(mh), '(Y)')]
pmDat[, nmh := count.instances(as.character(mh), '|') + 1]
# Research support
pmDat[, supp := grepl("Research Support", pt)]
#pmDat[, gov := grepl("Research Support, (U\\.S\\.|N\\.I\\.H\\.)", pt)]
#pmDat[, non := grepl("Research Support, Non-", pt)]
# Number of grants
pmDat[, ng := count.instances(as.character(gr), "|") + 1]
pmDat[gr == ""]$ng <- 0

# Calculate yearmon
pmDat[, ms := substr(dp, 6, 8)]
pmDat[ms == "Aut"]$ms <- "Fal"
pmDat[!(ms %in% c(month.abb, c("Win", "Spr", "Sum", "Fal")))]$ms <- NA
pmDat[, ym := as.yearmon(paste(ms, yr))]
pmDat <- pmDat[!is.na(ym)]

# Keep only print ISSNs that match exactly with unique journal identifiers
pmDat <- pmDat[grep("Print", is)]  # Lose many journals (<5% of articles)
uniqDat <- unique(pmDat[, .(jid, is)])
jidTab <- table(uniqDat$jid)
isTab <- table(uniqDat$is)
pmDat <- pmDat[!(jid %in% names(jidTab)[jidTab > 1]) &
                 !(is %in% names(isTab)[isTab > 1])]
print(paste("Journals with unique print ISSNs:", length(unique(pmDat$jid))))

# Restrict to journals with at least 30 articles each year
#pmDat[, apy := length(pmid), by = list(jid, yr)]
#pmDat <- pmDat[apy >= 30]
#print(paste("Journals with 30 articles per year:", length(unique(pmDat$jid))))

# Restrict to desired fields
setnames(pmDat, "is", "issn")
pmDat <- pmDat[, .(pmid, jid, issn, ym, nau, cau, rev, trial,
                   npmt, nmh, supp, ng, co)]
# Set keys
setkey(pmDat, pmid, jid, issn, ym)

# Save!
save(pmDat, file = "calc/pmDat.RData")

rm(list = ls())
