### Combine the batches of prestige files into a single file for each metric

# Source base code
source("code/base.r")
# Load packages from GitHub
if (!require(readxl)) {
  install_github("hadley/readxl")
  stopifnot(require(readxl))
}

## Thomson-Reuters journal rankings
# https://jcr.incites.thomsonreuters.com

# List files
wosFiles <- list.files(path = paste0(datadir, "jcr"),
                       pattern = "jcr[0-9]{4}.csv")
# Read all csvs into a list of data.tables
wosLst <- lapply(wosFiles, function(file) {
    # Read the csv into a data.table
    dat <- as.data.table(read.csv(
        paste0(datadir, "jcr/", file),
        na.strings = c("NA", "Not Available"),
        colClasses = "character"
    ))
    dat <- dat[-nrow(dat)]
    dat$X <- NULL; dat$Rank <- NULL;
    dat$X5.Year.Impact.Factor <- NULL;
    dat$Eigenfactor.Score <- NULL;
    setnames(dat, 1:4, c("title", "issn", "cites", "im"))
    dat[, cites := as.numeric(gsub(",", "", cites))]
    dat[, im := as.numeric(im)]
    dat[, yr := as.numeric(gsub("^jcr([0-9]*).csv$", "\\1", file))]
    setkey(dat, issn, yr)
    dat
})
# Bind them into a single data.table
wosDat <- rbindlist(wosLst)

# Remove nonsense ISSNs
wosDat <- wosDat[!(issn %in% c("", "*********", "****-****"))]

# Save!
setkey(wosDat, issn, yr)
wosDat <- unique(wosDat)


## Make a data.table from the subject categories files

# Function: Convert to title case
# http://stackoverflow.com/a/15778614/4556798
properCase <- function(x) {
    # Makes Proper Capitalization out of a string or collection of strings. 
    sapply(x, function(strn)
    { s <- strsplit(strn, "\\s")[[1]]
      paste0(toupper(substring(s, 1,1)), 
             tolower(substring(s, 2)),
             collapse=" ")}, USE.NAMES=FALSE)
}

# List files obtained from
# http://ip-science.thomsonreuters.com/cgi-bin/jrnlst/jlsubcatg.cgi?PC=K
scFiles <- list.files(path = paste0(datadir, "SCI"), pattern = "[A-Z]{2}.txt")
# Iteratively construct data.table
scDat <- data.table()
for(file in scFiles) {
    print(file)
    
    # Read lines
    lines <- readLines(paste0(datadir, "SCI/", file))
    # Extract subject category and journal count
    Category <- properCase(gsub("(^[A-Z ]* - )|( - [A-Z ]*$)", "", lines[1]))
    ct <- as.numeric(gsub("^Total journals: ", "", lines[2]))
    if(is.na(ct)) stop("Failed journal count")
    
    # Identify journal title lines
    titles <- grep("^[0-9]*\\.", lines)
    if(length(titles) != ct) stop("Journal count != number of journals")
    # Clean titles
    Title <- properCase(gsub("^[0-9]*\\. ", "", lines[titles]))
    
    # Identify Frequencies & ISSNs
    issns <- grep("ISSN: ", lines)
    if(length(issns) != ct) stop("Journal count != number of ISSNs")
    # Clean frequencies & ISSNs
    Frequency <- gsub(" *ISSN: .*$", "", lines[issns])
    ISSN <- gsub("^.*ISSN: ", "", lines[issns])
    # Check meaningfulness
    if(any(grepl("ISSN", Frequency))) stop("ISSNs in the Frequency field")
    if(any(grepl("ISSN", ISSN))) stop("ISSN string in ISSN field")
    
    dat <- data.table(issn = ISSN, title = Title,
                      freq = Frequency, cat = Category)
    scDat <- rbindlist(list(scDat, dat))
}
# Check meaningfulness
print(unique(scDat$freq))
stopifnot(unique(nchar(scDat$issn)) == 9)
# Check that the same journal nowhere has different frequency or title data
n <- length(unique(scDat$issn))
if(n != nrow(unique(scDat[, .(issn, title, freq)]))) stop("Inconsistent info")


## Collapse categories into a list of vectors

# Aggregate data.table, combining categories by concatenation
scDat[, cats := paste(cat, collapse = "|"), by = list(issn)]
# Collapse the data by journal
scDat <- unique(scDat[, .(issn, freq, cats)])
if(n != nrow(scDat)) stop("Collapsed to wrong size")


## Merge into the TR metrics data

# Merge by issn; keep extras only from wosDat
wosDat <- merge(wosDat, scDat, by = "issn", all.x = TRUE)
# Print table of overlap
print(table(is.na(wosDat$cats)))
# Split from single string to vector
wosDat[, cats := strsplit(cats, "\\|")]

# Save!
save(wosDat, file = "calc/wosDat.RData")
rm(wosFiles, wosLst, wosDat)


## Scimago journal rankings

# Years to include
dlyrs <- 1999:2013
sjrDat <- data.table()
for(dlyr in dlyrs) {
    print(dlyr)
    file <- paste0(datadir, "sjr/sjr", dlyr, ".xlsx")
    # Download Excel file (if not already downloaded)
    if(!file.exists(file)) {
        download.file(url = paste0(
            "http://www.scimagojr.com/journalrank.php?category=0&area=0&year=",
            dlyr, "&country=&order=sjr&page=0&min=0&min_type=cd&out=xls"
        ), destfile = file)
    }
    # Read into data.table
    #dat <- as.data.table(read.xls(file, sheet = 1, header = TRUE,
    #                              colClasses = "character"))
    dat <- as.data.table(readxl::read_excel(file, sheet = 1))
    dat[, `SJR Best Quartile` := NULL]
    setnames(dat, 1:14,
             c("rk", "title", "type", "issn", "sjr", "h",
               "docs", "docs3", "refs", "cites3", "citable3",
               "cpa", "rpa", "co"))
    # Remove unwanted fields
    dat[, rk := NULL]
    dat[, docs := NULL]
    dat[, docs3 := NULL]
    dat[, refs := NULL]
    dat[, cites3 := NULL]
    dat[, citable3 := NULL]
    dat[, co := NULL]
    #dat[, h := NULL] # h-index does not change over time
    # Remove "ISSN " from ISSN field & add year field
    dat[, issn := paste0(substr(issn, 6, 9), "-", substr(issn, 10, 13))]
    dat[, yr := dlyr]
    # Coerce numbers to numeric
    dat[, sjr := as.numeric(sjr)]
    dat[, h := as.numeric(h)] # h-index does not change over time
    dat[, cpa := as.numeric(cpa)]
    dat[, rpa := as.numeric(rpa)]
    # Set key
    setkey(dat, issn, yr)
    sjrDat <- rbindlist(list(sjrDat, dat))
}
# Remove nonsense ISSNs
sjrDat <- sjrDat[nchar(issn) == 9]

# Save!
setkey(sjrDat, issn, yr)
sjrDat <- unique(sjrDat)
save(sjrDat, file = "calc/sjrDat.RData")

rm(list = ls())
