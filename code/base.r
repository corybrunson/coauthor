## Setup

# Data directory (Where are the PubMed, etc. data stored?)
datadir <- "../../data/"

# Load packages from CRAN
pkgs <- c("devtools", "zoo", "ggplot2", "data.table")
for(pkg in pkgs) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg, repos = 'http://cran.us.r-project.org')
        stopifnot(require(pkg, character.only = TRUE))
    }
}

# Identify range of years and 3-year intervals
# (before electronic publishing takeover)
years <- 1982:2008  # omit sketchy earlier years
years <- setdiff(years, 2008)  # omit incomplete year
years <- setdiff(years, 1800:1990)  # 5 yrs before 1st yr w/ 25-author limit


## Functions

# Read PubMed data as a table
read.pubmed <- function(file, nrows = -1, skip = 0)
{
    lines <- readLines(file)
    # Regular expressions used in the conversion process
    res <- c(
        fau = paste(
            c('(^[0-9]+,".*)"(.*"', ',".*"', rep(',"[^"]*"', 12), '$)'),
            collapse = ''
        ),
        au = paste(
            c('(^[0-9]+,"[^"]*",".*)"(.*"', rep(',"[^"]*"', 12), '$)'),
            collapse = ''
        ),
        ad = paste(
            c('(^[0-9]+', rep(',"[^"]*"', 11), ',".*)"(.*"',
              rep(',"[^"]*"', 2), '$)'),
            collapse = ''
        )
    )
    # Replace double quotes with single quotes within each field
    for(re in res) {
        wh <- grep(re, lines, useBytes = TRUE)
        while(length(wh) > 0) {
            lines[wh] <- gsub(re, '\\1\'\\2', lines[wh], useBytes = TRUE)
            wh <- grep(re, lines, useBytes = TRUE)
        }
    }
    # Write lines to a temp file
    temp <- paste0(datadir, "medline/temp.txt")
    if(file.exists(temp)) file.remove(temp)
    con <- file(temp)
    writeLines(lines, con)
    close(con)
    # Read the temp file into a data frame
    dat <- read.table(file = paste0(datadir, "medline/temp.txt"),
                      header = TRUE,
                      sep = ',', quote = '\"',
                      fill = TRUE, comment.char = '',
                      nrows = nrows, skip = skip,
                      row.names = NULL,
                      colClasses = 'character')
    names(dat) <- names(read.csv(file, nrows = 1))
    as.data.table(dat)
}


## Transforms

# Anscombe transformation
anscombeFun <- function(x) 2 * sqrt(x + 3/8)
# Log+1 function
logPlus1 <- function(x) log(x + 1)
