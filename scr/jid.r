### Read the file "J_Entrez" as a data.table

# Read lines, check that dashes occur at regular intervals
lines <- readLines("data/J_Entrez")
wh <- which(lines == lines[1])
n <- unique(diff(wh))
stopifnot(length(n) == 1)

# Extract header from first "line"
hdr <- gsub("(^[^:]*): .*$", "\\1", lines[2:n])

# Bind remaining "lines"
tab <- t(sapply(wh[-length(wh)], function(i) {
    gsub("^[^:]*: (.*$)", "\\1", lines[i + 1:(n - 1)])
}))

# Coerce to data.table
jids <- as.data.table(tab)
setnames(jids, names(jids), hdr)

# Save as part of the pubmed.network package
save(jids, file = "calc/jids.RData")
