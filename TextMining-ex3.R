
# Gutenberg project
# http://www.gutenberg.org/

TEXTFILE <- "data/pg100.txt"

if (!file.exists(TEXTFILE)) {
    dir.create(dirname(TEXTFILE), FALSE)
    download.file("http://www.gutenberg.org/cache/epub/100/pg100.txt", destfile = TEXTFILE)
}
shakespeare <- readLines(TEXTFILE)
length(shakespeare)

# check head and tail
head(shakespeare)
tail(shakespeare)

# remove header
shakespeare <- shakespeare[-(1:173)]

# remove footer
shakespeare <- shakespeare[-(124195:length(shakespeare))]