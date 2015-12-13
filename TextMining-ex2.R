#https://cran.r-project.org/web/packages/quanteda/vignettes/LitVignette.html
# In this vignette we show how the quanteda package can be used to replicate the analysis from Matthew Jockers' book Text Analysis with R for Students of Literature (London: Springer, 2014)

install.packages("quanteda", dependencies = TRUE)
require(quanteda)


# read the text as a single file
mobydicktf <- textfile("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
mobydicktf
class(mobydicktf)


substring(texts(mobydicktf), 10, 100)


# extract the header information
mobydickText <- texts(mobydicktf)
endMetadataIndex <- regexec("CHAPTER 1. Loomings.", mobydickText)[[1]]
metadata.v <- substring(texts(mobydicktf), 1, endMetadataIndex - 1)


# verify that "orphan" is the end of the novel
kwic(mobydickText, "orphan")


# extract the novel -- a better way
novel.v <- substring(mobydickText, endMetadataIndex, 
                     regexec("End of Project Gutenberg's Moby Dick.", mobydickText)[[1]]-1)

# lowercase
novel.lower.v <- toLower(novel.v)

# tokenize
moby.word.v <- tokenize(novel.lower.v, removePunct = TRUE, simplify = TRUE)
length(moby.word.v)
