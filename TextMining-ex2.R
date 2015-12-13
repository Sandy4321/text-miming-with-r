#https://cran.r-project.org/web/packages/quanteda/vignettes/LitVignette.html
# In this vignette we show how the quanteda package can be used to replicate the analysis from Matthew Jockers' book Text Analysis with R for Students of Literature (London: Springer, 2014)

#install.packages("quanteda", dependencies = TRUE)
require(quanteda)


# 2.1 Loading the first text file

# read the text as a single file
mobydicktf <- textfile("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
mobydicktf
class(mobydicktf)


substring(texts(mobydicktf), 10, 100)


# 2.2 Separate content from metadata

# extract the header information
mobydickText <- texts(mobydicktf)
endMetadataIndex <- regexec("CHAPTER 1. Loomings.", mobydickText)[[1]]
metadata.v <- substring(texts(mobydicktf), 1, endMetadataIndex - 1)

# verify that "orphan" is the end of the novel
kwic(mobydickText, "orphan")


# extract the novel -- a better way
novel.v <- substring(mobydickText, endMetadataIndex, 
                     regexec("End of Project Gutenberg's Moby Dick.", mobydickText)[[1]]-1)


# 2.3 Reprocessing the content

# lowercase
novel.lower.v <- toLower(novel.v)

# tokenize
moby.word.v <- tokenize(novel.lower.v, removePunct = TRUE, simplify = TRUE)
length(moby.word.v)

total.length <- length(moby.word.v)
str(moby.word.v)


moby.word.v[1:10]
moby.word.v[99986] 
moby.word.v[c(4,5,6)]


# 2.4 Beginning the analysis


moby.word.v <- tokenize(novel.lower.v, simplify = TRUE)

# count of the word 'whale'
length(moby.word.v[which(moby.word.v == "whale")])

# total occurrences of 'whale' including possessive
length(moby.word.v[which(moby.word.v == "whale")]) + length(moby.word.v[which(moby.word.v == "whale's")])

# same thing using kwic()
nrow(kwic(novel.lower.v, "whale"))

# includes words like 'whalemen'
nrow(kwic(novel.lower.v, "whale*")) 

(total.whale.hits <- nrow(kwic(novel.lower.v, "^whale('s){0,1}$", valuetype = 'regex')))

# What fraction of the total words in the novel are 'whale'?
total.whale.hits / ntoken(novel.lower.v, removePunct=TRUE)  

# total unique words
length(unique(moby.word.v))

ntype(toLower(novel.v), removePunct = TRUE)

# ten most frequent words
mobyDfm <- dfm(novel.lower.v)

mobyDfm[, "whale"]

topfeatures(mobyDfm)


plot(topfeatures(mobyDfm, 100), log = "y", cex = .6, ylab = "Term frequency")



