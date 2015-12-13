#https://cran.r-project.org/web/packages/quanteda/vignettes/LitVignette.html
# In this vignette we show how the quanteda package can be used to replicate the analysis from Matthew Jockers' book Text Analysis with R for Students of Literature (London: Springer, 2014)

#install.packages("quanteda", dependencies = TRUE)
library(quanteda)


# 2 First Foray
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


# 3 Accessing and Comparing Word Frequency Data
# 3.1 Accessing Word Data

# frequencies of 'he' and 'she' - these are matrixes, not numerics
mobyDfm[, c("he", "she", "him", "her")]

mobyDfm[, "her"]

mobyDfm[, "him"]/mobyDfm[, "her"]

mobyDfm[, "he"]/mobyDfm[, "she"]


# 3.2 Recycling
mobyDfmPct <- weight(mobyDfm, "relFreq") * 100
mobyDfmPct[, "the"]


plot(topfeatures(mobyDfmPct), type="b",
     xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(topfeatures(mobyDfmPct)))

# 4 Token Distribution Analysis
# 4.1 Dispersion plots

# using words from tokenized corpus for dispersion
plot(kwic(novel.v, "whale"))

plot(kwic(novel.v, "Ahab"))

# 4.2 Searching with grep

# identify the chapter break locations
(chap.positions.v <- kwic(novel.v, "CHAPTER \\d", valuetype = "regex")$position)

head(kwic(novel.v, 'chapter'))


chaptersVec <-unlist(segment(novel.v, what='other', delimiter="CHAPTER\\s\\d", perl=TRUE))
chaptersLowerVec <- toLower(chaptersVec)
chaptersCorp <- corpus(chaptersVec)

# Fig 4.4 barplots of whale and ahab

chapDfm <- dfm(chaptersCorp)


# 'whale'
barplot(as.numeric(chapDfm[, 'whale']))

# 'ahab'
barplot(as.numeric(chapDfm[, 'ahab']))

# Relative frequency barplots of whale and ahab

relDfm <- weight(chapDfm, type='relFreq') * 100
head(relDfm)

# 'whale'
barplot(as.numeric(relDfm[, 'whale']))

# 'ahab'
barplot(as.numeric(relDfm[, 'ahab']))

# 5 Correlation
# 5.2 Correlation Analysis

wf <- as.numeric(relDfm[,'whale'])
af <- as.numeric(relDfm[,'ahab'])
cor(wf, af)

waDfm <- cbind(relDfm[,'whale'], relDfm[,'ahab'])
cor(as.matrix(waDfm))


# 5.4 Random Sampling

samples <- replicate(1000, cor(sample(af), sample(wf)))

h <- hist(samples, breaks=100, col="grey",
          xlab="Correlation Coefficient",
          main="Histogram of Random Correlation Coefficients\n
with Normal Curve",
          plot=T)
xfit <- seq(min(samples),max(samples),length=1000)
yfit <- dnorm(xfit,mean=mean(samples),sd=sd(samples))
yfit <- yfit*diff(h$mids[1:2])*length(samples)
lines(xfit, yfit, col="black", lwd=2)


cor.test(wf, af)


# 6 Measures of Lexical Variety
# 6.2 Mean word frequency

firstChap <- as.matrix(chapDfm[1,])
numWords <- length(firstChap[firstChap > 0])
sum(chapDfm[1,])/numWords


sum(chapDfm[1,])/ntype(chaptersCorp[1], removePunct=TRUE)

# 6.3 Extracting Word Usage Means
chapMeans <- Matrix::rowMeans(chapDfm)
plot(chapMeans, type="h")


