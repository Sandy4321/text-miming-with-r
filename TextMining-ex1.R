library(tm)              # Framework for text mining.
library(SnowballC)       # Provides wordStem() for stsis of transcripts.
library(qdapDictionaries)
library(dplyr)           # Data preparation and pipes %>%.
library(RColorBrewer)    # Generate palette of colours for plots.
library(ggplot2)         # Plot word frequencies.
library(scales)          # Include commas in numbers.
library(Rgraphviz)       # Correlation plots.

# install from bioconductor
# source('http://bioconductor.org/biocLite.R')
# biocLite('Rgraphviz')


# These are dependencies that would otherwise be loaded as required.

library(magrittr)         #
library(stringr)

## get help on functions or packages
## ?read.csv
## library(help=rattle)


## convert pdf to txt
## system("for f in *.pdf; do pdftotext -enc ASCII7 -nopgbrk $f; done")

## convert doc fo txt
## system("for f in *.doc; do antiword $f; done")


## list sources
getSources()


## list readers
getReaders()


## location of txt docs
cname <- file.path(".", "corpus", "txt")
cname

## folder of txt docs
length(dir(cname))
dir(cname)


## load corpus
library(tm)
docs <- Corpus(DirSource(cname))
docs
class(docs)
class(docs[[1]])
summary(docs)

inspect(docs[16])
writeLines(as.character(docs[[16]]))

## ----read_pdf, eval=FALSE------------------------------------------------
## cname2 <- file.path(".", "corpus", "pdf")
## docs2 <- Corpus(DirSource(cname2), readerControl=list(reader=readPDF))


## ----read_doc, eval=FALSE------------------------------------------------
## docs <- Corpus(DirSource(cname), readerControl=list(reader=readDOC))


## ----read_doc_options, eval=FALSE----------------------------------------
## docs <- Corpus(DirSource(cname), readerControl=list(reader=readDOC("-r -s")))


# check available transformations
getTransformations()


## transform slash
toSpace <- content_transformer( function(x, pattern) gsub(pattern, " ", x) )
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

inspect(docs[16])
writeLines(as.character(docs[[16]]))

## do the same in a single line
## docs <- tm_map(docs, toSpace, "/|@|\\|")


## to lowercase
docs <- tm_map(docs, content_transformer(tolower))

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## remove numbers
docs <- tm_map(docs, removeNumbers)

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## remove punctuation
docs <- tm_map(docs, removePunctuation)

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## remove stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

inspect(docs[16])
writeLines(as.character(docs[[16]]))

length(stopwords("english"))
stopwords("english")


## remove own stopwords
docs <- tm_map(docs, removeWords, c("department", "email", "lewis"))

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## strip whitespaces
docs <- tm_map(docs, stripWhitespace)

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## specific transforms
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "harbin institute technology", "HIT")
docs <- tm_map(docs, toString, "shenzhen institutes advanced technology", "SIAT")
docs <- tm_map(docs, toString, "chinese academy sciences", "CAS")

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## ------------------------------------------------------------------------
library(SnowballC)
docs <- tm_map(docs, stemDocument)

inspect(docs[16])
writeLines(as.character(docs[[16]]))


## create document term matrix
dtm <- DocumentTermMatrix(docs)
dtm

inspect(dtm[1:5, 1000:1005])
class(dtm)
dim(dtm)


## create term document matrix
tdm <- TermDocumentMatrix(docs)
tdm
class(tdm)
dim(tdm)
inspect(tdm[1000:1005, 1:5])



## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtm))
length(freq)


ord <- order(freq)

# Least frequent terms
freq[head(ord)]


# Most frequent terms
freq[tail(ord)]

# Frequency of frequencies.
head(table(freq), 15)
tail(table(freq), 15)


## dtm -> matrix
m <- as.matrix(dtm)
dim(m)


## save DTM to csv file
## write.csv(m, file="dtm.csv")


## remove sparse terms
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)


## ------------------------------------------------------------------------
inspect(dtms)


## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtms))
freq
table(freq)


## freq terms 1000
findFreqTerms(dtm, lowfreq=1000)


## freq terms 100
findFreqTerms(dtm, lowfreq=100)


## assoc rules
findAssocs(dtm, "data", corlimit=0.5)


## plot correlations
plot(dtm, 
     terms=findFreqTerms(dtm, lowfreq=100)[1:50], 
     corThreshold=0.5)


## plot correlations with options
plot(dtm, 
     terms=findFreqTerms(dtm, lowfreq=100)[1:50], 
     corThreshold=0.5,
     weighting=TRUE)


## word count
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
head(wf)


## plot freq
library(ggplot2)
subset(wf, freq>500)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## wordcloud
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)


## wordcloud max words
set.seed(142)
wordcloud(names(freq), freq, max.words=100)


## wordcloud higher freq
set.seed(142)
wordcloud(names(freq), freq, min.freq=100)


## wordcloud colour
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))


## wordcloud scale
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))


## wordcloud rotate
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=100, rot.per=0.2, colors=dark2)


## library qdap
library(qdap)

## qdap create word list
words <- dtm                                                          %>%
  as.matrix                                                           %>%
  colnames                                                            %>%
  (function(x) x[nchar(x) < 20])


## qdap word length
length(words)
head(words, 15)
summary(nchar(words))
table(nchar(words))
dist_tab(nchar(words))


## qdap word length plot
data.frame(nletters=nchar(words))                                     %>%
  ggplot(aes(x=nletters))                                              + 
  geom_histogram(binwidth=1)                                           +
  geom_vline(xintercept=mean(nchar(words)), 
             colour="green", size=1, alpha=.5)                         + 
  labs(x="Number of Letters", y="Number of Words")


## qdap letter freq plot
library(dplyr)
library(stringr)

words                                                        %>%
  str_split("")                                                       %>%
  sapply(function(x) x[-1])                                           %>%
  unlist                                                              %>%
  dist_tab                                                            %>%
  mutate(Letter = factor(toupper(interval),
                       levels=toupper(interval[order(freq)])))        %>%
  ggplot(aes(Letter, weight=percent))                                  + 
  geom_bar()                                                           +
  coord_flip()                                                         +
  ylab("Proportion")                                                   +
  scale_y_continuous(breaks=seq(0, 12, 2), 
                     label=function(x) paste0(x, "%"), 
                     expand=c(0,0), limits=c(0,12))

## qdap count position plot
words                                                                 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE))        %>%
  unlist                                                              %>%
  (function(x) x[x!=-1])                                              %>%
  (function(x) setNames(x, gsub("\\d", "", names(x))))                %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)), 
                                      position=unname(x))),
                     1, function(y) y/length(x)))                     %>%
  qheat(high="green", low="yellow", by.column=NULL, 
        values=TRUE, digits=3, plot=FALSE)                             +
  ylab("Letter")                                                       + 
  xlab("Position")                                                     + 
  theme(axis.text.x=element_text(angle=0))                             +
  guides(fill=guide_legend(title="Proportion"))


## install data to reocognise gender based on first name
## devtools::install_github("lmullen/gender-data-pkg")

## recognise gender based on name
name2sex(qcv(graham, frank, leslie, james, jacqui, jack, kerry, kerrie))





## #######################################################################33
## REVIEW
library(tm)
library(wordcloud)

## # Locate and load the Corpus.
cname <- file.path(".", "corpus", "txt")
docs <- Corpus(DirSource(cname))
docs
summary(docs)
inspect(docs[16])
writeLines(as.character(docs[[16]]))

## # Transforms
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/|@|\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("own", "stop", "words"))
docs <- tm_map(docs, stripWhitespace)
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "specific transform", "ST")
docs <- tm_map(docs, toString, "other specific transform", "OST")
docs <- tm_map(docs, stemDocument)

## # Document term matrix.
dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:5, 1000:1005])

## # Explore the corpus.
findFreqTerms(dtm, lowfreq=100)
findAssocs(dtm, "data", corlimit=0.6)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf   <- data.frame(word=names(freq), freq=freq)
library(ggplot2)
p <- ggplot(subset(wf, freq>500), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))

## # Generate a word cloud
library(wordcloud)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))