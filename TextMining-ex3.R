# http://www.exegetic.biz/blog/2013/09/text-mining-the-complete-works-of-william-shakespeare/

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


# concatenate all character vectors into 1 vector
shakespeare <- paste(shakespeare, collapse = " ")
nchar(shakespeare)

# remove
# <<THIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM
# ....
#SERVICE THAT CHARGES FOR DOWNLOAD TIME OR FOR MEMBERSHIP.>>

shakespeare <- strsplit(shakespeare, "<<[^>]*>>")[[1]]
length(shakespeare)

(dramatis.personae <- grep("Dramatis Personae", shakespeare, ignore.case = TRUE))
length(shakespeare)

shakespeare <- shakespeare[-dramatis.personae]
length(shakespeare)



library(tm)

# create corpus
doc.vec <- VectorSource(shakespeare)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)

# transformations
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))

library(SnowballC)
doc.corpus <- tm_map(doc.corpus, stemDocument)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)


inspect(doc.corpus[8])
writeLines(as.character(doc.corpus[[8]]))

# create Term Document Matrix
TDM <- TermDocumentMatrix(doc.corpus)
TDM


inspect(TDM[1:10,1:10])

# DTM is a transpose of TDM
DTM <- DocumentTermMatrix(doc.corpus)
inspect(DTM[1:10,1:10])

# find frequent terms
findFreqTerms(TDM, 2000)

# find assosiations rules to 'love'
findAssocs(TDM, "love", 0.8)

# remove sparse terms
TDM.common = removeSparseTerms(TDM, 0.1)
dim(TDM)
dim(TDM.common)

inspect(TDM.common[1:10,1:10])


library(slam)
TDM.dense <- as.matrix(TDM.common)
TDM.dense
object.size(TDM.common)
object.size(TDM.dense)


library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, color = palette)

library(reshape2)
TDM.dense = melt(TDM.dense, value.name = "count")
head(TDM.dense)


library(ggplot2)
ggplot(TDM.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
    geom_tile(colour = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
    ylab("") +
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



