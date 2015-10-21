library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

api_key ="NnsMrGv2CEF8g71LqVOiHdXeg"
api_secret ="OCt39dwLhpt9WFKRq3mD9k4o2zSRagLr3GZVBjBAFzAX09pe5I"
access_token ="142569552-AHcBcvwBckHbQOiWiwq99iXErWHJlQ7QRphyTHqz"
access_token_secret ="mftLRi6SDv0fJ09cePgw4ae5rAJNgY3KB7rW8K68mWXa0"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


viki.tweets = searchTwitteR("viki", n=1500, lang="en")

tweet = viki.tweets[[1]]
class(tweet)
tweet$getScreenName()
tweet$getText()

tweet_text = sapply(viki.tweets, function(x) x$getText())

length(tweet_text)
head(tweet_text, 100)

#most used words
tweets = userTimeline("Viki", n = 3200)
n.tweet = length(tweets)


## convert tweets to a data frame

tweets.df <- twListToDF(tweets)

dim(tweets.df)

1:2
c(1:2, 320)

for (i in c(1:2, 320)) {
  
  cat(paste0("[", i, "] "))
  
  writeLines(strwrap(tweets.df$text[i], 60))
  
}


library(tm)



myCorpus <- Corpus(VectorSource(tweets.df$text))



myCorpus <- tm_map(myCorpus, content_transformer(tolower))



## remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)



myCorpus <- tm_map(myCorpus, content_transformer(removeURL))


# remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove punctuation

myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers

myCorpus <- tm_map(myCorpus, removeNumbers)

# add two extra stop words: "available" and "via"

myStopwords <- c(stopwords('english'), "available", "via")

# remove "r" and "big" from stopwords

myStopwords <- setdiff(myStopwords, c("r", "big"))

# remove stopwords from corpus

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# remove extra whitespace

myCorpus <- tm_map(myCorpus, stripWhitespace)

# keep a copy of corpus to use later as a dictionary for stem completion

myCorpusCopy <- myCorpus

# stem words
library(SnowballC)
install.packages("SnowballC")
myCorpus <- tm_map(myCorpus, stemDocument)

# inspect the first 5 documents (tweets)

inspect(myCorpus[1:5])

# The code below is used for to make text fit for paper width

for (i in c(1:2, 320)) {
  
  cat(paste0("[", i, "] "))
  
  writeLines(strwrap(as.character(myCorpus[[i]]), 60))
  
}


# tm v0.6

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  
  
  
  x <- x[x != ""]
  
  x <- stemCompletion(x, dictionary=dictionary)
  
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
  
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)

myCorpus <- Corpus(VectorSource(myCorpus))


# count frequency of "viki"

miningCases <- lapply(myCorpusCopy,
                      
                      function(x) { grep(as.character(x), pattern = "\\<viki")} )

sum(unlist(miningCases))



# count frequency of "Korea"

minerCases <- lapply(myCorpusCopy,
                     
                     function(x) {grep(as.character(x), pattern = "\\<korea")} )

sum(unlist(minerCases))



# replace "miner" with "mining"

myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   
                   pattern = "korea", replacement = "viki")

tdm <- TermDocumentMatrix(myCorpus,
                          
                          control = list(wordLengths = c(1, Inf)))

idx <- which(dimnames(tdm)$Terms == "viki")

inspect(tdm[idx + (0:5), 101:110])


(freq.terms <- findFreqTerms(tdm, lowfreq = 15))



term.freq <- rowSums(as.matrix(tdm))

term.freq <- subset(term.freq, term.freq >= 15)

df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)

ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with 'r'?

findAssocs(tdm, "viki", 0.2)



# which words are associated with 'mining'?

findAssocs(tdm, "viki", 0.25)

require(devtools)


source("https://bioconductor.org/biocLite.R")
biocLite("RBGL")

#install_url("http://cran.r-project.org/src/contrib/Archive/graph/graph_1.30.0.tar.gz")


library(graph)

source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

library(Rgraphviz)

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

m <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency

word.freq <- sort(rowSums(m), decreasing = T)

# colors

pal <- brewer.pal(9, "BuGn")

pal <- pal[-(1:4)]

# plot word cloud

library(wordcloud)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          
          random.order = F, colors = pal)
