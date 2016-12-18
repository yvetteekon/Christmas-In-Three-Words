# load libraries
packages <- c("twitteR", "RCurl", "tm", "SnowballC", "magrittr")
lapply(packages, require, character.only = T)

# set up api credentials
consumer_key <- "<hidden>"
consumer_secret <- "<hidden>"
access_token <- "<hidden>"
access_secret <- "<hidden>"

# create handshake
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# extract tweets
tweets <- searchTwitter("#christmasinthreewords", n = 500, lang = "en", resultType = "recent")
head(tweets, 3)

# more info about tweets
tweetsDF <- twListToDF(tweets)
head(tweetsDF, 3)
save(tweetsDF, file = "christmas.RData")
write.csv(tweetsDF, file = "christmas.csv", quote = FALSE, row.names = FALSE)

# convert tweet list to character vectors
tweets_text <- sapply(tweets, function(x) x$getText())
head(tweets_text, 3)
save(tweets_text, file = "tweets.txt")

# convert character vector to corpus
docs <- Corpus(VectorSource(tweets_text))
inspect(docs[1])
inspect(docs)[1]
viewDocs <- function(d, n){
  d %>% extract2(n) %>% as.character() %>% writeLines()
}
viewDocs(docs, 16)

# text cleaning
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/|@|#|\\|") # remove mentions, hastags

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeNumbers)

toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "christ", "christ")

myStopwords <- c(stopwords("en"), "midnight", "tco", "christmasinthreewords",
                 "christmasinwords", "christmas", "americasmorning")
docs <- tm_map(docs, removeWords, myStopwords)

removeURLS <- content_transformer(function(x) gsub("http[[:alnum:]]*", "", x))
docs <- tm_map(docs, removeURLS)

removeControls <- content_transformer(function(x) gsub('[[:cntrl:]]', '', x))
docs <- tm_map(docs, removeControls)

removeRT <- content_transformer(function(x) gsub('\\b+RT', '', x))
docs <- tm_map(docs, removeRT)

removeLeadingSpaces <- content_transformer(function(x) gsub("^[[:space:]]*", "", x))
docs <- tm_map(docs, removeLeadingSpaces)

removeTrailingSpaces <- content_transformer(function(x) gsub("[[:space:]]*$", "", x))
docs <- tm_map(docs, removeTrailingSpaces)

docs <- tm_map(docs, stripWhitespace)

# stemming
docsCopy <- docs
docs <- tm_map(docs, stemDocument)
viewDocs(docs, 16)
viewDocs(docsCopy, 16)

# clean corpus
docs <- tm_map(docs, PlainTextDocument)

# create document term matrix
dtm <- DocumentTermMatrix(docs) #, control = list(wordLengths = c(1, Inf)))
dtm

# reduce sparsity in matrix
dtm <- removeSparseTerms(dtm, 0.99)
save(dtm, file = "dtm.RData")

# Create matrix and dataframe
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
words <- names(freq)
words

# stem completion
words <- stemCompletion(words, docsCopy, type = "prevalent")
words

df <- data.frame(words = words, freq = freq)
row.names(df) <- NULL
which(df$words == "")
df$words <- as.character(df$words)
df$words[5] <- "family"
df$words[10] <- "merry"
df$words[30] <- "ugly"
df$words <- factor(df$words)
df
save(df, file = "df.RData")
