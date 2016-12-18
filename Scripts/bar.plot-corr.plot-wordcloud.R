# load libraries
packages <- c("tm", "ggplot2", "graph", "Rgraphviz", "wordcloud")
lapply(packages, require, character.only = T)

# load data
load("df.RData")
load("dtm.RData")

# create term document matrix
tdm <- as.TermDocumentMatrix(dtm)
save(tdm, file = "tdm.RData")

# term frequency
findFreqTerms(dtm, lowfreq = 5)

inspect(tdm[1:5, 20:25])
idx <- which(dimnames(tdm)$Terms == "broke")
idx
inspect(tdm[idx + (0:5), 20:25])
inspect(tdm[idx + (0:5), 16])

# graph of term frequency
pdf("Term Frequency.pdf")
ggplot(df, aes(x = words, y = freq)) + geom_bar(stat = "identity") +
  labs(title = "Frequency of All Terms", x = "Words", y = "Count") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) + coord_flip()

subset(df, freq > 6) %>% ggplot(aes(x = words, y = freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "red", alpha = 0.6) +
  labs(title = "Terms Which Occur at Least 7 Times", x = "Words", y = "Count") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()

# term association
findAssocs(dtm, c("trump", "jesus", "santa"), c(0.3, 0.1))

# correlation plot
pdf("Term Association.pdf")
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 5), corThreshold = 0.1, weighting = T)
dev.off()

# both term frequency and association
# word cloud
pdf("WordCloud.pdf")
set.seed(123)
color1 <- rainbow(6)
color2 <- brewer.pal(6, "Dark2")
color3 <- brewer.pal(6, "Set1")
wordcloud(df$words, df$freq, scale = c(4, 0.5), min.freq = 3, random.order = FALSE,
          rot.per = 0.1, colors = color3)
dev.off()

