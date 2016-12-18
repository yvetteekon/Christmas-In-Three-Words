# load libraries
packages <- c("cluster", "fpc", "qdap", "qdapDictionaries", "ggplot2", "stringr",
              "dplyr")
lapply(packages, require, character.only = T)

# load data
load("tdm.RData")
load("dtm.RData")

# Clustering
# Advanced Clustering
# Hierarchical clustering
matrix <- as.matrix(tdm)
distMatrix <- dist(scale(matrix), method = "euclidean")
hfit <- hclust(distMatrix, method = "ward.D")
plot(hfit)
plot(hfit, hang = -1)
(group <- cutree(hfit, k=5))
rect.hclust(hfit, k=6, border = 2)

# Partition Based
# K-Means Clustering
matrixTranspose <- t(matrix) # transpose of matrix of cluster documents (tweets)
set.seed(123)
k = 6
kmeansResult <- kmeans(matrixTranspose, k)
str(kmeansResult)

round(kmeansResult$centers, digits = 3) # cluster centers

plot(matrixTranspose, col = kmeansResult$cluster)
points(kmeansResult$centers, col = 1:6, pch = 8, cex = 2)

clusplot(matrixTranspose, kmeansResult$cluster, color = T, shade = T, labels = 6, lines =0)

# Partitioning Around Medoids (PAM)
pamResult <- pamk(matrixTranspose, metric = "manhattan")
k <- pamResult$nc # number of clusters identified
pamResult <- pamResult$pamobject

# print cluster medoids
for (i in 1:k){
  cat("cluster", i, ": ",
      colnames(pamResult$medoids)[which(pamResult$medoids[i, ] == 1)], "\n")
}

# plot clustering results
layout(matrix(c(1,1), 1, 1)) # set to two graphs per page
plot(pamResult, col.p = pamResult$clustering)


# Quantitative Analysis of Text
words <- dtm %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

length(words)
head(words, 15)
summary(nchar(words))
table(nchar(words))
dist_tab(nchar(words))

# Word Length Counts
data.frame(nletters=nchar(words)) %>%
  ggplot(aes(x=nletters)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=mean(nchar(words)),
             colour="green", size=1, alpha=.5) +
  labs(x="Number of Letters", y="Number of Words")


# Letter Frequency
words %>%
  str_split("") %>%
  sapply(function(x) x[-1]) %>%
  unlist %>%
  dist_tab %>%
  mutate(Letter=factor(toupper(interval),
                       levels=toupper(interval[order(freq)]))) %>%
  ggplot(aes(Letter, weight=percent)) +
  geom_bar() +
  coord_flip() +
  labs(y="Proportion") +
  scale_y_continuous(breaks=seq(0, 12, 2), label=function(x) paste0(x, "%"),
                     expand=c(0,0), limits=c(0,12))

# Letter and Position Heatmap
words %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))


