library(tm)
library(purrr)
library(qdap)
library(dplyr)
library(magrittr)
library(cluster)
library(ggplot2)
library(tibble)
library(dendextend)

# Loading a file into the workspace
doc_file <- read.csv("GameTheory.csv", stringsAsFactors = FALSE)

# Function to create any csv file to a VCorpus
game_corpus_func <- function(csv_file) {
       corpus_source <- VectorSource(csv_file)
       corpus_file <- VCorpus(corpus_source)
    return(corpus_file)
}

# Generating the VCorpus file for text mining
game_corpus <- game_corpus_func(doc_file)

# Creating a function to clean the text data
clean_function <- function(corpus) {
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
}

# Cleaning the corpus file
clean_game_corpus <- clean_function(game_corpus)

# Printing out the text file to console
# writeLines(as.character(clean_game_corpus[1]))

# Finding the most common words and ploting them
common_words <- freq_terms(clean_game_corpus, top = 25, at.least = 2, stopwords = "Top200Words")
word_plot <- ggplot(common_words, aes(x = WORD, y = FREQ)) +
    geom_bar(stat = "identity", width = 0.75, fill = "steelblue")
 print(word_plot)

# Converting given Corpus to a Matrix
game_dtm <-DocumentTermMatrix(clean_game_corpus)
game_dtm_clean <- removeSparseTerms(game_dtm, sparse = 0.975)
game_matrix <- as.matrix(game_tdm_clean)

# Converting Matrix of most frequent words into a Dataframe
game_frequency <- colSums(game_matrix)
game_frequency <- subset(game_frequency, game_frequency > 1)
word_frequency <- sort(game_frequency, decreasing = FALSE)
term_list <- names(word_frequency)
game_df <- data.frame(terms = term_list, frequency = word_frequency)


# Converting to a distance matrix for clustering
game_dist <- dist(game_df[, 2])
game_dist2 <- dist(game_df)
# ----------------------------------------------------------------------------------------------------------------------------------------
# Hierarchial Clustering

# Function takes a distance matrix, computer clusters and return a dendrogram object

m_hclust <- hclust(game_dist2, method = "complete")
hcluster_cutree <- cutree(m_hclust, h = 5)
game_df <- mutate(game_df, h_cluster = hcluster_cutree)

# Plotting the Dendrogram
m_dendrogram <- as.dendrogram(m_hclust)
clean_dendrogram <- color_branches(m_dendrogram, h = 5)
# plot(clean_dendrogram)
# --------------------------------------------------------------------------------------------------------------------------------------
# K - Means Clustering

# Running various K-means model to find best k - value
tot_withinss <- map_dbl(1:10, function(k) {
    mock_model <- kmeans(x = game_dist, centers = k)
    mock_model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plotting the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
    geom_line() +
    scale_x_continuous(breaks = 1:10)

# Clustering the data based on the best k gotten from the Scree plot
game_kcluster1 <- kmeans(game_dist, 2)
game_cluster_vec <- game_kcluster1$cluster
game_df <- mutate(game_df, k_cluster = game_cluster_vec)

# Plotting the clusters.
game_plot <- ggplot(game_df, aes(x = frequency, y = terms, color = factor(k_cluster))) +
    geom_point() + labs(title = "Plotting of Clusters", color = "Cluster")
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------




