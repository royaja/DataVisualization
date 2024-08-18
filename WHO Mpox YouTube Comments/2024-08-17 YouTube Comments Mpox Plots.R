
# This code was created in order to look into the type of comments WHO recieve 
# on their announcement about highest level of alert for mpox. 
# Link to video: https://www.youtube.com/watch?v=gf-b8AHL1FI

# All data were collected using a separate script that connect to the 
# Google Cloud API for YouTube. 
# Data were gathered 2024-08-17.

# Load libraries
library(tidyverse)
library(janitor)
library(quanteda)
library(sentimentr)
library(patchwork)
library(showtext)

# Define color
plot_color <- "#4CAF50"

# Font "Roboto"
font_add_google("Roboto", "roboto")
showtext_auto()

# Wrangle data
whocomments <- 
  youtubeData %>%
  clean_names() %>%
  select(comment)

# Create a corpus
corpus <- corpus(whocomments$comment)

tokens <- 
  corpus %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_select(min_nchar = 4)

# Document-Feature Matrix (DFM)
dfm <- dfm(tokens)

# Summarize common words
dfm_df <- 
  dfm %>%
  textstat_frequency() %>%
  as_tibble() %>%
  arrange(desc(frequency))

# Plot common words
plot1 <- dfm_df %>%
  slice_max(frequency, n = 10) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = plot_color) +
  coord_flip() +
  labs(title = "Common Words in WHO YouTube Comments",
       x = "Words",
       y = "Frequency") +
  theme_bw() +
  theme(text = element_text(family = "roboto"))

# Plot top 10 words in polar coordinates
plot2 <- dfm_df %>%
  slice_max(frequency, n = 10) %>%
  ggplot(aes(x = reorder(feature, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = plot_color, color = "black") +
  coord_polar(theta = "x", start = 0) +
  labs(title = "",
       x = "",
       y = "") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", hjust = 1),  
    axis.text.y = element_blank(),  
    axis.ticks = element_blank(),  
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(family = "roboto")
  ) 

# Calculate sentiment
sentiment_scores <- sentiment(whocomments$comment)

sentiment_summary <- 
  sentiment_scores %>%
  group_by(element_id) %>%
  summarize(mean_sentiment = mean(sentiment))

# Plot sentiment distribution
plot3 <- ggplot(sentiment_summary, aes(x = mean_sentiment)) +
  geom_histogram(bins = 30, fill = plot_color, color = "black") +
  labs(title = "Distribution of Sentiment in WHO YouTube Comments",
       x = "Sentiment Score",
       y = "Count") +
  theme_bw() +
  theme(text = element_text(family = "roboto"))

# Create bigrams
tokens_ngrams <- tokens_ngrams(tokens, n = 2)

# Create a DFM for bigrams
dfm_ngrams <- dfm(tokens_ngrams)

# Summarize the most common bigrams
dfm_ngrams_df <- dfm_ngrams %>%
  textstat_frequency() %>%
  as_tibble() %>%
  arrange(desc(frequency))

# Plot common bigrams
plot4 <- dfm_ngrams_df %>%
  slice_max(frequency, n = 10) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = plot_color) +
  coord_flip() +
  labs(title = "Common Bigrams in WHO YouTube Comments",
       x = "Bigrams",
       y = "Frequency") +
  theme_bw() +
  theme(text = element_text(family = "roboto"))

# Combine plots
combined_plot <- (plot1 + plot2 + plot3) / plot4 +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Analysis of WHO YouTube Video Comments") &
  theme(text = element_text(family = "roboto"))

# Plot
print(combined_plot)
