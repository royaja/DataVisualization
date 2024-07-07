# Analyzing Facebook posts from Veste Viken Drammen Sykehus 
# Data collected from January 1st to July 6th. 
# This code generates a wordclout of the most common words in messages. 

library(tidyverse)
library(readxl)
library(wordcloud)
library(tidytext)
library(stopwords)

# Load the data from the Excel file
drammen_sykehus <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Vestre Viken Drammen Sykehus/Drammen Sykehus Facebook Meldinger.xlsx")

# Combine all messages into a single text
messages_text <- 
  drammen_sykehus %>% 
  select(message) %>%
  filter(!is.na(message)) %>%
  pull() %>%
  paste(collapse = " ")

# Get Norwegian stopwords from the stopwords package
norwegian_stopwords <- stopwords(language = "no")

# Custome stopwords 
custom_stopwords <- c("http", "https", "also", "and", "so", "www", "com", "net", "org", "ikk", "ikke")

# Process the text: remove hashtags, emojis, stopwords, words shorter than 4 characters, and symbols
processed_text <- tibble(text = messages_text) %>%
  mutate(text = str_remove_all(text, "#\\S+")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% norwegian_stopwords) %>%
  filter(!word %in% custom_stopwords) %>%
  filter(nchar(word) >= 4) %>%
  filter(!str_detect(word, "[[:punct:]]"))

# Calculate word frequencies
word_freq <- processed_text %>%
  count(word, sort = TRUE)

# Define a single color for all words
all_words_color <- rep("red", nrow(word_freq))

# Generate the word cloud
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100, random.order = FALSE, colors = all_words_color)
