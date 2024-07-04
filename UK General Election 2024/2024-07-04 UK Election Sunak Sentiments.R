
# The UK general election 2024: Messages from Rishi Sunak 
# Data drawn from Facebook 

# Load library
library(tidyverse)
library(readxl)
library(lubridate)
library(tidytext)

# Load dataset 
uk_election <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Election United Kingdom 2024/Election United Kingdom 2024.xlsx")

# Wrangle data 
glimpse(uk_election)

uk_election <- 
  uk_election %>% 
  mutate(date = as_date(post_published_date))

# Filter the data for the last two days
filtered_election <- 
  uk_election %>%
  filter(date >= today() - days(2))

# Tokenize messages and perform sentiment analysis
sentiment_plot <- filtered_election %>%
  mutate(word = str_extract_all(message, "\\w+")) %>%
  unnest(word) %>%
  inner_join(get_sentiments("bing"), by = c(word = "word")) %>%
  count(message, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(
    positive = if_else(is.na(positive), 0, positive),
    negative = if_else(is.na(negative), 0, negative),
    total = positive + negative
  ) %>%
  gather(sentiment, count, positive:negative) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative")))

# Viz
sentiment_plot  <- 
  sentiment_plot %>% 
  ggplot(aes(x = message, y = count, fill = sentiment)) +
  geom_col(position = "stack") +
  labs(
    x = " ", 
    y = "Count", 
    fill = "Sentiment",
    title = "Sentiment Analysis of Messages (Using the Bing Lexicon)",
    subtitle = "Counting occurance of negative and positive words from the Bing lexicon",
    caption = "R.A. Jacobsen | @AulieRoy | Source: Facebook"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  scale_fill_manual(values = c("positive" = "#1b9e77", "negative" = "#E3120B"))

# Save plot
ggsave("sentiment_plot.png", plot = sentiment_plot, width = 10, height = 6, dpi = 300)
