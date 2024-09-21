
# Data comes from the Twitter/X account of Kamala Harris. 
# Analysis was performed on September 1st. 2024. 
# All code is present within the R syntax file. 

# Load libraries 
library(tidyverse)
library(tidytext)
library(showtext)
library(sysfonts)

# Load data 
kamala <- read_csv("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/kamala_harris_dataset_sentiments_campaign_strategies.csv")

# Sentiments were found using the ChatGPT API and the GPT-4o model.
# Separate code with API key. 

# Sentiments 
sentiment_summary <- 
  kamala %>%
  count(gpt) %>%
  mutate(sentiment = case_when(
    gpt == 1 ~ "Positive",
    gpt == 2 ~ "Neutral",
    gpt == 3 ~ "Negative"
  )) 

sentiment_summary <- 
  sentiment_summary %>%
  mutate(sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative")))

# Finding examples of words that provoke "Positive", "Neutral", "Negative" sentiments. 
# Using the Bing lexicon to perform this task. 

custom_stop_words <- c("http", 
                       "https", 
                       "rt", 
                       "vp")

kamala_words <- 
  kamala %>%
  unnest_tokens(word, data.legacy.full_text) %>% 
  mutate(
    word = str_remove_all(word, "https?://\\S+|www\\.\\S+"),  
    word = str_remove_all(word, "\\d+"),  
    word = str_remove_all(word, "[^\\w\\s]"),  
    word = str_remove_all(word, "\\b\\d{1,2}\\b")  
  ) %>% 
  filter(nchar(word) >= 4) %>%  
  filter(!word %in% custom_stop_words) %>%  
  filter(word != "") %>%  
  anti_join(stop_words, by = "word")  
  
bing_lexicon <- get_sentiments("bing")

sentiment_analysis <- 
  kamala_words %>%
  inner_join(bing_lexicon, by = "word")


positive_words <- 
  sentiment_analysis %>%
  filter(sentiment == "positive") %>%
  select(word) %>%
  distinct() %>%
  head(10) %>%
  pull()

negative_words <- 
  sentiment_analysis %>%
  filter(sentiment == "negative") %>%
  select(word) %>%
  distinct() %>%
  head(10) %>%
  pull()

neutral_words <- 
  kamala_words %>%
  anti_join(bing_lexicon, by = "word") %>%
  select(word) %>%
  distinct() %>%
  head(10) %>%
  pull()

# Words not classified as positive or negative is considered to be neutral. 
positive_words
negative_words
neutral_words

# Words examples based on Bing lexicon
positive_words <- "succeed, grateful, promise"
neutral_words <- "economy, chance, opportunity"
negative_words <- "attack, poverty, abused"

# Google Font "Lora"
font_add_google("Lora", "lora")
showtext_auto()

# Colors
colors <- c("#C70039", "#FF5733", "#FFC300")  

# Background color
bg_color <- "#f6f1eb"

# Plot 
sentiment_summary %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +  
  geom_text(aes(label = n), vjust = -0.5, size = 5, family = "lora") +  
  annotate("text", x = 1, y = max(sentiment_summary$n) + 15, label = positive_words, size = 5, family = "lora", color = "#C70039") +  
  annotate("text", x = 2, y = max(sentiment_summary$n) + 15, label = neutral_words, size = 5, family = "lora", color = "#FF5733") + 
  annotate("text", x = 3, y = max(sentiment_summary$n) + 15, label = negative_words, size = 5, family = "lora", color = "#FFC300") +  
  labs(
    title = "Overall Sentiments in Tweets (Week 26 to Week 34)",
    subtitle = "A set of sentiments were derived from Kamala Harris's tweets between Week 26 and Week 34 (data ranged until August 27th). Most tweets carry an overall positive sentiment, \nusing words such as 'succeed,' 'grateful,' and 'promise.' Tweets provoking negative sentiments are the least common but typically include words like 'attack,' 'poverty,' and 'abuse.' \nNeutral tweets contain words that are not considered either positive or negative, such as 'economy,' 'chance,' and 'opportunity.' The overall classification of sentiments was done \nusing the LLM GPT-4, with individual words identified using the Bing lexicon. Overall, messages from Kamala Harris predominantly evoke positive sentiments.",
    caption = "R.A.Jacobsen | @AulieRoy | Source: Twitter/X",
    x = NULL,  
    y = NULL   
  ) +
  theme_minimal(base_family = "lora") +  
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 16, hjust = 0),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.ticks.x = element_line(color = "black"),  
    axis.text.y = element_blank(),  
    legend.position = "none",  
    axis.text.x = element_text(size = 12, face = "bold") 
  )
