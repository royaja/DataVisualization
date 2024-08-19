
library(tidyverse)
library(tidytext)
library(stopwords)
library(showtext)
library(patchwork)

# Add Roboto font
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

# Load data
bbcmpoxcomments <- read_csv("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Mpox YouTube/Mpox YouTube News Channel Data/bbc_youtube_comments_mpox.csv")
dwmpoxcomments <- read_csv("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Mpox YouTube/Mpox YouTube News Channel Data/dw_youtube_comments_mpox.csv")
skynewsmpoxcomments <- read_csv("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Mpox YouTube/Mpox YouTube News Channel Data/skynews_youtube_comments_mpox.csv")

# Convert PublishedAt to datetime<UTC> format in all datasets
bbcmpoxcomments <- bbcmpoxcomments %>%
  mutate(PublishedAt = as.POSIXct(PublishedAt, format="%Y-%m-%dT%H:%M:%S", tz="UTC"))

dwmpoxcomments <- dwmpoxcomments %>%
  mutate(PublishedAt = as.POSIXct(PublishedAt, format="%Y-%m-%dT%H:%M:%S", tz="UTC"))

skynewsmpoxcomments <- skynewsmpoxcomments %>%
  mutate(PublishedAt = as.POSIXct(PublishedAt, format="%Y-%m-%dT%H:%M:%S", tz="UTC"))

# Merge data
all_comments <- bind_rows(bbcmpoxcomments, dwmpoxcomments, skynewsmpoxcomments)

all_comments <- 
  all_comments %>% 
  janitor::clean_names() %>% 
  mutate(id = row_number()) 

# stopword
english_stopwords <- stopwords(language = "en", source = "stopwords-iso")

stopwords_df <- data.frame(word = english_stopwords, stringsAsFactors = FALSE)

# Process comments
all_comments_clean <- all_comments %>%
  unnest_tokens(word, comment) %>%  
  filter(nchar(word) >= 4) %>%     
  anti_join(stopwords_df, by = "word") %>%  
  filter(!word %in% c("https", "t.co", "amp")) %>%  
  count(source, word, sort = TRUE) 


# 10 most common words
top_words <- all_comments_clean %>%
  group_by(source) %>%
  top_n(10, n) %>%
  ungroup()


# total number of comments
total_comments <- top_words %>%
  group_by(source) %>%
  summarize(total_comments = sum(n))

caption_text <- paste("Total comments - BBC:", total_comments$total_comments[total_comments$source == "BBC"], 
                      "| DW:", total_comments$total_comments[total_comments$source == "DW"], 
                      "| Sky News:", total_comments$total_comments[total_comments$source == "Sky News"])


# Plot 
plot1 <- 
  top_words %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = source)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ source, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top Words in YouTube Comments Across News Channels",
    subtitle = "Analysis of frequently used words in comments from BBC, DW, and Sky News.'People' is the most common word across all channels, \n with varying emphasis on other terms such as 'mpox' and 'covid'.",
    x = "Word",
    y = "Frequency"
  ) +
  theme_classic() +  
  theme(
    text = element_text(family = "roboto", size = 14),  
    plot.title = element_text(size = 20, face = "bold"), 
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)), 
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 16, face = "bold", color = "black"),  
    strip.background = element_rect(fill = "white", color = "black"),  
    panel.grid.major = element_line(color = "gray90"), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    panel.spacing = unit(1, "lines"),  
    plot.title.position = "plot",  
    plot.caption = element_text(size = 10, hjust = 1)
  ) +
  scale_fill_manual(values = c("BBC" = "#BB1919", "DW" = "#1F3D7A", "Sky News" = "#AA1E22"))


# bing sentiment lexicon
bing_sentiments <- get_sentiments("bing")

# sentiment analysis
sentiment_analysis <- all_comments_clean %>%
  inner_join(bing_sentiments, by = "word") %>%  
  count(source, sentiment)  

# plot 
plot2 <- 
  sentiment_analysis %>% 
  ggplot(aes(x = source, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 4, 
            family = "Roboto") +  
  labs(
    title = "Sentiment Analysis of YouTube Comments",
    x = "News Source",
    y = "Count",
    caption = "Note. Positive sentiment is indicated in green, negative sentiment in red."
  ) +
  ylim(0, 850) + 
  theme_classic() +
  scale_fill_manual(values = c("positive" = "#66c2a5", "negative" = "#e07b6d")) +
  theme(
    text = element_text(family = "Roboto", size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    legend.title = element_blank(),  
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0.5)
  )

combined_plot <- plot1 / plot2 + plot_layout(ncol = 1) 

combined_plot

