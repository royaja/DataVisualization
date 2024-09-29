# Data comes from the Twitter/X account of Kamala Harris. 
# Analysis was performed on September 27th, 2024. 
# All code is present within the R syntax file. 

# Load libraries 
library(tidyverse)
library(ndjson)
library(lubridate)
library(tidytext)
library(showtext)

# Load data
kamala_harris <- ndjson::stream_in("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/2024-08-27 Kamala Harris August Twitter Posts.ndjson")
kamala_harris_more <- ndjson::stream_in("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/2024-09-21 kamala harris twitter.ndjson")

# Merge data 
kamala_harris <- 
  kamala_harris %>%
  mutate(data.legacy.created_at = as.POSIXct(data.legacy.created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC")) %>%
  filter(data.legacy.created_at <= as_datetime("2024-08-26 23:59:59", tz = "UTC"))

kamala_harris_more <- 
  kamala_harris_more %>%
  mutate(data.legacy.created_at = as.POSIXct(data.legacy.created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC")) %>%
  filter(data.legacy.created_at >= as_datetime("2024-08-27 00:00:00", tz = "UTC"))

kamala_harris_combined <- bind_rows(kamala_harris, kamala_harris_more)

kamala <- 
  kamala_harris_combined %>%
  distinct(data.legacy.created_at, .keep_all = TRUE)
  

kamala_sentiment <- 
  kamala %>%
  mutate(week = week(ymd_hms(data.legacy.created_at))) %>%  
  unnest_tokens(word, data.legacy.full_text) %>%  
  inner_join(get_sentiments("bing"), by = "word") %>%  
  count(week, sentiment) %>%  
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment_score = positive - negative)

# Google Font Lora
font_add_google("Lora", "lora")
showtext_auto()

bg_color <- "#f6f1eb"

kamala_sentiment %>% 
  ggplot(aes(x = week, y = sentiment_score)) + 
  geom_area(fill = "#28B463", alpha = 0.8) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +  
  theme_minimal(base_family = "lora") +
  labs(
    title = "Sentiment Score: Kamala Harris Tweets Week 26 to Week 38", 
    subtitle = "The sentiment score for tweets by Kamala Harris fluctuates from week 26 to week 38, indicating a shift in the public's reaction to the messages, with peaks in positive sentiments during week 29 and 36.", 
    x = "Week", 
    y = "Sentiment Score", 
    caption = "R.A.Jacobsen | @AulieRoy | Source: Twitter/X",
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", family = "lora", hjust = 0),
    plot.subtitle = element_text(size = 14, family = "lora", hjust = 0),
    plot.caption = element_text(size = 8, family = "lora", hjust = 0),
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"), 
    axis.text.x = element_text(size = 10, family = "lora"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"), 
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )
