
# Data comes from the Twitter/X account of the World Health Organization (WHO)
# Analysis was performed on August 27. 2024. 
# All code is present within the R syntax file. 

# Load libraries 
library(tidyverse)
library(ndjson)
library(lubridate)
library(patchwork)
library(tidytext)

# Load WHO Twitter Messages 
who_data <- ndjson::stream_in("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/2024-08-27 WHO Twitter Data July First.ndjson")

# Convert the date column to Date type
who_data$date <- as.Date(who_data$date, format="%Y-%m-%d")

# Extract the day of the week from the date
who_data$day_of_week <- weekdays(who_data$date)

# Perform a hashtag analysis to see which hashtags occur most often. 

hashtag_counts <- 
  who_data %>%
  filter(!is.na(data.legacy.entities.hashtags.0.text)) %>%
  group_by(data.legacy.entities.hashtags.0.text) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Plot 
hashtag_counts <- 
  hashtag_counts %>% 
  slice_max(order_by = count, n = 10) %>%  
  ggplot(aes(x = reorder(data.legacy.entities.hashtags.0.text, count), y = count)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +  
  geom_text(aes(label = count), hjust = -0.1, size = 5) +  
  coord_flip() +  
  labs(
    title = "Top 10 Hashtags",
    subtitle = "The most common hashtags used the last month by the WHO is mpox. Gaza, and Sudan",
    x = "Hashtags",
    y = "Frequency"
  ) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), expand = c(0, 0)) + 
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()  
  )


# Examining days with the highest number of posts

who_data$date_parsed <- parse_date_time(who_data$data.legacy.created_at, orders = "a b d H:M:S z Y")

who_data$day_of_week <- weekdays(who_data$date_parsed)

# Prepare the data
posts_per_day <- 
  who_data %>%
  count(day_of_week) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  arrange(day_of_week)

# Plot
posts_per_day <- 
  posts_per_day %>% 
  ggplot(aes(x = day_of_week, y = n)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) + 
  geom_text(aes(label = n), vjust = -0.3, size = 5) +  
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(posts_per_day$n) * 1.1)) + 
  labs(
    title = "Posts frequency by day of the week",
    subtitle = "Monday has the highest number of posts", 
    x = "Day of the week",
    y = "Number of posts"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()  
  )


#### Stacked barchart 

# Create a new column categorizing the media type
who_data$media_type <- ifelse(is.na(who_data$data.legacy.entities.media.0.type), "None", who_data$data.legacy.entities.media.0.type)

posts_per_day_media <- 
  who_data %>%
  filter(media_type %in% c("None", "photo", "video")) %>%
  group_by(day_of_week, media_type) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

posts_per_day_media <- 
  posts_per_day_media %>%
  mutate(media_type = recode(media_type, 
                             "photo" = "Photo", 
                             "video" = "Video"))

# Create the stacked bar chart
posts_per_day_media <- 
  posts_per_day_media %>% 
  ggplot(aes(x = day_of_week, y = count, fill = media_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), expand = c(0, 0)) + 
  scale_fill_manual(values = c("None" = "#fed966", "Photo" = "#9ad1d4", "Video" = "#f4a261")) + 
  labs(
    title = "Posts frequency by day of the week and media type",
    subtitle = "Monday, Wednesday, and Thursday are the days with most posts", 
    x = "Day of the week",
    y = "Number of posts",
    fill = "Media type"
  ) +
  theme_classic(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank(),  
    legend.position = "top"
  )


engagement_by_media <- 
  who_data %>%
  filter(media_type %in% c("None", "photo", "video")) %>%
  group_by(media_type) %>%
  summarize(
    avg_likes = round(mean(data.legacy.favorite_count, na.rm = TRUE)),
    avg_retweets = round(mean(data.legacy.retweet_count, na.rm = TRUE)),
    avg_replies = round(mean(data.legacy.reply_count, na.rm = TRUE)),
    count = n()
  ) %>%
  mutate(media_type = recode(media_type, 
                             "None" = "No Media",
                             "photo" = "Photo",
                             "video" = "Video"))


engagement_plot <- 
  engagement_by_media %>%
  pivot_longer(cols = starts_with("avg"), names_to = "engagement_type", values_to = "average_count") %>%
  ggplot(aes(x = media_type, y = average_count, fill = engagement_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("avg_likes" = "#fed966", "avg_retweets" = "#66d7fe", "avg_replies" = "#8d66fe"),
                    labels = c("avg_likes" = "Likes", "avg_retweets" = "Retweets", "avg_replies" = "Replies")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,250)) +  
  labs(
    title = "Average Engagement by Media Type",
    subtitle = "Comparison of Likes, Retweets, and Replies",
    x = "Media Type",
    y = "Average Engagement",
    fill = " "
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top"
  )



# Mentiones 

mentions_count <- 
  who_data %>%
  filter(!is.na(data.legacy.entities.user_mentions.0.name)) %>% 
  group_by(data.legacy.entities.user_mentions.0.name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice_max(order_by = count, n = 10)  

# Plot
mentions_count <- 
  mentions_count %>% 
  ggplot(aes(x = reorder(data.legacy.entities.user_mentions.0.name, count), y = count)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +  
  geom_text(aes(label = count), hjust = -0.5, size = 5, color = "black") +  
  coord_flip() +
  labs(
    title = "Top 10 Mentions in WHO Posts",
    subtitle = "Tedros Ghebreyesus was mentioned in most posts",
    x = "User Mentioned",
    y = "Number of Mentions"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()  
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,110)) 


# Hashtags that get the most likes 

hashtag_summary <- 
  who_data %>%
  filter(!is.na(data.legacy.entities.hashtags.0.text)) %>%  
  group_by(data.legacy.entities.hashtags.0.text) %>%
  summarize(
    total_favorites = sum(data.legacy.favorite_count, na.rm = TRUE),
    total_retweets = sum(data.legacy.retweet_count, na.rm = TRUE),
    total_replies = sum(data.legacy.reply_count, na.rm = TRUE)
  ) %>%
  arrange(desc(total_favorites))

plot_favorites <- 
  hashtag_summary %>% 
  arrange(desc(total_favorites)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(data.legacy.entities.hashtags.0.text, total_favorites), y = total_favorites)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +  
  geom_text(aes(label = total_favorites), hjust = -0.5, size = 5, color = "black") +  
  coord_flip() +
  labs(
    title = "Top 10 Hashtags by Favorite Count",
    x = "Hashtag",
    y = "Favorites"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10000))


plot_retweets <- 
  hashtag_summary %>% 
  arrange(desc(total_retweets)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(data.legacy.entities.hashtags.0.text, total_retweets), y = total_retweets)) +
  geom_bar(stat = "identity", fill = "#66d7fe", width = 0.7) +
  geom_text(aes(label = total_retweets), hjust = -0.5, size = 5, color = "black") +  
  coord_flip() +
  labs(
    title = "Top 10 Hashtags by Retweet Count",
    x = "Hashtag",
    y = "Retweets"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,11000))

  
plot_replies <- 
  hashtag_summary %>% 
  arrange(desc(total_replies)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(data.legacy.entities.hashtags.0.text, total_replies), y = total_replies)) +
  geom_bar(stat = "identity", fill = "#8d66fe", width = 0.7) +
  geom_text(aes(label = total_replies), hjust = -0.5, size = 5, color = "black") +  
  coord_flip() +
  labs(
    title = "Top 10 Hashtags by Reply Count",
    x = "Hashtag",
    y = "Total Replies"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,11000))


# Sentiment analysis 

who_tokenized <- 
  who_data %>%
  mutate(clean_text = tolower(data.legacy.full_text)) %>%  
  mutate(clean_text = str_replace_all(clean_text, "https?://\\S+\\s?", "")) %>%  
  mutate(clean_text = str_replace_all(clean_text, "[^\\w\\s]", "")) %>%  
  unnest_tokens(word, clean_text) %>%  
  filter(nchar(word) >= 4) %>%  
  anti_join(stop_words, by = "word")

sentiment_data <- 
  who_tokenized %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative")))

sentiment_summary <- 
  sentiment_data %>%
  summarize(
    positive = sum(n[sentiment == "positive"]),
    negative = sum(n[sentiment == "negative"])
  ) %>%
  mutate(neutral = nrow(who_data_tokens) - (positive + negative)) 

sentiment_summary_df <- 
  sentiment_summary %>%
  pivot_longer(cols = everything(), names_to = "sentiment", values_to = "count") %>%
  mutate(sentiment = factor(sentiment, levels = c("positive","neutral", "negative")))


sentiment_summary_df <- 
  sentiment_summary_df %>% 
  ggplot(aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("positive" = "darkgreen", "neutral" = "gray70", "negative" = "darkred")) +
  labs(
    title = "Sentiment Analysis of WHO Posts", 
    subtitle = "Sentiments based on the Bing dictionary", 
    x = "Sentiment", 
    y = "Count", 
    fill = " "
    ) +
  theme_classic(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank(), 
    legend.position = "none"
  )

# Biagrams 

who_bigrams <- 
  who_data %>%
  mutate(clean_text = tolower(data.legacy.full_text)) %>%  
  mutate(clean_text = str_replace_all(clean_text, "https?://\\S+\\s?", "")) %>%  
  mutate(clean_text = str_replace_all(clean_text, "[^\\w\\s]", "")) %>%  
  unnest_tokens(bigram, clean_text, token = "ngrams", n = 2)

bigram_counts <- 
  who_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>%  
  count(bigram, sort = TRUE) %>% 
  slice_head(n = 10) 
  

bigram_counts <- 
  bigram_counts %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 5) +  
  coord_flip() +
  labs(
    title = "Top 10 Bigrams in WHO Posts",
    x = "Bigram",
    y = "Frequency"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()  
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,18))


# Words that attract most engagement 

word_engagement <- 
  who_data %>%
  mutate(clean_text = tolower(data.legacy.full_text)) %>%
  mutate(clean_text = str_replace_all(clean_text, "https?://\\S+\\s?", "")) %>%
  mutate(clean_text = str_replace_all(clean_text, "[^\\w\\s]", "")) %>%
  unnest_tokens(word, clean_text) %>%
  filter(nchar(word) >= 4) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(word) %>%
  summarize(
    avg_likes = round(mean(data.legacy.favorite_count, na.rm = TRUE)), 
    avg_retweets = round(mean(data.legacy.retweet_count, na.rm = TRUE)),  
    avg_replies = round(mean(data.legacy.reply_count, na.rm = TRUE)), 
    count = n()
  ) %>%
  filter(count >= 5)

word_engagement <- 
  word_engagement %>%
  mutate(engagement_score = round(avg_likes + avg_retweets + avg_replies)) %>% 
  arrange(desc(engagement_score)) %>%
  slice_max(order_by = engagement_score, n = 10)

word_engagement <- 
  word_engagement %>% 
  ggplot(aes(x = reorder(word, engagement_score), y = engagement_score)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +  
  geom_text(aes(label = engagement_score), hjust = -0.1, size = 5) +  
  coord_flip() +
  labs(
    title = "Top 15 Words by Engagement Score",
    subtitle = "Words that attract the most attention in WHO tweets",
    x = "Word",
    y = "Engagement Score (Sum of Avg. Likes, Retweets, Replies)"
  ) +
  theme_classic(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)), 
    axis.title = element_text(face = "bold", size = 14),  
    axis.text = element_text(size = 12), 
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.x = element_blank()  
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1500))



hashtag_engagement <- 
  who_data %>%
  filter(!is.na(data.legacy.entities.hashtags.0.text)) %>%
  group_by(hashtag = data.legacy.entities.hashtags.0.text) %>%
  summarize(
    avg_likes = round(mean(data.legacy.favorite_count, na.rm = TRUE)),
    avg_retweets = round(mean(data.legacy.retweet_count, na.rm = TRUE)),
    avg_replies = round(mean(data.legacy.reply_count, na.rm = TRUE)),
    count = n()
  ) %>%
  mutate(engagement_score = avg_likes + avg_retweets + avg_replies) %>%
  arrange(desc(engagement_score))


mention_engagement <- 
  who_data %>%
  filter(!is.na(data.legacy.entities.user_mentions.0.name)) %>%
  group_by(mention = data.legacy.entities.user_mentions.0.name) %>%
  summarize(
    avg_likes = round(mean(data.legacy.favorite_count, na.rm = TRUE)),
    avg_retweets = round(mean(data.legacy.retweet_count, na.rm = TRUE)),
    avg_replies = round(mean(data.legacy.reply_count, na.rm = TRUE)),
    count = n()
  ) %>%
  mutate(engagement_score = avg_likes + avg_retweets + avg_replies) %>%
  arrange(desc(engagement_score))


hashtag_engagement <- 
  hashtag_engagement %>%
  slice_max(order_by = engagement_score, n = 10) %>%
  ggplot(aes(x = reorder(hashtag, engagement_score), y = engagement_score)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +
  geom_text(aes(label = engagement_score), hjust = -0.1, size = 5) +
  coord_flip() +
  labs(
    title = "Top 10 Hashtags by Engagement Score",
    subtitle = "Hashtags that attract the most attention in WHO tweets",
    x = "Hashtag",
    y = "Engagement Score (Sum of Avg. Likes, Retweets, Replies)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1500))


mention_engagement <- 
  mention_engagement %>%
  slice_max(order_by = engagement_score, n = 10) %>%
  ggplot(aes(x = reorder(mention, engagement_score), y = engagement_score)) +
  geom_bar(stat = "identity", fill = "#fed966", width = 0.7) +  
  geom_text(aes(label = engagement_score), hjust = -0.1, size = 5) +  
  coord_flip() +
  labs(
    title = "Top 10 Mentions by Engagement Score",
    subtitle = "Mentions that attract the most attention in WHO tweets",
    x = "Mention",
    y = "Engagement Score (Sum of Avg. Likes, Retweets, Replies)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 0.8),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1500))


# Merge into three figures

background_color <- "#C4A484"

# Figure 1: Hashtags and mentions 
figure1 <- (hashtag_counts + mentions_count) / 
  (plot_favorites + plot_retweets + plot_replies) + 
  plot_annotation(
    title = "Hashtag and mentions analysis of WHO posts from July 27. to August 27. 2024",
    subtitle = "Examining the most frequent and engaged hashtags and mentions in WHO posts",
    caption = "R.A. Jacobsen | @AulieRoy | source: Twitter data from the World Health Organization (WHO)\nAnalysis was performed on August 28, 2024.",
    theme = theme(
      plot.background = element_rect(fill = background_color, color = NA),
      plot.title = element_text(size = 20, face = "bold", color = "white"),
      plot.subtitle = element_text(size = 16, face = "italic", color = "white"),
      plot.caption = element_text(size = 12, color = "white", hjust = 0)
    )
  )

# Figure 2: Engagement Analysis
figure2 <- (engagement_plot + word_engagement) / 
  (hashtag_engagement + mention_engagement) + 
  plot_annotation(
    title = "Engagement analysis of WHO posts from July 27. to August 27. 2024",
    subtitle = "Exploring the average engagement across media types, words, hashtags, and mentions",
    caption = "R.A. Jacobsen | @AulieRoy | source: Twitter data from the World Health Organization (WHO)\nAnalysis was performed on August 28, 2024.",
    theme = theme(
      plot.background = element_rect(fill = background_color, color = NA),
      plot.title = element_text(size = 20, face = "bold", color = "white"),
      plot.subtitle = element_text(size = 16, face = "italic", color = "white"),
      plot.caption = element_text(size = 12, color = "white", hjust = 0)
    )
  )

# Figure 3: Posting Behavior Analysis
figure3 <- (posts_per_day / posts_per_day_media) + 
  plot_annotation(
    title = "Posting behavior analysis of WHO posts from July 27. to August 27. 2024",
    subtitle = "Analyzing WHO's posting frequency by day of the week and media type",
    caption = "R.A. Jacobsen | @AulieRoy | source: Twitter data from the World Health Organization (WHO)\nAnalysis was performed on August 28, 2024.",
    theme = theme(
      plot.background = element_rect(fill = background_color, color = NA),
      plot.title = element_text(size = 20, face = "bold", color = "white"),
      plot.subtitle = element_text(size = 16, face = "italic", color = "white"),
      plot.caption = element_text(size = 12, color = "white", hjust = 0)
    )
  )

# Figure 4: Textual Analysis
figure4 <- (sentiment_summary_df / bigram_counts) + 
  plot_annotation(
    title = "Textual analysis of WHO posts from July 27. to August 27. 2024",
    subtitle = "Sentiment and bigram analysis of WHO's tweets",
    caption = "R.A. Jacobsen | @AulieRoy | source: Twitter data from the World Health Organization (WHO)\nAnalysis was performed on August 28, 2024.",
    theme = theme(
      plot.background = element_rect(fill = background_color, color = NA),
      plot.title = element_text(size = 20, face = "bold", color = "white"),
      plot.subtitle = element_text(size = 16, face = "italic", color = "white"),
      plot.caption = element_text(size = 12, color = "white", hjust = 0)
    )
  )


ggsave("figure1_hashtags_mentions.png", plot = figure1, width = 28, height = 14, dpi = 300)
ggsave("figure2_engagement.png", plot = figure2, width = 16, height = 12, dpi = 300)
ggsave("figure3_posting_behavior.png", plot = figure3, width = 16, height = 12, dpi = 300)
ggsave("figure4_textual_analysis.png", plot = figure4, width = 16, height = 12, dpi = 300)
