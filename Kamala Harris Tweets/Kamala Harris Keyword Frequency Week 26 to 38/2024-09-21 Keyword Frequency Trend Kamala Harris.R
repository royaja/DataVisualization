# Data comes from the Twitter/X account of Kamala Harris. 
# Analysis was performed on September 1st. 2024. 
# All code is present within the R syntax file. 

# Load libraries 
library(tidyverse)
library(ndjson)
library(lubridate)
library(tidytext)
library(showtext)
library(sysfonts)
library(patchwork)

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

# Custom stop words
custom_stop_words <- c("http", 
                       "https", 
                       "rt", 
                       "vp")

kamala_df <- 
  kamala %>% 
  select(data.legacy.full_text, data.legacy.created_at) %>% 
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
  anti_join(stop_words, by = "word") %>%  
  mutate(
    date = as.Date(data.legacy.created_at, format = "%a %b %d %H:%M:%S %z %Y"),  
    week = isoweek(date) 
  )

# keyword
keywords <- c("president", "trump", "biden")

time_series_data <- 
  kamala_df %>%
  filter(word %in% keywords) %>%  
  count(week, word) %>%  
  complete(week = full_seq(week, 1), word, fill = list(n = 0)) 


# Add Google Font "Lora"
font_add_google("Lora", "lora")
showtext_auto()

# Colors
colors <- c("#C70039", "#FF5733", "#FFC300")  

# Background color
bg_color <- "#f6f1eb"

# Plots
plot_president <- ggplot(subset(time_series_data, word == "president"), aes(x = week, y = n)) +
  geom_area(fill = colors[1], alpha = 0.8) +
  geom_line(color = colors[1], size = 1) +
  geom_point(color = colors[1], size = 1.5) +
  scale_x_continuous(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal(base_family = "lora") +
  labs(title = "President") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

plot_trump <- ggplot(subset(time_series_data, word == "trump"), aes(x = week, y = n)) +
  geom_area(fill = colors[2], alpha = 0.8) +
  geom_line(color = colors[2], size = 1) +
  geom_point(color = colors[2], size = 1.5) +
  scale_x_continuous(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) + 
  theme_minimal(base_family = "lora") +
  labs(title = "Trump") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

plot_biden <- ggplot(subset(time_series_data, word == "biden"), aes(x = week, y = n)) +
  geom_area(fill = colors[3], alpha = 0.8) +
  geom_line(color = colors[3], size = 1) +
  geom_point(color = colors[3], size = 1.5) +
  scale_x_continuous(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) + 
  theme_minimal(base_family = "lora") +
  labs(title = "Biden") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

# Combine plots
combined_plot <- plot_president + plot_trump + plot_biden + 
  plot_annotation(
    title = "Exploring Keyword Frequency Over Time in Kamala Harris's Tweets Between Week 26 and 38 (Until September 21th)",
    subtitle = "The frequency of the terms 'President,' 'Trump,' and 'Biden' in Kamala Harris's tweets over time reveals that discussions around 'Trump' are most \n frequent, particularly in the earlier weeks. The term 'President' shows consistent mention across the period, while 'Biden' appears less frequently, \n with noticeable spikes in specific weeks.",
    caption = "R.A.Jacobsen | @AulieRoy | Source: Twitter/X",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", family = "lora", hjust = 0),
      plot.subtitle = element_text(size = 14, family = "lora", hjust = 0),
      plot.caption = element_text(size = 10, family = "lora", hjust = 0), 
      plot.background = element_rect(fill = bg_color, color = NA)
    )
  )

print(combined_plot)
