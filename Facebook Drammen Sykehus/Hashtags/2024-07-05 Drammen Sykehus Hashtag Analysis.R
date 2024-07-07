
# Analyzing Facebook posts from Veste Viken Drammen Sykehus 
# Data collected from January 1st to July 6th. 
# This code looks into popular hashtags. 

# Load required libraries 
library(tidyverse)
library(readxl)

# Load .xlsx file with Facebook messages and engagement 
drammen_sykehus <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Vestre Viken Drammen Sykehus/Drammen Sykehus Facebook Meldinger.xlsx")

# Wrangle data 
hashtags_drammen_sykehus <- 
  drammen_sykehus %>%
  mutate(hashtags = str_extract_all(message, "#\\S+")) %>%
  unnest(hashtags)

hashtag_counts <- 
  hashtags_drammen_sykehus %>%
  count(hashtags, sort = TRUE)

# top 10 most frequent hashtags
top_hashtags <- hashtag_counts %>%
  slice_max(n, n = 10)

# top 10 hashtags
top_hashtags %>% 
  ggplot(aes(x = reorder(hashtags, n), y = n)) +
  geom_bar(stat = "identity", aes(fill = hashtags == "#nyttsykehusidrammen"), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 11, by = 1), limits = c(0, 11)) +
  labs(
    title = "10 Mest Brukte Hashtags",
    subtitle = "Hashtagen #nyttsykehusidrammen brukes to ganger",
    x = " ",
    y = "Frequency", 
    caption = "R.A. Jacobsen | @AulieRoy | Source: Facebook"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "italic"),
    axis.text.y = element_text(face = "italic"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_blank()
  )
