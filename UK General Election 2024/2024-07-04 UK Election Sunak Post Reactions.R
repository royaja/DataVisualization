# The UK general election 2024: Messages from Rishi Sunak 
# Data drawn from Facebook 

# Load packages 
library(tidyverse)
library(readxl)
library(ggtext)
library(lubridate)

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

# Summarize the data to find the most common reactions
reactions_summary <- 
  filtered_election %>%
  summarize(
    like = sum(likes, na.rm = TRUE),
    love = sum(heart, na.rm = TRUE),
    haha = sum(laughs, na.rm = TRUE),
    care = sum(heartface, na.rm = TRUE),
    wow = sum(wow, na.rm = TRUE),
    sad = sum(sad, na.rm = TRUE),
    angry = sum(angry, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "reaction", values_to = "count") %>%
  arrange(desc(count))

# Define emojis
emoji_labels <- c("like" = "\U0001F44D", "love" = "\U0001F496", "haha" = "\U0001F606",
                  "wow" = "\U0001F62E", "sad" = "\U0001F622", "angry" = "\U0001F620", "care" = "\U0001F9E1")

# Viz: barplot 
reactions_summary %>% 
  ggplot(aes(x = reorder(reaction, -count), y = count, fill = reaction)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  geom_richtext(aes(label = emoji_labels[reaction]), 
                vjust = -0.5, 
                fill = NA, 
                label.color = NA,
                size = 6,
                family = "emoji") +
  labs(title = "Most Common Facebook Reactions on Rishi Sunak's Posts (Last Two Days)",
       x = " ",
       y = " ", 
       caption = "R.A. Jacobsen | @AulieRoy | Source: Facebook") +
  ylim(0, max(reactions_summary$count) + 1000) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size = 20, face = "bold"), 
    plot.caption = element_text(size = 8, hjust = 0)
  )
