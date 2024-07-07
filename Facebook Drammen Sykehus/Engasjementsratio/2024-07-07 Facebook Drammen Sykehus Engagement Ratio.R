
# Analyzing Facebook posts from Veste Viken Drammen Sykehus 
# Data collected from January 1st to July 6th. 
# This code looks into whether shorter of longer messages gets most engagement. 

# Load libraries
library(tidyverse)
library(readxl)

# Load .xlsx file with Facebook messages and engagement 
drammen_sykehus <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/Vestre Viken Drammen Sykehus/Drammen Sykehus Facebook Meldinger.xlsx")

# Calculate the number of words
drammen_sykehus <- drammen_sykehus %>%
  mutate(num_words = str_count(message, "\\w+"))

# Calculate engagement ratio
drammen_sykehus <- drammen_sykehus %>%
  mutate(engagement_ratio = (likes + shares + comments) / num_words)

# Plot engagement ratio
drammen_sykehus %>% 
  ggplot(aes(x = num_words, y = engagement_ratio)) +
  geom_point(alpha = 0.6, color = "#2C3E50", size = 3) +
  geom_smooth(method = "lm", color = "#E74C3C", se = FALSE, linetype = "dashed", size = 1) +
  labs(title = "Engasjementsratio versus meldingslengde", 
       subtitle = "Mer engasjement blant korte meldinger", 
       x = " ", 
       y = " ",
       caption = "R.A. Jacobsen | @AulieRoy | Source: Facebook"
       ) + 
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
