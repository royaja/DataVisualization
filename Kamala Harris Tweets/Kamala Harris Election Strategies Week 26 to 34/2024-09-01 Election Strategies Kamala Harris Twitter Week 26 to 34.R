# Data comes from the Twitter/X account of Kamala Harris. 
# Analysis was performed on September 1st, 2024. 
# All code is present within the R syntax file. 

# Load libraries
library(tidyverse)
library(lubridate)
library(showtext)
library(sysfonts)
library(patchwork)

# Google Font Lora
font_add_google("Lora", "lora")
showtext_auto()

# Colors
colors <- c("#C70039", "#FF5733", "#FFC300", "#28B463", "#1F618D")  

# Background color
bg_color <- "#f6f1eb"

# Load data 
kamala_df <- read_csv("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/kamala_harris_dataset_sentiments_campaign_strategies.csv")

# Wrangle data 
kamala_df <- 
  kamala_df %>%
  mutate(date = as.Date(data.legacy.created_at, format = "%a %b %d %H:%M:%S %z %Y"))

# Acknowledgement
plot_acknowledgement <- 
  kamala_df %>%
  filter(acknowledgement == 1) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_area(fill = colors[1], alpha = 0.8) +
  geom_line(color = colors[1], size = 1) +
  geom_point(color = colors[1], size = 1.5) +
  scale_x_date(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal(base_family = "lora") +
  labs(title = "Strategy: Acknowledgement") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

# Critic
plot_critic <- 
  kamala_df %>%
  filter(critic == 1) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_area(fill = colors[2], alpha = 0.8) +
  geom_line(color = colors[2], size = 1) +
  geom_point(color = colors[2], size = 1.5) +
  scale_x_date(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal(base_family = "lora") +
  labs(title = "Strategy: Critic") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

# Personalization
plot_personalization <- 
  kamala_df %>%
  filter(personalization == 1) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_area(fill = colors[3], alpha = 0.8) +
  geom_line(color = colors[3], size = 1) +
  geom_point(color = colors[3], size = 1.5) +
  scale_x_date(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal(base_family = "lora") +
  labs(title = "Strategy: Personalization") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

# Information
plot_information <- 
  kamala_df %>%
  filter(information == 1) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_area(fill = colors[4], alpha = 0.8) +
  geom_line(color = colors[4], size = 1) +
  geom_point(color = colors[4], size = 1.5) +
  scale_x_date(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal(base_family = "lora") +
  labs(title = "Strategy: Information") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, family = "lora"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey40"),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

# Mobilization
plot_mobilization <- 
  kamala_df %>%
  filter(mobilization == 1) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_area(fill = colors[5], alpha = 0.8) +
  geom_line(color = colors[5], size = 1) +
  geom_point(color = colors[5], size = 1.5) +
  scale_x_date(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal(base_family = "lora") +
  labs(title = "Strategy: Mobilization") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, family = "lora"),
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
combined_plot <- 
  plot_acknowledgement + plot_critic + plot_personalization + plot_information + plot_mobilization +
  plot_annotation(
    title = "Trends in Election Strategies Used by Kamala Harris Over Time",
    subtitle = "Exploring Kamala Harris's use of election strategies between Week 26 and Week 34 (data until August 27th) on Twitter/X. Harris employs a blend of strategies to engage her followers, \nwith notable spikes across all strategies between August 1st and August 15th. The information strategy is consistently the most utilized, with frequent peaks throughout the period. \nAcknowledgement and critic strategies show significant activity, especially in late July and early August, while personalization and mobilization are used more selectively, with sporadic peaks.",
    caption = "R.A.Jacobsen | @AulieRoy | Source: Twitter/X",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", family = "lora", hjust = 0),
      plot.subtitle = element_text(size = 14, family = "lora", hjust = 0),
      plot.caption = element_text(size = 10, family = "lora", hjust = 0),
      plot.background = element_rect(fill = bg_color, color = NA)
    )
  )

print(combined_plot)
