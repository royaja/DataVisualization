
# Created 29.03.2024 
# This code look into the sentiments and character count of the last 100 
# posts before the Norwegian local election in 2023 

# Load libraries
library(tidyverse)
library(patchwork)
library(readxl)

# Load dataset
facebook_election_2023 <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/[81.01] Facebook/Valget 2023/facebook_election_2023_dataset.xlsx")

# Bar plot for sentiments
sentiments_plot <- ggplot(facebook_election_2023, aes(x = sentiments, fill = sentiments)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 3, color="black") + 
  scale_fill_manual(values = c("Positive" = "darkgreen", "Neutral" = "grey", "Negative" = "darkred")) +
  labs(
    x = NULL, 
    y = "Count", 
    title = "Sentiment Distribution for the 100 Latest Posts During the Norwegian Local Election 2023"
  ) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.title = element_text(size = 14, face = "bold")
  )

# Plotting the bar chart for post length
post_length_plot <- ggplot(facebook_election_2023, aes(x = post_length_category)) +
  geom_bar(fill = "#C79B00") +
  geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 3, color="black") +
  labs(
    x = NULL,
    y = "Count",
    title = "Post length: Long > 150, Medium <= 150, Short <= 50"
  ) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.title = element_text(size = 14, face = "bold")
  )

# Combine the two plots
combined_plots <- sentiments_plot / post_length_plot

# Display the combined plot
combined_plots
