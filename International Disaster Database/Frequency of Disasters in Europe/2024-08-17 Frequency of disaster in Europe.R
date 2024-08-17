# Data from the international disaster database. 
# Link to EMDAT: https://www.emdat.be/

# Code developed by Roy Aulie Jacobsen. 

# Load necessary libraries
library(tidyverse)
library(readxl)
library(viridis)

# international disaster database
emdat <- read_xlsx("/mnt/data/public_emdat.xlsx")

# Clean columns
emdat <- 
  emdat %>% 
  janitor::clean_names()

# Define European countries
european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
                        "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania",
                        "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", "Portugal",
                        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
                        "United Kingdom")

# Filter columns
disaster_data <- 
  emdat %>%
  filter(!is.na(disaster_subtype) & !is.na(country) & country %in% european_countries) %>%
  count(country, disaster_subtype)

# Plot
disaster_data %>% 
  ggplot(aes(x = disaster_subtype, y = country, fill = n)) +
  geom_tile() +
  scale_fill_viridis(option = "rocket", direction = -1) + 
  theme_bw(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10), 
    axis.text.y = element_text(size = 9),  
    plot.margin = margin(10, 10, 10, 10), 
    legend.position = "top", 
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.title = element_text(size = 14),
  ) +
  labs(
    title = "Frequency of Disasters Across European Countries (2000-2023)", 
    x = " ",
    y = " ",
    fill = "Frequency",
    caption = "R.A. Jacobsen | @AulieRoy | Source: International Disaster Database"
  )

