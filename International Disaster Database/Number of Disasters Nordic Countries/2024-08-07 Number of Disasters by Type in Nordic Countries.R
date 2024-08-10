
# Data from the international disaster database. 
# Link to EMDAT: https://www.emdat.be/
# Economic damage by disasters in the UK.

# Code developed by Roy Aulie Jacobsen. 

# Load libraries. 
library(tidyverse)
library(readxl)

# international disaster database 
emdat <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/International Disaster Database/public_emdat.xlsx")

# Clean columns
emdat <- 
  emdat %>% 
  janitor::clean_names()

# List Nordic countries
nordic_countries <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")

# Filter Nordic countries
nordic_data <- 
  emdat %>% 
  filter(country %in% nordic_countries)

# Number of disasters
nordic_data %>%
  count(disaster_type) %>%
  ggplot(aes(x = reorder(disaster_type, n), y = n, fill = disaster_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.5, size = 4) +  
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold"), 
    plot.caption = element_text(size = 8, hjust = 0), 
    plot.subtitle = element_text(size = 12, face = "italic")
  ) +
  labs(
    title = "Number of Disasters by Type in Nordic Countries",
    subtitle = "Storms and extreme temperatures happens most often between 2000 and 2024", 
    x = " ",
    y = "Number of disasters",
    fill = "Disaster Type",
    caption = "R.A.Jacobsen | @AulieRoy | Source: International Disaster Database"
  )
