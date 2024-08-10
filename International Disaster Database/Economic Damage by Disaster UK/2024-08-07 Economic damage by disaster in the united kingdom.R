
# Data from the international disaster database. 
# Link to EMDAT: https://www.emdat.be/
# Economic damage by disasters in the UK.

# Code developed by Roy Aulie Jacobsen. 

# Load libraries. 
library(tidyverse)
library(readxl)

# Load data from the international disaster database 
emdat <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/International Disaster Database/public_emdat.xlsx")

# Clean column names
emdat <- 
  emdat %>% 
  janitor::clean_names()

# Filter country: United Kingdom
uk_data  <- 
  emdat %>% 
  filter(country == "United Kingdom of Great Britain and Northern Ireland")

# Plot economic damage by disaster
uk_data %>%
  group_by(disaster_type) %>%
  summarise(total_damage_adjusted_000_us = sum(total_damage_adjusted_000_us, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(disaster_type, total_damage_adjusted_000_us), y = total_damage_adjusted_000_us)) +
  geom_bar(stat = "identity", fill = "#E3120B") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold"), 
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  labs(
    title = "Economic Damage by Disaster in the United Kingdom",
    subtitle = "Flooding and storms are the most costly natural disasters", 
    x = " ",
    y = "Total Economic Damage (000 USD)",
    caption = "R.A.Jacobsen | @AulieRoy | Source: International Disaster Database"
  )
