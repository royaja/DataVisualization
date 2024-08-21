
# Data from the international disaster database. 
# Link to EMDAT: https://www.emdat.be/
# Number of disaster over time in the United States 

# Code developed by Roy Aulie Jacobsen. 


# Load libraries 
library(tidyverse)
library(readxl)
library(showtext)

# Roboto font
font_add_google("Roboto", "roboto")
showtext_auto()

# Load data from the international disaster database 
emdat <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/International Disaster Database/public_emdat.xlsx")

# Clean column names
emdat <- 
  emdat %>% 
  janitor::clean_names()


# Filter country: United States
us_data  <- 
  emdat %>% 
  filter(country == "United States of America")

# Plot 
us_data %>%
  count(start_year) %>%
  ggplot(aes(x = start_year, y = n)) +
  geom_line(color = "black", size = 1) + 
  geom_point(color = "black", size = 3) +  
  geom_text(aes(label = n), vjust = -1, size = 4, color = "black", family = "roboto") +  
  geom_smooth(method = "loess", color = "blue", linetype = "dashed", size = 1, se = FALSE) + 
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    axis.line = element_line(color = "black"),  
    plot.background = element_rect(fill = "white", color = NA),  
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "roboto"),  
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", family = "roboto"),  
    plot.caption = element_text(hjust = 0, size = 12, family = "roboto"),  
    axis.title.x = element_text(size = 13, face = "bold", family = "roboto"), 
    axis.title.y = element_text(size = 13, face = "bold", family = "roboto"),  
    axis.text = element_text(size = 12, family = "roboto")  
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks = seq(0, 40, 5)) +  
  scale_x_continuous(breaks = seq(min(us_data$start_year), max(us_data$start_year), by = 2)) +  
  labs(
    title = "Trends in the Number of Disasters in the United States (2000-2023)",
    subtitle = "Analysis of Disaster Data from the International Disaster Database",
    x = "Year",
    y = "Number of Disasters",
    caption = "R.A.Jacobsen | @AulieRoy | Source: International Disaster Database"
  )
