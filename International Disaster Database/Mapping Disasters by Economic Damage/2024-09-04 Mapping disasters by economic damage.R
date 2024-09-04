
# Data comes from the International Disaster Database. 
# The data can be downloaded here: https://www.emdat.be/
# Mapping out total economic damage by country. 
# R.A.Jacobsen | @AulieRoy on Twitter and Bluesky 

# Load libraries  
library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(showtext)

# Load dataset from International Disaster Database 
emdata <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/International Disaster Database/public_emdat.xlsx")

# Economic damage in USD 
economic_damage <-
  emdata %>%
  filter(!is.na(`Total Damage, Adjusted ('000 US$)`)) %>%
  group_by(ISO) %>%
  summarise(Total_Damage = sum(`Total Damage, Adjusted ('000 US$)`, na.rm = TRUE) / 1e6)

# Map 
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(sovereignt != "Antarctica")

world_economic_damage <- 
    world %>%
    left_join(economic_damage, by = c("iso_a3_eh" = "ISO"))

# Mapping out the data 

font_add_google("Roboto", "roboto")
showtext_auto()

colors_plot <- c("#FDE725", "#5DC863", "#21908C", "#3B528B", "#440154")

world_economic_damage %>% 
  ggplot() +
  geom_sf(aes(fill = Total_Damage), color = "white", size = 0.3) +
  scale_fill_gradientn(
    colors = colors_plot,
    trans = "log", na.value = "lightgrey", labels = scales::comma) +
  theme_minimal(base_family = "roboto") +  # Use the custom Roboto font
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "lightblue"),
    panel.grid = element_blank(),
    text = element_text(color = "darkblue", size = 16, face = "bold"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "darkblue"),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "darkblue"),
    plot.caption = element_text(size = 8, hjust = 0), 
    legend.background = element_rect(fill = "lightblue", color = NA),
    legend.title = element_text(size = 14, color = "darkblue"),
    legend.text = element_text(size = 12, color = "darkblue"),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    legend.position = "right"
  ) +
  labs(
    title = "Total Economic Damage",
    subtitle = "Map showing economic damage across countries (in Million USD). Data on total damage not available for all countries",
    caption = "R.A.Jacobsen | @AulieRoy | Source: International Disaster Database", 
    fill = "Total Damage"
  )