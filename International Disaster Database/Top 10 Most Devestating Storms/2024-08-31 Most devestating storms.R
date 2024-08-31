

# Data comes from the International Disaster Database. 
# The data can be downloaded here: https://www.emdat.be/
# Most devestating disasters.
# R.A.Jacobsen | @AulieRoy on Twitter and Bluesky 

# Load libraries  
library(tidyverse)
library(readxl)

# Load dataset from International Disaster Database 
emdata <- read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/International Disaster Database/public_emdat.xlsx")

top_10_storms <- emdata %>%
  filter(!is.na(`Total Damage, Adjusted ('000 US$)`), `Disaster Type` == "Storm", !is.na(`Event Name`)) %>%
  top_n(10, `Total Damage, Adjusted ('000 US$)`) %>%
  arrange(desc(`Total Damage, Adjusted ('000 US$)`))

# Plot
top_10_storms %>% 
  ggplot(aes(x = reorder(`Event Name`, `Total Damage, Adjusted ('000 US$)`),y = `Total Damage, Adjusted ('000 US$)` / 1e6, fill = `Disaster Type`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) + 
  geom_text(aes(
    label = round(`Total Damage, Adjusted ('000 US$)` / 1e6, 1)), 
    hjust = -0.5, 
    color = "black",
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Storm" = "#FFA07A")) +  
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#f0f8ff"), 
    plot.background = element_rect(fill = "#f0f8ff"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, color = "#ff4500"),  
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#ff4500"),
    plot.caption = element_text(size = 8, hjust = 0, colour = "#ff4500"), 
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()   
  ) +
  labs(
    title = "Top 10 Most Devastating Storms",
    subtitle = "Damage in millions USD",
    x = "Disaster Event",
    y = "Total Damage (Million USD)", 
    caption = "R.A.Jacobsen | @AulieRoy | Source: International Disaster Database"
  ) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 250))
