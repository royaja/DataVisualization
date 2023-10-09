
# Plotting a chloropleth map of Europe showing trust in the countries parliament. 
# Data from the European Social Survey Round 9

# Load libraries
library(tidyverse)
library(haven)
library(sf)
library(countrycode)
library(rnaturalearth)

# Load and process data
ess <- read_spss("C:/Users/roja006/OneDrive - Kristiania/[80-89] Coding and Data/[81] Data/[81.06] European Social Survey/ESS-Data-Wizard-subset-2023-10-07 Media & Politics SPSS/ESS-Data-Wizard-subset-2023-10-07.sav")

# Process
ess_round <- ess %>%
  filter(essround == 9) %>%
  group_by(cntry) %>%
  summarise(mean_trust = mean(trstprl, na.rm = TRUE)) %>%
  rename(iso_a2 = cntry)

world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world[world$region_un == "Europe", ]

ess_europe_trust <- europe %>%
  left_join(ess_round, by = c("iso_a2" = "iso_a2"))

# Chloropleth map
ggplot(ess_europe_trust) +
  geom_sf(aes(fill = mean_trust)) +
  scale_fill_viridis_c(name = "Scale of trust") +
  labs(
    title = "Mean Trust in Countries' Parliament",
    caption = "R.A. Jacobsen | @AulieRoy | Source: European Social Survey Round 9"
  ) + 
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank(),
    plot.title = element_text(size = 16, hjust = 0, color = "black", face = "bold"),
    plot.caption = element_text(size = 8, color = "black", hjust = 0)
  ) +
  coord_sf(xlim = c(-28, 28), ylim = c(36, 73))
