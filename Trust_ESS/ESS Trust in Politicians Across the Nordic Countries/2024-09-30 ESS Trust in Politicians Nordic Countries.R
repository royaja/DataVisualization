
# Data comes from the last five rounds of the European Social Survey.  
# Analysis was performed on September 30th, 2024. 
# All code is present within the R syntax file. 

# Load libraries
library(tidyverse)
library(haven)  
library(showtext)
library(viridis)

# Load data 
ess_data <- read_spss("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[29] Coding and Data/[03] Data/European Social Survey/ESS data/ESS data.sav")

# Wrangle data 
nordic_countries = c("DK", "IS", "FI", "SE", "NO")

ess_data <- 
  ess_data %>% 
  mutate(country = cntry) %>% 
  filter(country %in% nordic_countries)

ess_trust_politicians <- 
  ess_data %>% 
  mutate(year = 2002 + (essround - 1) * 2) %>%  
  filter(year >= max(year) - 8) %>% 
  group_by(year, country) %>%
  summarize(avg_trust_politicians = mean(trstplt, na.rm = TRUE)) %>%
  ungroup()

# Plot 
font_add_google("Lora", "lora")
showtext_auto()

ess_trust_politicians %>% 
  ggplot(aes(x = year, y = avg_trust_politicians, color = country, group = country)) +
  geom_line(size = 2) +  
  geom_point(size = 5, shape = 16) + 
  geom_text(aes(label = round(avg_trust_politicians, 1)), vjust = -0.8, size = 4, color = "black") + 
  labs(
    title = "Average Trust in Politicians in Nordic Countries (Last 5 Rounds)",
    subtitle = "Norway has consistently higher levels of trust in politicians than other Nordic countries, but levels of trust are almost on par with Finland for the last round of the European Social Survey. \nThe lowest levels of trust are found in Iceland, where the ESS was performed in 2018 and 2020.",
    caption = "R.A.Jacobsen | @AulieRoy | Source: European Social Survey",
    x = "Year",
    y = "Average Trust in Politicians",
    color = "Country"
  ) +
  scale_color_viridis_d(option = "C", begin = 0.1, end = 0.9) +  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits = c(4.4, 6.1)) 
