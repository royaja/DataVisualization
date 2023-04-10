
# Percentage reported to have posted political content online in 2022 (ESS Round 10)
# Data from the European Social Survey 
# Theme similar to The Economist 

# Load pacakges 
library(tidyverse)
library(haven)
library(countrycode)
library(scales)

# Data downloaded from ESS website 
# Load data 
ess <- read_spss("C:/Users/roja006/OneDrive - Kristiania/02_Data/ESS-Data-Wizard-subset-2023-04-05.sav")

# Wrangle data: Keep only pstplonl "Yes" and "No" for posting content online 
# Change ISO country code to country name 
ess <- 
  ess %>% 
  filter(pstplonl %in% c(1,2) & !is.na(pstplonl)) %>% 
  mutate(country = countrycode(cntry, "iso2c", "country.name"))

# Calculate proportion of individual that posted content online 
politics_online <- 
  ess %>% 
  filter(essround == "10") %>% 
  group_by(country) %>%
  summarize(prop_posted_politics = mean(pstplonl == 1))

# Plot text 
title <- "Norway Leads in Sharing Political Content Online"
subtitle <- "Insights into online political engagement using the 10th Round of the European Social Survey"
caption <- "R.A.Jacobsen | @AulieRoy | Source: European Social Survey"


# Plot 
politics_online %>%
  ggplot(aes(x = reorder(country, prop_posted_politics), y = prop_posted_politics)) +
  geom_bar(stat = "identity", fill = ifelse(politics_online$country == "Norway", "#E3120B", "#F4A4A2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(sec.axis = dup_axis(labels = scales::percent)) +
  coord_flip() +
  labs(
    x = "", 
    y = "", 
    title = title, 
    subtitle = subtitle, 
    caption = caption
  ) + 
  theme(axis.line.x.bottom =element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_line(color = "#D9D9D9"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5"), 
        plot.title = element_text(family = "Milo TE", size = 16, face = "bold", hjust = -0.17, vjust = -0.17), 
        plot.subtitle = element_text(family = "Milo TE", size = 10, hjust = -0.18, vjust = -0.18), 
        plot.caption = element_text(family = "Milo TE", size = 8, hjust = -0.12, vjust = -0.12))
