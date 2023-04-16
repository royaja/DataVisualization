
# Data from Facebook extracted using CrowdTangle
# Limited to Norwegian political actors during the first two months of the COVID-19 pandemic
# Code by: R.A.Jacobsen | @AulieRoy

# Load libraries  
library(tidyverse)
library(readxl)

# Load data 
facebook <- read_xlsx("facebook_politik.xlsx")

# Wrangle data 
facebook <- 
  facebook %>% 
  janitor::clean_names()

facebook_covid_norway <- 
  facebook %>% 
  filter(page_admin_top_country == "NO") %>% 
  filter(post_created_date > "2020-03-12" & post_created_date < "2020-08-13") %>% 
  group_by(post_created_date) %>% 
  summarize(n = n())

# Plot text 
title <- "Political actors posted less in July and August 2020"
subtitle <- "Raw count of daily Facebook posts during the first 6 months of the COVID-19 pandemic in Norway"
caption <- "R.A.Jacobsen | @AulieRoy | Source: CrowdTangle"

# Plot 
facebook_covid_norway %>% 
  ggplot(aes(x = post_created_date, y = n)) + 
  geom_line(color = "#E3120B", size = 1) + 
  scale_y_continuous(sec.axis = dup_axis()) + 
  labs(
    x = "", 
    y = "", 
    title = title, 
    subtitle = subtitle, 
    caption = caption
  ) + 
  theme(axis.line.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(), 
        axis.ticks.x = element_line(), 
        panel.grid.major.y = element_line(color = "#D9D9D9"), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5"), 
        plot.title = element_text(family = "Milo TE", size = 16, face = "bold", hjust = 0, vjust = 0), 
        plot.subtitle = element_text(family = "Milo TE", size = 10, hjust = 0, vjust = 0), 
        plot.caption = element_text(family = "Milo TE", size = 8, hjust = 0, vjust = 0))
