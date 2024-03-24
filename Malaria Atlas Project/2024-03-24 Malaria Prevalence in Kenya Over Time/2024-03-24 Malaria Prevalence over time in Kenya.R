
# Data from malariaAtlas project 
# https://malariaatlas.org/
# Code created 20.03.2024 

# Load libraries 
library(tidyverse)
library(malariaAtlas)
library(scales)

# Looking into malaria incidents in Kenya 
kenya_pr <- malariaAtlas::getPR(ISO = "KEN", species = "both")

kenya_pr <- kenya_pr %>% 
  group_by(year_start) %>% 
  summarise(examined = sum(examined),
            positive = sum(positive), 
            studies = n()) %>%
  mutate(pr = positive / examined)

ggplot(data = kenya_pr, aes(x = year_start, y = pr)) + 
  geom_line() + 
  geom_point(aes(size = studies)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Malaria Prevalence Over Time in Kenya",
       x = "Year",
       y = "Prevalence Rate")

# Plotting malaria incidents over time in Kenya 
ggplot(data = kenya_pr, aes(x = year_start, y = pr)) + 
  geom_line(color = "#e5121a", size = 1.5) + 
  geom_point(aes(size = studies), color = "#e5121a", size = 5) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Malaria Prevalence Over Time in Kenya",
       x = "Year",
       y = "Prevalence Rate") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#000000"),
    axis.title = element_text(color = "#000000", face = "bold"),
    plot.title = element_text(color = "#e5121a", face = "bold")
  ) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 5))