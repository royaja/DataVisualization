
# Raw count of tweets from former Norwegian cultural minister Abid Raja
# Created: 06 October 2022
# Edited: 06 October 2022
# Data: Gathered via Twitter API

# Load packages

library(tidyverse)
library(lubridate)
library(ggtext)


# Load data

raja <- read_csv2("C:/Users/royja/Desktop/tweeter_data/source/raja.csv")


# Wrangle and plot data 

plot <- raja %>% 
  mutate(date = as.Date(ymd_hms(created_at))) %>% 
  count(date) %>% 
  filter(date > "2020-02-01" & date < "2021-10-31") %>% 
  ggplot(aes(x = date, 
             y = n)) + 
  geom_line(aes(colour = screen_name), 
            color = "white", 
            alpha = 0.7, size = 1.5) +
  labs(
    y = "Count",
    x = "Date",
    title = "Raw count of tweets from Abid Raja",
    subtitle = "Number of tweets published by former Norwegian cultural minister Abid Raja between <br>
    February 2020 and October 2021",
    caption = "R.A.Jacobsen | @AulieRoy | Source: Twitter"
  ) + 
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "lightblue", colour = "lightblue"),
    panel.grid = element_blank(),
    plot.title = element_markdown(family = "DM Sans", colour = "black", size = 24,
                                  hjust = 0, margin = unit(c(1.5, 0, 0, 0), "cm")),
    plot.subtitle = element_markdown(family = "DM Sans", colour = "black", size = 16, 
                                     hjust = 0, lineheight = 1.35, margin = unit(c(1.5, 0, 1, 0), "cm")),
    plot.caption = element_markdown(family = "DM Sans", colour = "black", size = 12, 
                                    hjust = 0, margin = unit(c(1.5, 0, 0, 0), "cm")),
    axis.line.x = element_line(colour = "#C5C2CC"),
    axis.text = element_markdown(family = "DM Sans", colour = "#59516c", size = 10)
  )

ggsave(plot = plot, filename = file.path("20221006_raw_count_abid_raja_tweets.png"), 
       dpi = 400, width = 11, height = 12, bg = "lightblue")


