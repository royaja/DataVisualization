
# Load library
library(tidyverse)
library(readxl)
library(ggchicklet)
library(cowplot)

# Load data 
facebook <- read_xlsx("C:/Users/roja006/Downloads/facebook_politik.xlsx")

# Wrangle
facebook_first_month_norway <- facebook %>% 
  janitor::clean_names() %>% 
  filter(post_created_date > "2020-03-12" & post_created_date < "2020-04-13") %>% 
  filter(page_admin_top_country == "NO")

facebook_first_month_norway$post_created_date <- as.Date(facebook_first_month_norway$post_created_date, format = "%Y-%m-%d")

# Text 
title <- "Engagement on Facebook posts"
subtitle <- "Total engagement recieved on Facebook posts during the first month of COVID-19 in Norway \nwith actors grouped within three categories: Council, Political party, and Politician"

# Plot 
ggplot(facebook_first_month_norway, aes(x = post_created_date, y = total_interactions, fill = page_category)) +
  geom_chicklet() +
  scale_size(range = c(0, 0.3), guide = FALSE) +
  labs(
    title = title,
    subtitle = subtitle, 
    x = "Date", 
    y = "Total Engagement", 
    fill = "Category"
  ) + 
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C"), labels = c("Council", "Political party", "Politician")) +
  scale_x_date(date_breaks = "2 days", date_minor_breaks = "1 day", date_labels = "%b %d") +
  coord_flip() +
  theme_cowplot()
