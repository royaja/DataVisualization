
# Load packages 
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# Load data
instagram <- read_xlsx("C:/Users/roja006/Downloads/instagram_politik.xlsx")

# Wrangle data
instagram <- 
  instagram %>% 
  janitor::clean_names()

# Create a country variable 
instagram <- instagram %>% 
  mutate(country = if_else(str_detect(file_name, "NO"), "NO", 
                           if_else(str_detect(file_name, "DK"), "DK", 
                                   if_else(str_detect(file_name, "SE"), "SE", NA_character_))))

# Filter dates
instagram <- 
  instagram %>% 
  filter(post_created_date > "2020-03-12" & post_created_date < "2020-06-13")

# Raw count 
instagram <- 
  instagram %>%
  filter(!is.na(country)) %>% 
  group_by(post_created_date, country) %>% 
  summarise(across(everything()), n = n())

# Save data 
write_csv2(instagram, file = "initial_month_pandemic_instagram.csv")

# Plot colors 
custom_colors <- c("#E3120B", "#177b57", "#3366FF")

# post_created_date to date format 
raw_count$post_created_date <- as.Date(raw_count$post_created_date, format = "%Y-%m-%d")

# Plot 
raw_count %>% 
  ggplot(aes(x = post_created_date, y = n, color = country)) + 
  geom_line(size = 1) +
  scale_color_manual(values = custom_colors) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") + 
  labs(
    x = "", 
    y = "Messages per day", 
    color = "Country"
  ) + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(), 
        axis.line.x = element_line(), 
        axis.ticks.x = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_line(color = "#D9D9D9"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5"), 
        plot.title = element_text(family = "Milo TE", size = 16, face = "bold", hjust = 0.5, vjust = 0), 
        plot.subtitle = element_text(family = "Milo TE", size = 10, hjust = 0, vjust = 0), 
        plot.caption = element_text(family = "Milo TE", size = 8, hjust = 0, vjust = 0), 
        legend.position = "top", 
        legend.justification = "center",
        legend.title.align = 0.5, 
        legend.background = element_rect(fill = "#F5F5F5")
  )
