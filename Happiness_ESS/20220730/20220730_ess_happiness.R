
# Mean level of happiness across European countries

-# Load packages ----
library(ggplot2)
library(tidyverse)
library(haven)
library(countrycode)

-# Load data ----
ess <- read_spss("C:/Users/royja/Downloads/ESS9e03_1.sav/ESS9e03_1.sav")

world <- map_data("world")

-# Wrangle data ----
custom_match <- c("GB" = 'UK')

happiness <- ess %>% 
  select(cntry, happy) %>% 
  group_by(cntry) %>% 
  summarize(mean = mean(happy, na.rm = TRUE)) %>% 
  mutate(region = countrycode(cntry, "iso2c", "country.name", custom_match = custom_match))

europe = c('UK', 'France', 'Germany', 'Italy', 'Spain', 'Ukraine',
           'Poland', 'Romania', 'Netherlands', 'Belgium',
           'Czech Republic', 'Greece', 'Portugal', 'Sweden',
           'Hungary', 'Belarus', 'Austria', 'Serbia', 'Switzerland',
           'Bulgaria', 'Denmark', 'Finland', 'Slovakia', 'Norway', 
           'Ireland', 'Croatia', 'Moldova', 'Bosnia and Herzegovina',
           'Albania', 'Lithuania', 'Macedonia', 'Slovenia', 'Latvia', 
           'Estonia', 'Montenegro', 'Luxembourg', 'Malta', 'Iceland', 
           'Andorra', 'Monaco', 'Liechtenstein', 'San Marino', 
           'Vatican', 'Kosovo')

eur = world[world$region %in% europe, ] 

mapdata <- left_join(eur, happiness, by = "region")

-# Plot ----
  ggplot(mapdata, aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill = mean), color = "black") + 
    scale_fill_gradient(name = "Happiness", low = "yellow", high = "red", na.value = "white") + 
    labs(
      title = "Happiness in Europe",
      subtitle = "Mean level of happiness based on ESS round 9",
      caption = "R.A.Jacobsen | @AulieRoy | Data: European Social Survey"
    ) + 
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          rect = element_blank(), 
          plot.title = element_text(size = 20, hjust = 0, color = "steelblue4", face = "bold"),
          plot.subtitle = element_text(size = 14, color = "firebrick4", hjust = 0),
          plot.caption = element_text(color = "firebrick4", hjust=0),
    )
