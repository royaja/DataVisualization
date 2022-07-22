
-# Load packages ----
library(tidyverse)
library(haven)
library(countrycode)
library(gganimate)

-# Load data ----
ess <- read_spss("C:/Users/royja/Downloads/ess_all_rounds/ess_all_rounds.sav")

world <- map_data("world")

-# Wrangle data ----

custom_match <- c("GB" = 'UK')

trust <- ess %>%
  select(cntry, trstplt, essround) %>%
  mutate(region = countrycode(cntry, "iso2c", "country.name", custom_match = custom_match)) %>%
  group_by(essround, region) %>%
  summarise(mean = mean(trstplt, na.rm = TRUE)) %>%
  mutate(year = recode(essround,
                       `1` = 2002,
                       `2` = 2004,
                       `3` = 2006,
                       `4` = 2008,
                       `5` = 2010,
                       `6` = 2012,
                       `7` = 2014,
                       `8` = 2016,
                       `9` = 2018,
                       `10` = 2020))

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

mapdata <- left_join(eur, trust, by = "region")

-# Plot ----

mapdata %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = mean), color = "black") + 
  scale_fill_gradient(name = "Trust", low = "yellow", high = "red", na.value = "white") + 
  labs(title = "Mean Trust in Politicians across Europe",
       subtitle = "Year: {current_frame}", 
       caption = "R.A.Jacobsen | @AulieRoy | Source: European Social Survey"
  ) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        rect = element_blank(), 
        plot.title = element_text(size = 20, hjust = 0, color = "steelblue4", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "firebrick4", hjust = 0),
        plot.caption = element_text(size = 10, color = "firebrick4", hjust=0),
  ) +
  transition_manual(year)

anim_save("ess_trust_choropleth_animated.gif")
