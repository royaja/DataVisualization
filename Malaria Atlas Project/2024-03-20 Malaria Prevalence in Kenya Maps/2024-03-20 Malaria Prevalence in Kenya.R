
# Data from malariaAtlas project 
# https://malariaatlas.org/
# Code created 20.03.2024 

# Load libraries 
library(tidyverse)
library(malariaAtlas)

# Malaria prevalence in Kenya 
kenya_pr <- malariaAtlas::getPR(ISO = "KEN", species = "both")


# Plotting 

# Bob Ross colors: https://sites.radford.edu/~cdc/seansevillair.html
# Sap Green	#0A3410
# Bright Red	#DB0000

kenya_pr %>%
  filter(!is.na(pr)) %>%
  mutate(decade = 10 * (year_start %/% 10)) %>%
  mutate(decade = floor(year_start / 10) * 10) %>%
  arrange(pr) %>%
  ggplot(aes(longitude, latitude, color = pr)) +
  geom_tile(fill = "lightgrey", color = "white") + 
  geom_point() +
  borders(database = "world", regions = "Kenya") +
  scale_color_gradient2(low = "#0A3410", high = "#DB0000", midpoint = mean(kenya_pr$pr, na.rm = TRUE), labels = scales::percent_format()) +
  facet_wrap(~decade) +
  coord_map() +
  theme_bw() +
  labs(color = "Prevalence",
       title = "Malaria Prevalence Over Time in Kenya", 
       x = " ", 
       y = " ", 
       caption = "R.A. Jacobsen | @AulieRoy | Data from malariaAtlas") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(size = 16, face = "bold")) +
  guides(color = guide_colorbar(barwidth = 10, barheight = 0.5,
                                title.position = "top", 
                                title.hjust = 0.5))
