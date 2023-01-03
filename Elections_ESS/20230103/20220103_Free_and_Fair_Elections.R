
# Data from the European Social Survey Round 10
# https://www.europeansocialsurvey.org/
# The data was retrieved 19.12.2022

# Load libraries
library(tidyverse)
library(haven)
library(hrbrthemes)

# Load data
ess <- read_spss("ess.sav")

# Wrangle data
country_mean <- ess %>%
  group_by(cntry) %>%
  summarise(fairelc_mean = mean(fairelc, na.rm = TRUE))

# Plot
country_mean %>%
  mutate(cntry = fct_reorder(cntry, fairelc_mean)) %>%
  ggplot(aes(fairelc_mean, cntry)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  labs(title= "Free and fair elections",
       subtitle= "The public's attitudes across Europe",
       caption = "R.A.Jacobsen | @AulieRoy | Source: European Social Survey") +
  xlab("Mean") +
  ylab("Country code") +
  theme_ipsum()
