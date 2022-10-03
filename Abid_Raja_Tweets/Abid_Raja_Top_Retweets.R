
# 03.10.2022
# Top retweets from former cultural minister Abid Raja

# Load packages ---- 

library(tidyverse)
library(lubridate)
library(cowplot)

# Load data ---- 

raja <- read_csv2("C:/Users/royja/Desktop/tweeter_data/source/raja.csv")

# Wrange data ---- 

top_tweets <- raja %>% 
  mutate(date = as.Date(ymd_hms(created_at))) %>% 
  filter(date > "2020-02-01" & date < "2021-10-14") %>% 
  arrange(desc(retweet_count)) %>% 
  head(4)

# Plot ---- 

top_tweets %>% 
  arrange(desc(retweet_count)) %>% 
  ggplot(aes(x = reorder(text, retweet_count), y = retweet_count)) + 
  geom_segment(aes(xend = text, yend = 0)) + 
  geom_point(size = 4, color = "orange") +
  geom_text(aes(x = text, y = retweet_count,label = retweet_count),vjust = -1) +
  coord_flip() +
  labs(
    title = "Top retweets by Abid Raja between February 2020 and October 2021"
  ) + 
  xlab("Tweet being shared") + 
  ylab("Number of retweets") +
  theme_cowplot()

ggsave("abid_raja_top_retweets.png")


