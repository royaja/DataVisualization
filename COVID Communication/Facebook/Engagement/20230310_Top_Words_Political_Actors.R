
# Top 15 words used by political actors in Norway during the first month of the pandemic 
# The code focus on the posts with highest engagement 
# 2023-03-10

# Load library 
library(tidyverse)
library(readxl)
library(tidytext)
library(ggchicklet)

# Load data 
facebook <- read_xlsx("facebook_politik.xlsx")

# Wrangle data 
facebook_first_month <- facebook %>% 
  janitor::clean_names() %>% 
  filter(post_created_date > "2020-03-12" & post_created_date < "2020-04-13") %>% 
  filter(page_admin_top_country == "NO")

post_high_interaction <- facebook_first_month %>% 
  arrange(desc(total_interactions)) %>% 
  head(20)

facebook_top_interactions_words <- post_high_interaction %>%
  unnest_tokens(word, message) %>% 
  anti_join(get_stopwords(language = "norwegian", source = "snowball")) %>% 
  filter(str_length(word) >= 4) %>%  
  mutate(word = str_remove_all(word, "\\d+")) %>% 
  filter(nchar(word) > 0) %>% 
  select(word)

word_freq <- facebook_top_interactions_words %>%
  count(word, sort = TRUE)

top_words <- word_freq %>%
  head(15)

# Plot 
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_chicklet(fill = "#D2B48C", height = 0.4, width = 0.5, stroke = "black") +
  labs(title = "Top 15 Most Frequent Words",
       subtitle = "Top words used by Norwegian politicians on Facebook drawn from posts with the highest engagement during the first month of the COVID-19 pandemic",
       caption = "R.A.Jacobsen | @AulieRoy | Data from CrowdTangle",
       x = "Word",
       y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 1)) + 
  geom_hline(yintercept = seq(0, 12, 2), color = "gray80") +
  geom_text(aes(label = n), hjust = -0.1) 
