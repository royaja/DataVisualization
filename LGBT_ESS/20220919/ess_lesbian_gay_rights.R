# Exploring support for gay and lesbian rights using  European Social Survey data from round 9 in Norway 

-# Load packages ---- 
library(tidyverse)
library(haven)
library(bbplot)
library(HH)

-# Load data ---- 

ess <- haven::read_spss("C:/Users/royja/Downloads/ess_politics/ess_politics.sav")

-# Wrangle data ---- 

ess_df <- ess %>% 
  dplyr::select(freehms, hmsfmlsh, hmsacld) %>%
  dplyr::mutate(across(everything()), haven::as_factor(.)) %>% 
  dplyr::mutate(across(where(is.factor), ~na_if(., "Refused"))) %>% 
  tidyr::pivot_longer(everything(), names_to = "survey_question", values_to = "response") %>%
  filter(!is.na(response))

ess_df <- ess_df %>% 
  group_by(survey_question, response) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  mutate(frequency = round(freq * 100), 0) %>% 
  ungroup() 


-# Color palette for graph -----

color_palette <- c("Agree strongly" = "#B0A1BA",
                   "Agree" = "#A5B5BF",
                   "Neither agree nor disagree" = "#ABC8C7",
                   "Disagree" = "#B8E2C8", 
                   "Disagree strongly" = "#BFF0D4")
-# Plot ---- 

ggplot(data = ess_df, aes(x = survey_question, 
                          fill = response)) +
  geom_bar(data = subset(ess_df, response %in% c("Agree strongly",
                                                 "Agree")), 
           aes(y = -frequency), position="stack", stat="identity") +
  geom_bar(data = subset(ess_df, !response %in% c("Agree strongly",
                                                  "Agree")),
           aes(y = frequency), position="stack", stat="identity") +
  coord_flip() + 
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Attitudes toward gay and lesbian rights", 
    subtitle = "Evidence from Norway using the European Social Survey round 9") + 
  bbplot::bbc_style() + 
  scale_x_discrete(labels = c(
    "Ashamed if close family member gay or lesbian",
    "Gay and lesbian couples right to adopt children",
    "Gays and lesbians free to live life as they wish"
  )) + 
  theme(
    axis.text.x = element_blank()
  )

ggsave("ess_lesbian_gay_rights.png")