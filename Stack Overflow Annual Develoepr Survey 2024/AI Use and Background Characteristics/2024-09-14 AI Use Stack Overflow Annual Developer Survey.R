

# Linear model examining the use of AI among developers on Stackoverflow. 
# Data comes from the Stack Overflow Annual Developer Survey 2024. 
# All data were loaded from the TidyTuesday challenge in Week 36. 

# Load libraries
library(tidyverse)
library(broom)
library(showtext)

# Font
font_add_google("Roboto", "roboto")
showtext_auto()

# Load data from TidyTuesday
stackoverflow_survey_single_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-03/stackoverflow_survey_single_response.csv')

# Wrangle data 
stackoverflow <- 
  stackoverflow_survey_single_response %>%
  mutate(ai_select_binary = case_when(
    ai_select %in% c(1, 2) ~ 0,  
    ai_select == 3 ~ 1           
  ))

stackoverflow <- 
  stackoverflow %>%
  mutate(age = factor(age, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c('18-24', '25-34', '35-44', '45-54', '55-64', '65+', 'Prefer not to say', 'Under 18')))


stackoverflow <- 
  stackoverflow %>%
  mutate(remote_work = factor(remote_work, levels = c(1, 2, 3),
                              labels = c('Hybrid', 'In-person', 'Remote')))

stackoverflow <- 
  stackoverflow %>%
  mutate(ed_level = factor(ed_level, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                           labels = c('Associate degree', 'Bachelor???s degree', 'Master???s degree',
                                      'Primary school', 'Professional degree', 'Secondary school',
                                      'Some college', 'Something else')))

# Model 
lpm_stackoverflow <- lm(ai_select_binary ~ age + ed_level + years_code_pro + remote_work, data = stackoverflow)

lpm_stackoverflow <- 
  tidy(lpm_stackoverflow, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%  
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",  
    p.value < 0.01 ~ "**",   
    p.value < 0.05 ~ "*",   
    TRUE ~ ""
  ))


# Plot 
lpm_stackoverflow %>% 
  ggplot(aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +  
  geom_text(aes(label = sprintf("%.2f%s", estimate, significance)), 
            hjust = ifelse(tidy_lpm_model$estimate > 0, -0.1, 1.1),  
            size = 4) +  # Increased font size
  scale_fill_manual(values = c("firebrick", "forestgreen")) + 
  coord_flip() +
  labs(title = "Coefficients from a Linear Model Examining Developers' Use of AI", 
       subtitle = "The results from a linear probability model examining developers' background characteristics and the use of AI in their work find a positive and significant effect of doing remote work.\nFurther, differences in age groups reveal that older developers are less likely to use AI in their everyday work, with younger age groups using AI tools to a greater extent. In addition, those \nwith a professional degree are significantly less likely to use AI in their work.",
       x = NULL, 
       y = "Estimate") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"),  
        plot.subtitle = element_text(hjust = 0, size = 14), 
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.y = element_text(size = 12),   
        axis.text.x = element_text(size = 12), 
        text = element_text(family = "roboto")) +  
  scale_y_continuous(limits = c(min(tidy_lpm_model$estimate) - 0.05, max(tidy_lpm_model$estimate) + 0.05)) + 
  theme(panel.grid = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = "black"))
