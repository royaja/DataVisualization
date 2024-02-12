
# Barplot showing content type from the 100 last messages posted by Norwegian politicians 
# before the local election September 11th 2023. 
# The content categories were retrived using LLM GPT 3.5 Turbo to classify content. 

# Load packages 
library(tidyverse)
library(readxl)

# Load dataset 
facebook <- read_xlsx("file path")

# Prepare data
df <- 
  facebook |> 
  select(critic, acknowledgement, personal, mobilization, information) |> 
  pivot_longer(cols = everything(), names_to = "category", values_to = "value") |> 
  mutate(value = replace(value, is.na(value), 0), 
         category = str_to_title(category), 
         value = as.integer(value))

df <- 
  df |> 
  group_by(category) |> 
  mutate(total_value = sum(value))

# Plot
plot <- ggplot(df, aes(x = reorder(category, desc(value)), y = value)) + 
  geom_bar(stat = "identity", fill = "#336699") + 
  geom_text(aes(label = total_value, y = total_value), vjust = -0.5, size = 3, color = "black") + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) + 
  theme_bw() + 
  theme(
    plot.caption = element_text(size = 8, color = "black", hjust = 0), 
    plot.title = element_text(size = 16, hjust = 0, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  ) + 
  labs(
    title = "Content Analysis of the Last 100 Posts Before the Norwegian Election in 2023", 
    subtitle = "Content Classification Performed Using LLM GPT 3.5 Turbo",  
    x = "Type of Messages",
    y = "Number of Posts",
    caption = "R.A. Jacobsen | @AulieRoy | Data: Facebook"
  )

# Save ggplot
ggsave("2024-02-12 Plot Content Categories Facebook Elections GPT Classification.png", plot, width = 14, height = 8, units = "in")
