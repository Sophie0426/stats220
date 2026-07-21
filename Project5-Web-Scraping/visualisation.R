# Load libraries
library(tidyverse)
library(stringr)
library(ggplot2)

# Load saved data
beehive <- readRDS("beehive.rds")
ministers <- readRDS("ministers.rds")

# Separate minister names
beehive_ministers <- beehive %>%
  separate_rows(ministers, sep = ";") %>%
  mutate(ministers = str_trim(ministers))

# Combine Beehive data with Wikipedia data
combined_data <- beehive_ministers %>%
  left_join(ministers, by = c("ministers" = "minister"))

# Count climate-related words by minister
minister_counts <- combined_data %>%
  mutate(
    text = str_to_lower(paste(title, summary)),
    climate_mentions = str_count(
      text,
      "climate|carbon|energy|emission|emissions|environment|environmental"
    )
  ) %>%
  group_by(ministers) %>%
  summarise(
    total_mentions = sum(climate_mentions),
    number_of_releases = n()
  ) %>%
  arrange(desc(total_mentions)) %>%
  slice_head(n = 10) %>%
  mutate(
    highlight = case_when(
      ministers %in% c("Rt Hon Winston Peters", "Hon Simon Watts") ~ "Key climate-related voices",
      TRUE ~ "Other ministers"
    )
  )

# Create visualisation
my_plot <- minister_counts %>%
  ggplot(aes(
    x = reorder(ministers, total_mentions),
    y = total_mentions,
    fill = highlight
  )) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = total_mentions),
    hjust = -0.1,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Key climate-related voices" = "#2C7FB8",
      "Other ministers" = "#9ECAE1"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Ministers most associated with climate-related topics",
    subtitle = "Climate-related word mentions in 2025 Beehive releases",
    x = "Minister",
    y = "Climate-related mentions",
    fill = "Minister group",
    caption = "Sources: beehive.govt.nz search result pages and Wikipedia minister information"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_blank(),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Save visualisation
ggsave(
  "my_viz.png",
  my_plot,
  width = 11,
  height = 6
)