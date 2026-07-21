library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(magick)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQoMXO1WqygkgxtGEPEfJvQ27P0SWWvSZjWvcqI1a7lMKNe1lwgmFpkcEuJFb3tE-buIRDUohX0_khI/pub?output=csv")

names(logged_data)

glimpse(logged_data)

transport_colours <- c(
  "Walking" = "#8dd3c7",
  "Car" = "#bebada",
  "Bicycle" = "#fb8072",
  "Scooter" = "#80b1d3",
  "Bus" = "#fdb462"
)

group_colours <- c(
  "Alone" = "#ffed6f",
  "Pair" = "#fccde5",
  "Group of 3 or more" = "#b3de69"
)

transport_summary <- logged_data %>%
  filter(!is.na(`Transport mode`)) %>%
  group_by(`Transport mode`) %>%
  summarise(
    total = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total))

plot1 <- transport_summary %>%
  ggplot(aes(x = reorder(`Transport mode`, total),
             y = total,
             fill = `Transport mode`)) +
  geom_col() +
  scale_fill_manual(values = transport_colours) +
  labs(
    title = "Most Common Transport Modes",
    subtitle = "Community transport observations",
    x = "Transport mode",
    y = "Number of observations",
    caption = "Data source: Community Transport Observation Form"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "none"
  )

plot1

ggsave(
  "plot1.png",
  plot = plot1,
  width = 8,
  height = 5
)

clean_data <- logged_data %>%
  rename(date = `Column 1`) %>%
  filter(!is.na(`Transport mode`)) %>%
  mutate(
    date = dmy(date),
    day_of_week = wday(date, label = TRUE, week_start = 1),
    `Time period` = factor(
      `Time period`,
      levels = c("Morning", "Midday", "Afternoon", "Evening")
    ),
    `Travelling group` = factor(
      `Travelling group`,
      levels = c("Alone", "Pair", "Group of 3 or more")
    ),
    walking_status = if_else(str_detect(`Transport mode`, "Walking"),
                             "Walking",
                             "Other transport")
  )

time_transport_summary <- clean_data %>%
  group_by(`Time period`, `Transport mode`) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )

plot2 <- time_transport_summary %>%
  ggplot(aes(x = `Time period`,
             y = total,
             fill = `Transport mode`)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = transport_colours) +
  labs(
    title = "Transport Modes Across Different Time Periods",
    subtitle = "Comparing community transport patterns during the day",
    x = "Time period",
    y = "Number of observations",
    fill = "Transport mode",
    caption = "Data source: Community Transport Observation Form"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18)
  )

plot2

ggsave(
  "plot2.png",
  plot = plot2,
  width = 8,
  height = 5
)

group_summary <- clean_data %>%
  group_by(`Time period`, `Travelling group`) %>%
  summarise(
    total = n(),
    .groups = "drop"
  )

plot3 <- group_summary %>%
  ggplot(aes(x = `Time period`,
             y = total,
             colour = `Travelling group`,
             group = `Travelling group`)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = group_colours) +
  labs(
    title = "How Do People Travel at Different Times?",
    subtitle = "Comparing travelling group patterns across time periods",
    x = "Time period",
    y = "Number of observations",
    colour = "Travelling group",
    caption = "Data source: Community Transport Observation Form"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18)
  )

plot3

ggsave(
  "plot3.png",
  plot = plot3,
  width = 8,
  height = 5
)

morning_plot <- clean_data %>%
  filter(`Time period` == "Morning") %>%
  ggplot(aes(x = `Transport mode`,
             fill = `Transport mode`)) +
  geom_bar() +
  scale_fill_manual(values = transport_colours) +
  labs(
    title = "Morning Transport Patterns",
    x = "Transport mode",
    y = "Number of observations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("morning.png", morning_plot, width = 8, height = 5)

midday_plot <- clean_data %>%
  filter(`Time period` == "Midday") %>%
  ggplot(aes(x = `Transport mode`,
             fill = `Transport mode`)) +
  geom_bar() +
  scale_fill_manual(values = transport_colours) +
  labs(
    title = "Midday Transport Patterns",
    x = "Transport mode",
    y = "Number of observations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("midday.png", midday_plot, width = 8, height = 5)

afternoon_plot <- clean_data %>%
  filter(`Time period` == "Afternoon") %>%
  ggplot(aes(x = `Transport mode`,
             fill = `Transport mode`)) +
  geom_bar() +
  scale_fill_manual(values = transport_colours) +
  labs(
    title = "Afternoon Transport Patterns",
    x = "Transport mode",
    y = "Number of observations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("afternoon.png", afternoon_plot, width = 8, height = 5)

evening_plot <- clean_data %>%
  filter(`Time period` == "Evening") %>%
  ggplot(aes(x = `Transport mode`,
             fill = `Transport mode`)) +
  geom_bar() +
  scale_fill_manual(values = transport_colours) +
  labs(
    title = "Evening Transport Patterns",
    x = "Transport mode",
    y = "Number of observations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("evening.png", evening_plot, width = 8, height = 5)

transport_animation <- image_read(c(
  "morning.png",
  "midday.png",
  "afternoon.png",
  "evening.png"
))

transport_animation <- image_animate(
  transport_animation,
  fps = 0.5
)

transport_animation

image_write(
  transport_animation,
  "transport_animation.gif"
)