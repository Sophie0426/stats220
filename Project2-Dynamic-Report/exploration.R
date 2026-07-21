library(tidyverse)
logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQoMXO1WqygkgxtGEPEfJvQ27P0SWWvSZjWvcqI1a7lMKNe1lwgmFpkcEuJFb3tE-buIRDUohX0_khI/pub?output=csv")
glimpse(logged_data)
logged_data

latest_data <- logged_data |>
  rename(
    time_period = `Time period`,
    transport_mode = `Transport mode`,
    travelling_group = `Travelling group`,
    direction = `Direction of movement`
  )
names(latest_data)
glimpse(latest_data)

# Summary values for each variable
latest_data |> count(time_period)
latest_data |> count(transport_mode)
latest_data |> count(travelling_group)
latest_data |> count(direction)

# Summary values for relationships between variables
latest_data |> count(time_period, transport_mode)
latest_data |> count(transport_mode, travelling_group)
latest_data |> count(time_period, direction)

# Most common transport mode in each time period
latest_data |>
  count(time_period, transport_mode) |>
  group_by(time_period) |>
  slice_max(n, n = 1)

# Proportion of transport modes in each time period
latest_data |>
  count(time_period, transport_mode) |>
  group_by(time_period) |>
  mutate(proportion = n / sum(n))

# Save summary tables
time_summary <- latest_data |> count(time_period)
transport_summary <- latest_data |> count(transport_mode)
group_summary <- latest_data |> count(travelling_group)
direction_summary <- latest_data |> count(direction)

# Print summary tables
time_summary
transport_summary
group_summary
direction_summary

# Bar chart 1: Transport mode distribution
latest_data |>
  ggplot(aes(x = transport_mode)) +
  geom_bar() +
  labs(
    title = "Transport mode distribution",
    x = "Transport mode",
    y = "Count"
  )

# Bar chart 2: Transport mode by time period
latest_data |>
  ggplot(aes(x = transport_mode, fill = time_period)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Transport mode by time period",
    x = "Transport mode",
    y = "Count",
    fill = "Time period"
  )

# Bar chart 3: Transport mode by travelling group
latest_data |>
  ggplot(aes(x = transport_mode, fill = travelling_group)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Transport mode by travelling group",
    x = "Transport mode",
    y = "Count",
    fill = "Travelling group"
  )







