library(tidyverse)
library(httr)
library(magick)

api_key <- "xnnrCxWmbNszvi8Z6JKlTwPZtD6OzXtEec0hXTj3javGc2uDeoKghAK3"

url <- "https://api.pexels.com/v1/search?query=scuba%20diving&per_page=80"

response <- httr::GET(url, 
                      add_headers(Authorization = api_key))

data <- httr::content(response, 
                      as = "parsed", 
                      type = "application/json")

photo_data <- tibble(photos = data$photos) %>%
  unnest_wider(photos) %>%
  unnest_wider(src)

nrow(photo_data)

# I create new variables to better understand the size and shape of the scuba diving photos.
selected_photos <- photo_data %>%
  mutate(
    aspect_ratio = width / height,
    orientation = if_else(width > height, "landscape", "portrait"),
    size_group = case_when(
      width >= 4000 ~ "large",
      width >= 2500 ~ "medium",
      TRUE ~ "small"
    )
  ) %>%
  filter(orientation == "landscape") %>%
  slice(1:20)

# I select around 20 landscape photos because many scuba diving photos show wide underwater scenes.

write_csv(selected_photos, "selected_photos.csv")

nrow(selected_photos)

selected_photos

# Calculate the average width of the selected photos
mean_width <- mean(selected_photos$width, na.rm = TRUE)

# Count how many photos are landscape (categorical summary)
count_landscape <- sum(selected_photos$orientation == "landscape")

# Calculate the average aspect ratio
mean_aspect_ratio <- mean(selected_photos$aspect_ratio, na.rm = TRUE)

# Group photos by size_group and calculate average width for each group
grouped_photos <- selected_photos %>%
  group_by(size_group) %>%
  summarise(mean_width = mean(width, na.rm = TRUE))

# Extract one value (required by assignment)
mean_width_large <- grouped_photos$mean_width[1]

# Create an animated GIF where both images and text change across frames

gif_frames <- image_blank(width = 1800, height = 1050, color = "black")

titles <- c(
  "What is Scuba Diving?",
  "Experience Underwater",
  "Discover Marine Life"
)

captions <- c(
  "Scuba diving allows people to breathe underwater using special equipment and explore the ocean safely.",
  "Divers can move freely underwater and experience unique views, light patterns and deep ocean environments.",
  "Scuba diving gives people the chance to observe marine life such as fish, coral reefs and underwater ecosystems."
)

frame_index <- 1

for (i in c(1, 7, 13)) {
  
  photos <- image_read(selected_photos$large[i:(i + 5)]) %>%
    image_resize("600x350!")
  
  row1 <- photos[1:3] %>%
    image_append()
  
  row2 <- photos[4:6] %>%
    image_append()
  
  photo_grid <- c(row1, row2) %>%
    image_append(stack = TRUE)
  
  title <- image_blank(width = 1800, height = 140, color = "black") %>%
    image_annotate(
      titles[frame_index],
      size = 65,
      color = "white",
      gravity = "center"
    )
  
  caption <- image_blank(width = 1800, height = 210, color = "black") %>%
    image_annotate(
      captions[frame_index],
      size = 35,
      color = "white",
      gravity = "center"
    )
  
  frame <- c(title, photo_grid, caption) %>%
    image_append(stack = TRUE) %>%
    image_border(color = "white", geometry = "10x10")
  
  gif_frames <- c(gif_frames, frame)
  
  frame_index <- frame_index + 1
}

gif_frames <- gif_frames[-1]

creativity <- image_animate(gif_frames, fps = 1)

image_write(creativity, "creativity.gif")