# Load libraries
library(tidyverse)
library(jsonlite)
library(rvest)

# Create a vector of HTML file names
html_files <- list.files(
  path = "html",
  pattern = ".html",
  full.names = TRUE
)

# Load the Beehive scraping function
source("scrape_html.R")

# Create Beehive dataset from saved HTML files
beehive <- map_df(
  html_files,
  scrape_search_results
)

# Check and remove duplicate rows in Beehive data
beehive <- beehive %>%
  distinct()

# Save Beehive dataset locally
saveRDS(beehive, "beehive.rds")

# Create a vector of minister names
minister_names <- beehive %>%
  pull(ministers) %>%
  str_split(";") %>%
  unlist() %>%
  str_trim() %>%
  unique()

# Load the Wikipedia API function
source("get_wikipedia_infobox.R")

# Create ministers dataset from Wikipedia
ministers <- map_df(
  minister_names,
  possibly(get_wikipedia_infobox, tibble())
)

# Check and remove duplicate rows in ministers data
ministers <- ministers %>%
  distinct()

# Save ministers dataset locally
saveRDS(ministers, "ministers.rds")

# Save datasets as CSV files
write_csv(beehive, "beehive.csv")
write_csv(ministers, "ministers.csv")