# Loading in the packages
library(dplyr)
library(tidyverse)
library(ggplot2)

# Loading in the datasets
titles_df <- read.csv("titles.csv", stringsAsFactors = FALSE)

recent_titles_df <- titles_df %>% 
  group_by(release_year, type) %>%
  filter(release_year >= 2000) %>%
  summarize(num_media = n())

# make a line plot
viz_chart <- recent_titles_df %>%
  filter(type %in% c("MOVIE", "SHOW"))

ggplot(viz_chart) +
  geom_line(aes(x = release_year, y = num_media, color = type)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Trends in Release of Movies and Shows", x = "Release Year", y = "Number of Media Released")