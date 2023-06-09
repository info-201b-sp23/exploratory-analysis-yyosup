library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("scales")

# Read the csv files
credits_data <- read.csv("credits.csv", stringsAsFactors = FALSE)
titles_data <- read.csv("titles.csv", stringsAsFactors = FALSE)

# Join the two tables together 
credits_title_data <- inner_join(titles_data, credits_data, by = "id")

# Delete the columns that are not necessary 
credits_title_data <- select(credits_title_data, -description)
credits_title_data <- select(credits_title_data, -runtime)
credits_title_data <- select(credits_title_data, -genres)
credits_title_data <- select(credits_title_data, -production_countries)
credits_title_data <- select(credits_title_data, -tmdb_popularity)
credits_title_data <- select(credits_title_data, -tmdb_score)

# Create a list to store summary information
summary_info <- list()

# Calculate the movies with the highest IMDb for each year 
highest_IMDb_year <- credits_title_data %>%
  group_by(release_year) %>%
  filter(imdb_score == max(imdb_score, na.rm = TRUE)) %>%
  select(release_year, title, imdb_score) %>%
  distinct()

summary_info$num_observations <- nrow(credits_title_data)
summary_info$highest_IMDb_year <- highest_IMDb_year

# Calculate the movie with the highest IMDb score 
highest_IMDb_alltime <- credits_title_data %>%
  filter(imdb_score == max(imdb_score, na.rm = TRUE)) %>%
  select(release_year, title, imdb_score) %>%
  distinct()

summary_info$highest_IMDb_alltime <- highest_IMDb_alltime

# Find the actors who acted in the highest rated IMDb movies 
max_imdb_score <- max(credits_title_data$imdb_score, na.rm = TRUE)

highest_imdb_movies <- credits_title_data %>%
  filter(imdb_score == max_imdb_score)

actors <- unique(highest_imdb_movies$name)

summary_info$actors_highest_IMDb <- actors

# Calculate age certification/restriction and IMDb score of movies
age_imdb_data <- credits_title_data %>%
  group_by(title, age_certification) %>%
  summarize(imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(desc(imdb_score))

summary_info$age_imdb_data <- age_imdb_data

# Calculate the year with the highest average IMDb score
year_avg_imdb <- credits_title_data %>%
  group_by(release_year) %>%
  summarize(avg_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  filter(avg_imdb_score == max(avg_imdb_score))

summary_info$year_highest_avg_IMDb <- year_avg_imdb$release_year

# Calculate the movie with the most IMDb votes 
most_votes_movie <- credits_title_data %>%
  filter(imdb_votes == max(imdb_votes, na.rm = TRUE)) %>%
  distinct(title)

summary_info$most_votes_movie <- most_votes_movie

# Print the summary information
print(summary_info)

