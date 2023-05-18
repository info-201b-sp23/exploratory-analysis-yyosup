library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("scales")

#Read the csv files
credits_data <- read.csv("C:/Users/yosup/OneDrive/Desktop/info201/credits.csv", stringsAsFactors = FALSE)
titles_data <- read.csv("C:/Users/yosup/OneDrive/Desktop/info201/titles.csv", stringsAsFactors = FALSE)

#Join the two tables together 
credits_title_data <- inner_join(titles_data, credits_data, by = "id")

#Delete the columns that are not necessary 
credits_title_data <- select(credits_title_data, -description)
credits_title_data <- select(credits_title_data, -runtime)
credits_title_data <- select(credits_title_data, -genres)
credits_title_data <- select(credits_title_data, -production_countries)
credits_title_data <- select(credits_title_data, -tmdb_popularity)
credits_title_data <- select(credits_title_data, -tmdb_score)



#Calculate the movies with the highest IMDb for each year 
highest_IMDb_year <- credits_title_data %>%
  group_by(release_year) %>%
  filter(imdb_score == max(imdb_score, na.rm = TRUE)) %>%
  select(release_year, title, imdb_score) %>%
  distinct()

print(highest_IMDb_year)

#Calculate the movie with the highest IMDb score 
highest_IMDb_alltime <- credits_title_data %>%
  filter(imdb_score == max(imdb_score, na.rm = TRUE)) %>%
  select(release_year, title, imdb_score) %>%
  distinct()
  
print(highest_IMDb_alltime)

#Find the actors who acted in the highest rated IMDb movies 
max_imdb_score <- max(credits_title_data$imdb_score, na.rm = TRUE)

highest_imdb_movies <- credits_title_data %>%
  filter(imdb_score == max_imdb_score)

actors <- unique(highest_imdb_movies$name)

#Calculate age certification/restriction and IMDb score of movies
age_imdb_data <- credits_title_data %>%
  group_by(title, age_certification) %>%
  summarize(imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(desc(imdb_score))

print(age_imdb_data)

#Calculate the year with the highest average IMDb score
year_avg_imdb <- credits_title_data %>%
  group_by(release_year) %>%
  summarize(avg_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  filter(avg_imdb_score == max(avg_imdb_score))

print(year_avg_imdb$release_year)

#Calculate the movie with the most IMDb votes 
most_votes_movie <- credits_title_data %>%
  filter(imdb_votes == max(imdb_votes, na.rm = TRUE)) %>%
  distinct(title)

print(most_votes_movie)

# Storing summary information in a list
summary_info <- list(
  highest_IMDb_year = highest_IMDb_year,
  highest_IMDb_alltime = highest_IMDb_alltime,
  actors_in_highest_IMDb_movies = actors,
  age_imdb_data_table = age_imdb_data,
  year_with_highest_avg_imdb = year_avg_imdb$release_year,
  movie_with_most_votes = most_votes_movie
)

summary_info