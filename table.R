library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("scales")
library("knitr")

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

release_year_agg <- credits_title_data %>%
  distinct(title, .keep_all = TRUE) %>%
  group_by(release_year) %>%
  summarize(
    Number_of_Distinct_Movies = n(),
    Average_IMDb_Score = round(mean(imdb_score, na.rm = TRUE), 2),
    Average_IMDb_Votes = round(mean(imdb_votes, na.rm = TRUE)),
    Highest_Rated_Movie = first(title[imdb_score == max(imdb_score, na.rm = TRUE)])
  ) %>%
  arrange(desc(Average_IMDb_Score))

colnames(release_year_agg) <- c("Release Year", "Number of Distinct Movies", "Average IMDb Score", "Average IMDb Votes", "Highest Rated Movie")

knitr::kable(release_year_agg)
