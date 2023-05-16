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

#Find the actors who were acted in the highest rated IMDb movies 

#Calculate if age certification/restriction affect IMDb score

#Calculate the year with the highest average IMDb score

#Calculate the movie with the most IMDb votes 