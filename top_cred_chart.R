library(tidyverse)
library(dplyr)

titles_df <- read.csv("https://info-201b-sp23.github.io/final-project-proposal-WhenPigsInvade/titles.csv")
credits_df <- read.csv("https://info-201b-sp23.github.io/final-project-proposal-WhenPigsInvade/credits.csv")

# Add IMDb scores to actors 
credit_scored_df <- merge(x = credits_df, y = titles_df[ , c("id", "imdb_score")], by = "id", all.x=TRUE)
credit_scored_df <- credit_scored_df %>% 
  group_by(name) %>% 
  summarize(imdb_score = mean(imdb_score, na.rm=T), num_roles = n())

active_creds_score_df <- credit_scored_df %>% 
  filter(num_roles > 5)


# Top Ten Actors by IMDb score average
highest_rated_actor <- active_creds_score_df %>% 
  filter(imdb_score == max(imdb_score, na.rm=T)) %>% 
  pull(name)

top_actors <- active_creds_score_df[with(active_creds_score_df, order(-imdb_score)), ]
top_actors <- top_actors[1:10, ] 

top_plot <- ggplot(top_actors) +
  geom_col(aes(x=imdb_score, y=reorder(name, imdb_score)), fill="navy", width=0.6) +
  labs(
    title = "Top 10 Credited Contributors Based on IMDB Score", 
    subtitle = "Actors and Directors"
  ) +
  scale_x_continuous(limits = c(0, 10, 1),
                     breaks = seq(0, 10, 1),
                     expand = c(0, 0),
                     position = "top") +
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme (
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#A8BAC4"),
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color = "black"),
    axis.text.y = element_blank(),
  ) +
  geom_text(
    aes(0, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    size = 4
  )

