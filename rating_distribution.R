library(tidyverse)
library(stringr)

titles_df <- read.csv("https://info-201b-sp23.github.io/final-project-proposal-WhenPigsInvade/titles.csv")
# credits_df <- read.csv("https://info-201b-sp23.github.io/final-project-proposal-WhenPigsInvade/credits.csv")

titles_df <- titles_df %>% filter(release_year > 1999)

age_cert_per_year <- titles_df %>% 
  group_by(release_year, age_certification) %>% summarize(sum=n_distinct(id))

unrated <- age_cert_per_year %>% filter(nchar(age_certification) == 0) %>%
  mutate(age_certification="Unrated")

# age_cert_per_year <- age_cert_per_year %>% filter(!nchar(age_certification) == 0)

children <- age_cert_per_year %>% 
  filter(age_certification %in% c("G", "PG", "TV-G", "TV-Y", "TV-Y7")) %>% 
  mutate(age_certification="Children")

teen <- age_cert_per_year %>% 
  filter(age_certification %in% c("PG-13", "TV-14")) %>% 
  mutate(age_certification="Teen")

adult <- age_cert_per_year %>% 
  filter(age_certification %in% c("TV-MA", "R", "NC-17")) %>% 
  mutate(age_certification="Adult")


age_cert_per_year <- full_join(children, teen)
age_cert_per_year <- full_join(age_cert_per_year, adult)
age_cert_per_year <- full_join(age_cert_per_year, unrated)

age_cert_per_year <- age_cert_per_year %>% mutate(total=sum(sum)) %>%
  mutate(prop=sum/total, .before = total)

dist_plot <- ggplot(age_cert_per_year, aes(x=release_year, y=prop, fill=age_certification)) +
  
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(2000, 2022, 1)) +
  labs(x = "Year", y = "Proportion",
       title = "Age rating proportion from 2000-2023")

dist_plot

