### Recommendation Systems
library(dslabs)
library(tidyverse)
data("movielens")
head(movielens)
# num unique users and moview
movielens %>%
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
