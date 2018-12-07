### Recommendation Systems
library(dslabs)
library(tidyverse)
library(dplyr)
data("movielens")
head(movielens)
# num unique users and moview
movielens %>%
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
str(movielens)
# test accuracy of models
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,] # partition by row
test_set <- movielens[test_index,]

# remove users and movies not in train set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# RMSE rating
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Building Recommendation Systems
