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
# simplest model with all movies having same rating Y_hat = mu_hat + epsilon
mu_hat <- mean(train_set$rating)
mu_hat # avg across all users and movies
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# other nums increase RMSE
predictions <- rep(2.5, nrow(test_set))
head(predictions)
RMSE(test_set$rating, predictions)

# create table for storing results of obtaining RMSE from different methods
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# # Include avg rating or effect b for movie i 
# fit <- lm(rating ~ as.factor(userId), data = movielens) - very slow due num of movies

# Using a faster estimation is b_i = mean(y_i) - mu_i
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))


# comparing rmse from 'just the average' method with method include b_i effects for movies
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# investigating user effects
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# # try lm to fit but could crash computer due size of data:
# lm(rating ~ as.factor(movieId) + as.factor(userId))

# Hence estimate b_u:
user_avgs <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

# check rmse of new improved model
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


### Comprehension check: Recommendation Systems
# The following exercises all work with the movielens data, which can be loaded using the following code:
library(dslabs)
data("movielens")

# Q1. Compute the number of ratings for each movie and then plot it against the year the movie came out. 
# Use the square root transformation on the counts. What year has the highest median number of ratings?

