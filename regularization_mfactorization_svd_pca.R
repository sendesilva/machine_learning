### Regularization
library(dslabs)
library(tidyverse)
library(dplyr)
data("movielens")

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

# model fitting
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

# 10 mistakes made in model fitting
test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>% slice(1:10) %>% knitr::kable()

# 10 best movies
movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()

# 10 best movies
movie_avgs %>% left_join(movie_titles, by='movieId') %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()

# 10 worst movies
movie_avgs %>% left_join(movie_titles, by='movieId') %>%
  arrange(b_i) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()

# now include num of times movies are rated
# best 10
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# worst 10
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


# Q1. Compute the number of ratings for each movie and then plot it against the year the movie came out. 
# Use the square root transformation on the counts. What year has the highest median number of ratings?

library(dslabs)
library(tidyverse)
library(dplyr)
data("movielens")
library(caret)

# median num rating per year
num_rating <- movielens %>% 
  na.omit() %>% 
  select(movieId, rating, year) %>% 
  group_by(movieId, year) %>%
  summarise(n_rating = n()) %>%
  ungroup() %>%
  arrange(desc(n_rating)) 

knitr::kable(head(num_rating, 10))

median_rating <- num_rating %>%
  select(n_rating, year) %>%
  group_by(year) %>%
  summarise(med = median(n_rating, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(med))

knitr::kable(head(median_rating))

# Answer:
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Q2. We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer
# movies, starting in 1993, the number of ratings decreases with year: the more recent a movie is, the less 
# time users have had to rate it.
# Among movies that came out in 1993 or later, what are the 25 movies with the most ratings per year, and 
# what is the average rating of each of the top 25 movies?
# What is the average rating for the movie The Shawshank Redemption?
# What is the average number of ratings per year for the movie Forrest Gump?
