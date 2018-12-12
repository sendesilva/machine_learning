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

# compute mu and movie_avgs
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

# create database connecting movieId to movie title
movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()

# Effect of lambda
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# top 10 movies after regularization
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# 10 worst movies after regularization
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# compare RMSE with earlier methods
rmse_results <- data.frame(method = "Just the average", RMSE = 1.0482202)
rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie Effect Model", RMSE = 0.9862839))
rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie + User Effects Model", RMSE = 0.8848688))


predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

# using cv to choose lambda
lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)] # 3

# using full cv to choose lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)] 
lambda    # optimal lambda with full cv = 3.75

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()  # rmse = 0.881



### Comprehension check: Regularization
## 100 schools dataset, n = num students
set.seed(1986)
n <- round(2^rnorm(1000,8,1))

# true quality for each school independent of size
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
schools <- data.frame(id = paste("PS", 1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
# top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# simulated test scores
set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1. What are the top schools based on the average score? Show just the ID, size, and the average score.
# Report the ID of the top school and average score of the 10th school. What is the ID of the top school?
# Note that the school IDs are given in the form "PS x" - where x is a number. Report the number only.

top10_score <- schools %>% top_n(10, score) %>% arrange(desc(score))
top10_score

# Q2. Compare the median school size to the median school size of the top 10 schools based on the score.
# What is the median school size overall?
# What is the median school size of the of the top 10 schools based on the score?
medsize_overall <- median(schools$size)
medsize_overall
medsize_top10 <- median(top10_score$size)
medsize_top10


# Q3. According to this analysis, it appears that small schools produce better test scores than large schools.
# Four out of the top 10 schools have 100 or fewer students. But how can this be? We constructed the 
# simulation so that quality and size were independent. Repeat the exercise for the worst 10 schools.
# What is the median school size of the bottom 10 schools based on the score?

btm10_score <- schools %>% top_n(-10, score) %>% arrange(score)
btm10_score
medsize_btm10 <- median(btm10_score$size)
medsize_btm10


# Q4. Answer
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)


# Q5. Let's use regularization to pick the best schools. Remember regularization shrinks deviations from the 
# average towards 0. To apply regularization here, we first need to define the overall average for all 
# schools, using the following code:

overall <- mean(sapply(scores, mean))

# Then, we need to define, for each school, how it deviates from that average. Write code that estimates the 
# score above the average for each school but dividing by n+alpha  instead of n, with n the schools size and
# alpha the regularization parameters. Try alpha=25. What is the ID of the top school with regularization?

reg_schools <- schools %>% select(id, size, score) %>%
  mutate(b_i = (score - overall)*size/(size+25)) %>%
  mutate(reg_score = overall + b_i)

top10_reg_schools <- reg_schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))
top10_reg_schools



# Q6. Notice that this improves things a bit. The number of small schools that are not highly ranked is now 
# lower. Is there a better ? Find the  alpha that minimizes the 
# RMSE = (1/100)Summation_i:1-100(quality - estimate)^2. What value of alpha gives the minimum RMSE?

RMSE <- function(mu, score_reg){
  sum((mu - score_reg)^2)/100
}

alpha <- seq(1, 500, 1)
rmses <- sapply(alpha, function(alpha){
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  return(RMSE(mu, score_reg))
})
alpha[which.min(rmses)]
qplot(alpha, rmses)


# Q7. Rank the schools based on the average obtained with the best . Note that no small school is incorrectly
# included. What is the ID of the top school now? 
# What is the regularized average score of the 10th school now?
alpha <- 128
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


# Q8. A common mistake made when using regularization is shrinking values towards 0 that are not centered 
# around 0. For example, if we don't subtract the overall average before shrinking, we actually obtain a 
# very similar result. Confirm this by re-running the code from the exercise in Q6 but without removing the 
# overall mean. What value of  gives the minimum RMSE here?
RMSE <- function(mu, score_reg){
  sum((mu - score_reg)^2)/100
}

alpha <- seq(10, 250)
rmses <- sapply(alpha, function(alpha){
  score_reg <- sapply(scores, function(x)  sum(x)/(length(x)+alpha))
  return(RMSE(mu, score_reg))
})
alpha[which.min(rmses)]
qplot(alpha, rmses) 



### Matrix Factorization
# using a subset of movielens that has many ratings and users
library(dslabs)
library(tidyverse)
library(dplyr)
data("movielens")
library(caret)

train_small <- movielens %>%
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% # 3252 is Scent of a Woman used in eg.
  group_by(userId) %>%
  filter(n() >=50) %>% ungroup()
y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# add rownames and columnnames
rownames(y) <- y[,1]
movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()
colnames(y) <- with(movie_titles, title[match(colnames(y),movieId)])

y <- sweep(y, 1, rowMeans(y, na.rm = TRUE))
y <- sweep(y, 2, colMeans(y, na.rm = TRUE))

# set na to 0
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

# pple cpts or q vectors
dim(pca$rotation)

# p vectors
dim((pca$x))

# accessing vaariability via plot
plot(pca$sdev)

# var explained
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

# movie association with PCs
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

