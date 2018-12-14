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



### Comprehension check: Matrix Factorization - SVD

set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))


# Q1. How would you describe the data based on this figure?
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)  # students that test well on top and reverse at the btm and grouped by 3 subjects


# Q2. You can examine the correlation between the test scores directly:
# Which of the following best describes what you see?
my_image(cor(y), zlim = c(-1,1))
range(cor(y)) #  0.4855371 1.0000000
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2) 
# There is correlation among all tests, but higher if the tests are in science and math and even higher 
# within each subject. correct


# Q3. Use the function svd to compute the SVD of y. This function will return U, V, 
# and the diagonal entries of D.

s <- svd(y)
names(s)

# check that SVD works:
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of  Y and store them in ss_y. Then compute the sum of squares 
# of columns of the transformed  YV and store them in ss_yv. 
# Confirm that sum(ss_y) is equal to sum(ss_yv). 
# What is the value of sum(ss_y) (and also the value of sum(ss_yv))?
y_sq <- y*y # entries squared
ss_y <- colSums(y_sq)
sum(ss_y) #  175434.6 


y_svd_sq <- y_svd*y_svd # entries squared
ss_yv <- colSums(y_svd_sq)
sum(ss_yv) #  175434.6


# Q4. We see that the total sum of squares is preserved. This is because  is orthogonal. Now to start 
# understanding how  is useful, plot ss_y against the column number and then do the same for ss_yv.
# What do you observe?

plot(ss_y)
plot(ss_yv)


# Q6. So from the above we know that the sum of squares of the columns of  Y (the total sum of squares) 
# adds up to the sum of s$d^2 and that the transformation  YV gives us columns with sums of squares 
# equal to s$d^2. Now compute the percent of the total variability that is explained by just the first 
# three columns of YV. 
# What proportion of the total variability is explained by the first three columns of YV?

YV_t <- y%*%s$v
var_explained <- cumsum(sd(YV_t[,1])^2/sum(var(YV_t))) + cumsum(sd(YV_t[,2])^2/sum(var(YV_t))) +
  cumsum(sd(YV_t[,3])^2/sum(var(YV_t)))

var_explained


# Q7. A useful computational trick to avoid creating the matrix diag(s$d). To motivate this, we note 
# that if we write out in its columns [U1, U2, .. Up] then  UD = [U1d1,1, U2d2,2, ... Updp,p]
# Use the sweep function to compute UD without constructing diag(s$d) or using matrix multiplication.

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))


# Q8. We know that U1d1,1, the first column of UD, has the most variability of all the columns of UD.
# Earlier we looked at an image of  using my_image(y), in which we saw that the student to student 
# variability is quite large and that students that are good in one subject tend to be good in all. 
# This implies that the average (across all subjects) for each student should explain a lot of the 
# variability. Compute the average score for each student, plot it against U1d1,1, and 
# describe what you find. What do you observe?
#ud <- s$u%*%s$d
plot(y_svd[,1], rowMeans(y))

<<<<<<< HEAD

# Q9. We note that the signs in SVD are arbitrary because: UDV^T = (-U)Dz(-V)^T
# With this in mind we see that the first column of UD is almost identical to the average score for each
# student except for the sign. This implies that multiplying Y by the first column of V must be 
# performing a similar operation to taking the average. Make an image plot of V and describe the first 
# column relative to others and how this relates to taking an average.
# How does the first column relate to the others, and how does this relate to taking an average?

# my_image(cor(y), zlim = c(-1,1))
# range(cor(y)) #  0.4855371 1.0000000
# axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2) 
my_image(s$v)
dim(s$v)


## Ungraded practice Qs10-13











### Comprehension Check: Clustering

# Q1. Load the tissue_gene_expression dataset. Remove the row means and compute the distance between 
# each observation. Store the result in d. 
# Which of the following lines of code correctly does this computation?

data("tissue_gene_expression")
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))


# Q2. Make a hierarchical clustering plot and add the tissue types as labels. You will observe multiple 
# branches. Which tissue type is in the branch farthest to the left?

h <- hclust(d)
plot(h, cex = 0.75) # liver


