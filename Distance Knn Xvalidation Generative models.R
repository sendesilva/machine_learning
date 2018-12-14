library(dslabs)
library(tidyverse)
### Distance
set.seed(0)
# train dataset with feature matrix measurements for 784 features
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]
# save obsevation vectors
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
# distance between is expected to be smaller than diff nums
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

# matrix algebra allows faster computation using crossproduct
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

# using dist fuction - to access entries using row and col indices
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

# image of distances
image(as.matrix(d))
# after ordering distances by labels
image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))

# The distance with pixel 492 and its spatial image
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))


### Comprehension check: Distance
library(dslabs)
data("tissue_gene_expression")
# This dataset includes matrix x which has the gene expression levels of 500 genes 
# from 189 biological samples representing seven different tissues. The tissue type
# is stored in y:
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

# Q1. compute euclidean distance between each observation
d <- dist(tissue_gene_expression$x)


# Q2. Compare the distances between observations 1 and 2 (both cerebellum), observations 39 and 40 (both 
# colon), and observations 73 and 74 (both endometrium). Distance-wise, are samples from tissues of the 
# same type closer to each other?
as.matrix(d)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)]

# Q3. Make a plot of all the distances using the image function to see if the pattern you observed in Q2 
# is general. Which code would correctly make the desired plot?
image(as.matrix(d))

### Knn
# baseline: use logistic regression
library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")        
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

# Compare logistic regression against Knn3 which comes with caret package.
# 1st way uses a dataframe: knn_fit <- knn3(y ~ ., data = mnist_27$train)
# 2nd way uses a matrix: 
# x <- as.matrix(mnist_27$train[,2:3])
# y <- mnist_27$train$y
# knn_fit <- knn3(x, y)

# using the default k = 5
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

# using the predict function to maximises the probability of the outcome
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# overtraining
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, 
                reference=mnist_27$train$y)$overall["Accuracy"]
#> Accuracy 0.995

# However, the test set accuracy is actually worse than logistics regression:
  y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 0.735

# Oversmoothing
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 0.79


# choosing a better k: try odd nums between 3 and 251 (somewhere half way between 5 and 401)
# and using map_df
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  list(train = train_error, test = test_error)
})
# plot(ks, accuracy) - cannot plot .. x and y lengths differ


### Comprehension check: Nearest Neighbours
# Q1 Previously, we used logistic regression to predict sex based on height. Now we are going to use knn 
# to do the same. Use the code described in these videos to select the F_1 measure and plot it against k. 
# are to the F_1 of about 0.6 we obtained with regression. Set the seed to 1

library(caret)
library(dslabs)
library(purrr)
library(tidyverse)
data(heights)
y <- heights$sex
x <- heights$height
set.seed(1)
train_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[-train_index,]
train_set <- heights[train_index,]

ks <- seq(1, 101, 3)
F_1 <- map_df(ks, function(k){
  set.seed(1)
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_val <- F_meas(data = y_hat, reference = factor(test_set$sex))
  list(k=k, F_val=F_val)
})
F_1
F_1 %>% na.omit() %>% ggplot(aes(k, F_val)) + geom_line()
max(F_1$F_val)
best_k <- F_1$k[which.max(F_1$F_val)]
best_k
F_1 %>% slice(which.max(F_1$F_val)) # alternate coding
# using seq(1, 101, 3): max F_1 = .605 correct best_k = 50, 33, 45 (reverse test and training set), 
# 85 all 4 wrong


# Q2 Split the data into training and test sets, and report the accuracy you obtain. 
# Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.
library(dslabs)
data("tissue_gene_expression")

# y <- tissue_gene_expression$y
# x <- tissue_gene_expression$x
# set.seed(1)
# train_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
# test_set_x <- x[-train_index,]
# train_set_x <- x[train_index,]
# # test_set_y <- y[-train_index,]
# # train_set_y <- y[train_index,]
# ks <- seq(1, 11, 2)
# accuracy <- map_df(ks, function(k){
#   fit <- knn3(train_set_x, y, k = k)
#   y_hat <- predict(fit, test_set_x, type = "class")
#   match <- confusionMatrix(data = y_hat, reference = test_set_x)$overall["Accuracy"]
#   list(k=k, match=match)
# })

# alt code:
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1)
fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,11,2)))



### Comprehensino check: X-validation
# Q1
library(caret)
library(tidyverse)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results

# Q2 Now, instead of using a random selection of predictors, we are going to search for those that are 
# most predictive of the outcome. We can do this by comparing the values for the  group to those in the  
# group, for each predictor, using a t-test. You can do perform this step like this:

# add genefilter for r 3.5 ... from bioconductor website
library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)

str(tt)
head(tt$p.value)


# Q3 Create an index ind with the column numbers of the predictors that were "statistically significantly"
# associated with y. Use a p-value cutoff of 0.01 to define "statistically significantly."
# How many predictors survive this cutoff?

ind <- tt$p.value <= 0.01
sum(ind)


# Q4 Now re-run the cross-validation after redefinining x_subset to be the subset of x defined by the 
# columns showing "statistically significant" association with y. What is the accuracy now?

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
tt <- colttests(x, y)
x_subset <- x[ ,tt$p.value <= 0.01]
fit <- train(x_subset, y, method = "glm")
fit$results


# Q5 Re-run the cross-validation again, but this time using kNN. Try out the following grid 
# k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies. Which code is correct?
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


# Q7 Use the train function to predict tissue from gene expression in the tissue_gene_expression dataset.
# Use kNN. What value of k works best?

library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)



