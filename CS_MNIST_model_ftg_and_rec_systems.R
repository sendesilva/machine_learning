library(dslabs)
mnist <- read_mnist()
names(mnist)

dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)

# Using random sample of 10000 rows for training set and 1000 rows for test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

# Features with zero or close to zero variability
library(matrixStats)
library(tidyverse)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

# Using Caret's nearZeroVar to remove zero or close to zero var
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

# columns kept
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# column names as required by caret
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

## use knn to build model that maximises accuracy
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y, 
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)), 
                   trControl = control)
ggplot(train_knn, highlight = TRUE)


# testing on variable n, b starting from small to ensure code works and then increasing num
# what is reasonable in terms of processing time vs optimum accuracy
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index,], 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

# fitting model after optimising n and b
fit_knn <- knn3(x[ , col_index], y, k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[ , col_index],
                     type = "class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

## using Random Forrest - Rborist package
library(Rborist)
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10,15,25,35,50))
train_rf <- train(x[ , col_index],
                  y,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)
ggplot(train_rf)

# choose parameters using bestTune
train_rf$bestTune

# optimse to get final tree
fit_rf <- Rborist(x[, col_index], y, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

# demonstrating Variable importance using RF package
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y, ntree = 50)
imp <- importance(rf)
imp

image(matrix(imp, 28, 28))

# finding the digits where the call was incorrect
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

## Ensembling different algorithms into 1
p_rf <- predict(fit_rf, x_test[,col_index])$census  
p_rf<- p_rf / rowSums(p_rf)
p_knn  <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)


### Comprehension check: Ensembles
# Q1 Use the training set to build a model with several of the models available from the caret package. 
# We will test out all of the following models in this exercise:
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

# We have not explained many of these, but apply them anyway using train with all the default parameters. 
# You will likely need to install some packages. Keep in mind that you will probably get some warnings. Also,
# it will probably take a while to train all of the models - be patient!

# Run the following code to train the various models:
library(caret)
library(dslabs)
library(mboost)
library(libcoin)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2 Now that you have all the trained models in a list, use sapply or map to create a matrix of predictions 
# for the test set. You should end up with a matrix with length(mnist_27$test$y) rows and length(models).
# What are the dimensions of the matrix of predictions?
length(mnist_27$test$y) # 200
length(models) # 23

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

# Q3 Now compute accuracy for each model on the test set. Report the mean accuracy across all models.
mean(apply(pred, 2, function(object) object == mnist_27$test$y))

# Q4 Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
# What is the accuracy of the ensemble?
library(KODAMA) # remove!!!
library(mclust)
mv <- apply(pred, 1, function(object) majorityVote(object)$majority)
mean(as.factor(mv) == mnist_27$test$y)


#Q5 In Q3, we computed the accuracy of each method on the training set and noticed that the individual 
# accuracies varied. How many of the individual methods do better than the ensemble?
library(randomForest)
library(Rborist)

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)
# mean=0.84, ans=0.845

# on test set
ensemble <- 0.845
better_than_mean <- acc[acc > ensemble]
length(better_than_mean)
better_than_mean

# ans
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]


# Q6 It is tempting to remove the methods that do not perform well and re-do the ensemble. The problem with 
# this approach is that we are using the test data to make a decision. However, we could use the accuracy 
# estimates obtained from cross validation with the training data. Obtain these estimates and save them in 
# an object. Report the mean accuracy of the new estimates.
# What is the mean accuracy of the new estimates?

list(fits)
fits_acc <- sapply(fits, function(model) model$results["Accuracy"])
for (i in 1:length(fits_acc)) {
    fits_acc_num <- fits_acc[[i]]
   }
head(fits_acc_num)
mean(fits_acc_num) # [1] 0.8388977

# ans:
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat) # 0.8118911
acc_hat

# Q7 Now let's only consider the methods with an estimated accuracy of greater than or equal to 0.8 when 
# constructing the ensemble. What is the accuracy of the ensemble now?

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat) # 0.8118911
acc_hat
new_ensemble <- acc_hat[acc_hat >= 0.8]
new_ensemble
mean(new_ensemble) # 0.821013

#ans
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)



