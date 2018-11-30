library(dslabs)
library(tidyverse)
data("olive")
head(olive) # 8 fatty acids
# For illustrative purpose, we'll try to predict the region using the fatty acid composition values as
# predictors. It's either Northern Italy, Sardinia, or Southern Italy.
table(olive$region)

# remove area column because we don't use it as a predictor
olive <- select(olive, -area)

# using Knn from caret
library(caret)
fit <- train(region ~ ., method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), data = olive)
fit$results$Accuracy
ggplot(fit)

# Distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free")

# from review of distribution: eico only in S Italy, linoleic separates N Italy from Sardinia
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p

## Regression tree using 2008 US poll data with continuous outcome
data("polls_2008")
qplot(day, margin, data = polls_2008)

# partition space using rpart
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
# show decision tree with partitions
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
# show partitions on data - Note that we can prune tree by snipping of partitions 
# that do not meet a cp criterion:
pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


## Classification or Decision Trees for categorical data.
train_rpart <- train(y ~ ., method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)
confusionMatrix(predict(train_rpart, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

## Random Forests
library(randomForest)
fit <- randomForest(margin ~., data = polls_2008) 
plot(fit) # error converges quickly by 200
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

# applying random forests to 2 or 7 problem
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)

confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

# conditional probabilities
library(gam)
# plot_cond_prob(predict(train_rf, mnist_27$true_p, type = "prob")[,2])
# plot_cond_prob - user defined function

# using different random forest method - Rborist
fit <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


### Comprehension check: Trees and Random Forests
# Q1.
library(rpart)
library(tidyverse)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
# Which code correctly uses rpart to fit a regression tree and saves the result to fit?
fit <- rpart(y ~ ., data = dat)

# Q2 Which of the following plots correctly shows the final tree obtained in Q1?
plot(fit)
text(fit, cex = 0.75)

# Q3 Below is most of the code to make a scatter plot of y versus x along with the predicted values 
# based on the fit. Which line of code should be used to replace #BLANK in the code above?

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)   #BLANK


# Q4 Now run Random Forests instead of a regression tree using randomForest from the __randomForest__ 
# package, and remake the scatterplot with the prediction line. Part of the code is provided for you below.
library(randomForest)
fit <- randomForest(y ~ x, data = dat) #BLANK 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

  
# Q5 Use the plot function to see if the Random Forest from Q4 has converged or if we need more trees.
# Which is the correct plot to assess whether the Random Forest has converged?  
plot(fit)  


# Q6 It seems that the default values for the Random Forest result in an estimate that is too flexible 
# (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. 
# Remake the plot. Part of the code is provided for you below.


library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25) #BLANK
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  


  
### Caret Package - demonstration using 2 or 7 data
library(dslabs)
  library(tidyverse)
data("mnist_27")  
library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)    
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)   

# predict using predict.train function
y_hat_glm <- predict.train(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict.train(train_knn, mnist_27$test, type = "raw")  
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]  
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]  
    
  
## Tuning parameters with Caret
getModelInfo("knn")

# quick look up
modelLookup("knn")

# view results of X-validation using train using 'highlight' to highlight parameter optimising
# algorithm on default values
ggplot(train_knn, highlight = TRUE) # optimises at k=9

# to try for a different k use tuneGrid
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune # best k
# best model
train_knn$finalModel

# testing for accuracy of X-validation using the test set
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), 
                mnist_27$test$y)$overall["Accuracy"]


# using trainControl to do 10 fold X-validation
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)
train_knn$bestTune # best k = 45

# std dev bars from X-validation samples
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD, 
                    ymax = Accuracy + AccuracySD))

## try loess for smoother conditional probability boundary
library(gam)
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess", 
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), 
                mnist_27$test$y)$overall["Accuracy"]
# plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
# user defined function - not available


### Comprehension check: Caret package

# Q1 In the exercise in Q6 from Comprehension Check: Trees and Random Forests, we saw that changing 
# nodesize to 50 and setting maxnodes to 25 yielded smoother results. Let's use the train function to 
# help us pick what the values of nodesize and maxnodes should be.
# From the caret description of methods, we see that we can't tune the maxnodes parameter or the nodesize 
# argument with randomForests. So we will use the __Rborist__ package and tune the minNode argument. 
# Use the train function to try values minNode <- seq(25, 100, 25). Set the seed to 1.
library(rpart)
library(tidyverse)
set.seed(1)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

library(randomForest)
train_Rborist <- train(y ~ x,
                       method = "Rborist",
                       data = dat,
                       tuneGrid = data.frame(predFixed = 1, minNode = seq(25, 100, 25)))
ggplot(train_Rborist, highlight = TRUE)
train_Rborist$bestTune


# Q2 Part of the code to make a scatterplot along with the prediction from the best fitted model is provided 
# below.

library(caret)
dat %>% 
  mutate(y_hat = predict(train_Rborist)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)#BLANK


# Q3 Use the rpart function to fit a classification tree to the tissue_gene_expression dataset. Use the 
# train function to estimate the accuracy. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to 
# report the results of the best model. Set the seed to 1991.
# Which value of cp gives the highest accuracy?
library(dslabs)
data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991)
train_rpart <- train(x,y, method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
                    
plot(train_rpart)

# Q4 Study the confusion matrix for the best fitting classification tree from the exercise in Q3.
# What do you observe happening for the placenta samples?
confusionMatrix(train_rpart)

# Q5 Note that there are only 6 placentas in the dataset. By default, rpart requires 20 observations before 
# splitting a node. That means that it is difficult to have a node in which placentas are the majority. 
# Rerun the analysis you did in the exercise in Q3, but this time, allow rpart to split any node by using 
# the argument control = rpart.control(minsplit = 0). Look at the confusion matrix again to determine whether
# the accuracy increases. Again, set the seed to 1991. What is the accuracy now?

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991)
train_rpart <- train(x,y, method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                     control = rpart.control(minsplit = 0))
plot(train_rpart)
confusionMatrix(train_rpart)


# Q6 Plot the tree from the best fitting model of the analysis you ran in Q5.
# Which gene is at the first split?

fit <- rpart(y~x, data = tissue_gene_expression)
plot(fit)
text(fit)

# Q7 We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can
# predict the tissue type with even fewer genes using a Random Forest. Use the train function and the rf 
# method to train a Random Forest. Try out values of mtry ranging from seq(50, 200, 25) (you can also 
# explore other values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as 
# we did with the classification trees, use the following argument: nodesize = 1.
# Note: This exercise will take some time to run. If you want to test out your code first, try using smaller 
# values with ntree. Set the seed to 1991 again.

library(rpart)
library(tidyverse)
library(dslabs)
library(caret)
library(randomForest)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991)
train_rf <- train(x,y, method = "rf",
                     tuneGrid = data.frame(mtry = seq(50, 200, 25)))
ggplot(train_rf, highlight = TRUE)
train_rf$bestTune

# Q8 Use the function varImp on the output of train and save it to an object called imp.
# imp <- #BLANK
# imp
imp <- varImp(train_rf, useModel = FALSE, scale = FALSE) 
imp

varImp(train_rf)


# Q9. The rpart model we ran above produced a tree that used just seven predictors. Extracting the predictor 
# names is not straightforward, but can be done. If the output of the call to train was fit_rpart, we can 
# extract the names like this:

# tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
# tree_terms

# Calculate the variable importance in the Random Forest call for these seven predictors and examine where 
# they rank. What is the importance of the CFHR4 gene in the Random Forest call?

# What is the rank of the CFHR4 gene in the Random Forest call?

# set.seed(1991)
# data("tissue_gene_expression")
# 
# fit_rpart <- with(tissue_gene_expression, 
#                   train(x, y, method = "rpart",
#                         tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
#                         control = rpart.control(minsplit = 0)))

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

varImp(fit_rpart)

set.seed(1991)
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))
tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
tree_terms
varImp(fit)
