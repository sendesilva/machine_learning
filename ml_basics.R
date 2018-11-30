library(caret)
library(dslabs)
library(tidyverse)
library(tidyr)
data(heights)
y <- heights$sex
x <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

# algorithm1 - simply guessing the sex without the predictor - height
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)

# representing categorical outcomes as factors
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

# overall accuracy
mean(y_hat == test_set$sex) # 0.524

# actual data provides insight
heights %>% group_by(sex) %>%
  summarize(mean(height), sd(height))

# algorithm2 - predict male if within 2 sd from average male
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex) # 0.723
mean(y == y_hat) # 0.793

# repeat algo2 for 10 different cut-offs and choose best one
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

plot(cutoff, accuracy)
max(accuracy) # 0.836
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff  # 64 

# testing best_cutoff on test_set
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex) # 0.817

### Comprehension check - basics
mnist <- read_mnist()
Z <- mnist$train$labels
class(Z)
Z[5] + Z[6] # 11
Z[5] # 9
Z[6] # 2

## Confusion matrix for prediction of gender / sex
table(predicted = y_hat, actual = test_set$sex)

# computing prediction accuracy
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))

# confusion matrix
confusionMatrix(data = y_hat, reference = test_set$sex)

## maximising F-score instead of overall accuracy
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff, F_1)
max(F_1) # 0.61
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff  # 66

# confusion matrix
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference =  test_set$sex)

# ROC curve for guessing heights with different probabilities
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), length(test_index), replace = TRUE, 
           prob=c(p, 1-p)) %>% factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", 
                   ylab = "Sensitivity")

# ROC for diff cutoff heights 
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# ROC curves comparing sensitivty / 1-Specificity for guessing and cutoffs
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

# adding cutoff labels to the points
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text(nudge_y = 0.01)

# precision-recall curve - when prevalence matters
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


### Comprehension check: Confusion Matrix
library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

fi <- sum(dat$sex == "Female" & dat$type == "inclass") 
mi <- sum(dat$sex == "Male" & dat$type == "inclass")
fo <- sum(dat$sex == "Female" & dat$type == "online")
mo <- sum(dat$sex == "Male" & dat$type == "online")
pfi <- fi / (fi + mi)
pfo <- fo / (fo + mo)
pfi  # proportion inclass = female 0.6666667
pfo  # proportion online  = female 0.3783784


#using type to predict sex -> what is prediction accuracy
# from insight earlier use cutoff inclass = female
y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>%
  factor(levels = levels(y))
confusionMatrix(data = y_hat, reference =  y)


# prevalence
sum(dat$sex == "Female") / 
(sum(dat$sex == "Female") + sum(dat$sex == "Male"))



### Comprehension Check: Practice with Machine Learning
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


set.seed(2)
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

head(train, n=50)
# visually identifies petal.length as singular feature that can 
# distinguish bet species versicolor (3 - 4.9) and virginica (5 - 7)

cutoff <- seq(3,7,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
  })

plot(cutoff, accuracy)
max(accuracy) # 0.96
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff # 4.7

# testing best_cutoff on test_set
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
#y_hat <- factor(y_hat)
mean(y_hat == test$Species) # accuracy on test set = 0.9

# check test data for best feature
head(test, n=50)
# visually identifies petal.width as singular feature that can 
# distinguish bet species versicolor (1.0 - 1.7) and virginica (1.4 - 2.5)
# use training set to detmine best cutoff for petal.width
cutoff <- seq(0.5,3,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x | train$Petal.Length > 4.7 , "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff, accuracy)
max(accuracy) # 0.96
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff # 1.5

# test with a combined rule
y_hat <- ifelse(test$Petal.Length > 4.7 & test$Petal.Width > 1.5, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species) # accuracy on test set = 0.92


### Comprehension Check: Conditional Probabilities Review
## Coding for screening tests
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Q2 What is probability that test is positive?
ttl.tplus_dplus <- sum(test[disease==1])
ttl.tplus_dminus <- sum(test[disease==0])
tplus <- (ttl.tplus_dplus + ttl.tplus_dminus) / 1e6
tplus # [1] 0.114509


# Q4 What is probability that an individual has the disease if test is positive
ttl.tplus <- ttl.tplus_dplus + ttl.tplus_dminus
dplus_tplus <- ttl.tplus_dplus / ttl.tplus 
dplus_tplus  # 0.1471762

# Q3 What is probability that an individual has the disease if test is negative
ttl.tminus_dplus <- sum(disease) - sum(test[disease==1])
ttl.tminus_dminus <- sum(disease == 0) - sum(test[disease==0])
ttl.tminus <- ttl.tminus_dplus + ttl.tminus_dminus
dplus_tminus <- ttl.tminus_dplus / ttl.tminus
dplus_tminus  # 0.003461356

# Q5 If the test is positive, what is the relative risk of having the disease?
# probablity of getting disease given test is positive normalised againt the prevalence of disease
prev <- 0.02
rr <- dplus_tplus / prev
rr # 7.36


### Comprehension Check: Conditional Probabilities Practice
# Q1 We are now going to write code to compute conditional probabilities for being male in 
# the heights dataset. Round the heights to the closest inch. Plot the estimated conditional 
# probability  P(x) = Pr(Male|height=x) for each x.

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# Q2 In the plot we just made in Q1 we see high variability for low values of height. This 
# is because we have few data points. This time use the quantile (\ 0.1,0.2,\dots,0.9 \)and 
# the cut function to assure each group has the same number of points. Note that for any 
# numeric vector x, you can create groups based on quantiles like this: 
# cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


# Q3 You can generate data from a bivariate normal distrubution using the MASS package using 
# the following code.

# Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
# dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
#   data.frame() %>% setNames(c("x", "y"))
# And make a quick plot using plot(dat).

# Using an approach similar to that used in the previous exercise, let's estimate the 
# conditional expectations and make a plot. 

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)