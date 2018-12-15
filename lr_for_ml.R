library(dslabs)
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# guessing son's height
avg <- mean(train_set$son)
avg    # [1] 70.40682
# r^2 loss
mean((avg - test_set$son)^2) # 6.23

# using LR to fit
fit <- lm(son ~ father, data = train_set)
fit$coef
# r^2 loss
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2) #  4.82 - better than guessing


## Predict Funtion - takes fitted object from lm or glm and dataframe and predicts
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2) # get same value 4.815793



### Comprehension check - LR
#Q1 create data set. Use the caret package to partition the dataset into test and training 
# sets of equal size. Train a linear model and calculate the RMSE. Repeat this exercise 100 
# times and report the mean and standard deviation of the RMSEs. Use seed 1.
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

y <- dat$y
x <- dat$x
# test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# test_set <- dat %>% slice(test_index)
# train_set <- dat %>% slice(-test_index)


B <- 100 
set.seed(1)
rms <- replicate(B, {
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rms)  #  2.488661
sd(rms)    #  0.1243952


# Q2 Repeat the previous exercise but for datasets with n <- c(100, 500, 1000, 5000, 10000). 
# Save the average and standard deviation of RMSE from the 100 repetitions using a seed of 1.
# Hint: use the sapply or map functions.

set.seed(1)
myRMS <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n=n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rms <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(rms), sd(rms))
}

n <- c(100, 500, 1000, 5000, 10000)
sapply(n, myRMS) 


# Q4 
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

y <- dat$y
x <- dat$x
# test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# test_set <- dat %>% slice(test_index)
# train_set <- dat %>% slice(-test_index)


B <- 100 
set.seed(1)
rms <- replicate(B, {
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rms) #  0.9099808
sd(rms)  # 0.06244347


# Q6
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
y <- dat$y
x_1 <- dat$x_1
x_2 <- dat$x_2

cor(dat)
B <- 100 
set.seed(1)
rms <- replicate(B, {
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rms) #  y-x_1: 0.6369011; y-x_2: 0.6515611; y-x_1 + x_2: 0.3450927
sd(rms)  # y-x_1:  0.04133126; y-x_2: 0.03701591; y-x_1 + x_2: 0.02475961

# answer
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))   # 0.600666

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))       # [1] 0.630699

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))      # 0.3070962

# Q8
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)  

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))   # 0.6592608

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))  # 0.640081

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))  #  0.6597865


## Regression for categorial outcomes
library(dslabs)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# what is probability that you are F if you are 66in tall?

train_set %>%
  filter(round(height)==66) %>%
  summarize(mean(sex=="Female")) # mean = 0.24

lm__fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)

# use confusion matrix to determine accuracy of decision rule F if prediction > 0.5
p_hat <- predict(lm__fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)


## Logistic Regression - using glm
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)


## Designing algorithms for more than 1 predictors 

## 1st attempt using Logistic Regression - get accuracy of 79% 
# data("mnist_27")
# names(mnist_27)
# fit <- glm(y ~ x_1 + X_2, data = mnist_27$train, family = "binomial")
# p_hat <- predict(fit, newdata = mnist_27$test)
# y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
# confusionMatrix(data = y_hat, reference = mnist_27$test$y)

# conditional probabilities are known since dataset is constructed and not real life data
# mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
#   geom_raster()
# # improved version
# mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
#   geom_raster() +
#   scale_fill_gradientn(colors = c("#F8766D", "white", "#00BF(4")) +
#   stat_contour(breaks=c(0.5).color="black")

### Comprehension check
# Q1 Generate 25 different datasets changing the difference between the two classes using 
# delta <- seq(0, 3, len=25) and plot accuracy vs mu_1.
set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 0, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()


# delta <- seq(0, 3, len=25)

dat$train %>% ggplot(aes(x, color = y)) + geom_density()


## Introduction to smoothing
# one predictor example
data("polls_2008")
qplot(day, margin, data = polls_2008)

span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, x.points = day, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")


## Local Weighted Regression (loess)
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")


## Comprehension Check: Smoothing
# Q1 Use the loess function to obtain a smooth estimate of the expected number of deaths as a function of date.
# Plot this resulting smooth function. Make the span about two months long.
library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    
    
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")


total_days <- as.numeric(diff(range(dat$date)))
span <- 61/total_days
#span <- 61


fit <- loess(deaths ~ day, degree = 1, span = span, data = dat., na.action = na.exclude)

dat %>% na.omit %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(date, deaths)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(date, smooth), color = "red")

dat %>% mutate(smooth = predict(fit)) %>%
  ggplot(aes(date, deaths)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(date, smooth), color = "red")

# dat %>%
#   mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
  # ggplot(aes(day, smooth, col = year)) +
  # geom_line(lwd = 2)

# dat %>%
#   mutate(smooth = predict(fit, as.numeric(date)), day = mday(date), year = as.character(year(date))) %>%
#   ggplot(aes(day, smooth, col = year)) +
#   geom_line(lwd = 2)

# dat %>%
#   mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#   ggplot(aes(day, smooth)) +
#   geom_line(lwd = 2)

dat %>%
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2) # correct: col = year + day = yday(date)


# Q3
library(broom)
data(mnist_27)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
# qplot(x_2, y, data = mnist_27$train)
fit <- loess(as.numeric(y) ~ x_2, degree = 1, data = mnist_27$train, na.action = na.exclude)
mnist_27$train %>% mutate(smooth = predict(fit)) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(x_2, smooth), color = "red")


# Q3 complete


## Matrices
mnist <- read_mnist()
class(mnist$train$images)
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# creating a matrix
x_1 <- 1:5
x_2 <- 6:10
# x <- cbind(x_1, x_2)
# dim(x)
dim(x_1)
dim(as.matrix(x_1))
class(mnist)
dim(as.matrix(mnist))

# converting a vector to a matrix - fill by column
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t

# transpose matrix function
identical(t(mat), mat_t)

# recycles values w/o warning if mismatch vectors and matrix
matrix(my_vector, 5, 5)

# mapping pixel intensities for number recognition
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# r flips imagess which need to be flipped back
image(1:28, 1:28, grid[,28:1])

## Row and column summaries and apply
sums <- rowSums(x)
avg <- rowMeans(x)

# a boxplot can show how intensity changes from pixel to pixel
data.frame(labels = as.factor(y), row_averages = avg) %>%
  ggplot(aes(labels, row_averages)) +
  geom_boxplot()

# apply function to calculate row means
avgs <- apply(x, 1, mean)

# quantify the variation of each pixel with its standard deviation across all entries
# where 1 pixel = 1 column
library(matrixStats)
sds <- colSds(x) # columns sds
qplot(sds, bins = "30", color = I("black")) # distribution of sds
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1]) # variation by location
# shows little variability in the corners - because they are not used much

# extracting some columns and columns for inspection
x[, c(351,352)]
x[c(2,3),]

# removing uninformative columns - low sds
new_x <- x[, colSds(x) > 60]
dim(new_x)

# to preserve matrix for subsetted single col or row using drop
class(x[, 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

# matrix to vector
mat <- matrix(1:15, 5,3)
mat
as.vector(mat)

# histogram of all predictors
qplot(as.vector(x), bins = 30, color = ("black"))

# making values below of within specified condition into 0
# new_x <- x
# new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# Binarize data: pixels are either ink or no ink
# bin_x <- x
# bin_x[bin_x < 255/2] <- 0
# bin_x[bin_x > 255/2] <- 1
# 
# bin_x <- (x > 255/2)*1

# we can scale each row of a matrix using this simple code.
(x - rowMeans(x)) / rowSds(x)

# for columns we must transpose the matrix first
t(t(x) - colMeans(x))

# using sweep
X_mean_0 <- sweep(x, 2, colMeans(x))  # 2 = col

X_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(X_mean_0, 2, colSds(x), FUN = "/")

### Comprehension Check: Working with Matrices

set.seed(1)
x <- matrix(rnorm(3*5), 5, 3)
x

set.seed(1)
x <- matrix(rnorm(3*5), 3, 5)
x

x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

x <- matrix(1:15, 5, 3, byrow = TRUE)
# x
# x <- x + seq(nrow(x))
# x
# 
# x <- 1:nrow(x)
# x
# x <- sweep(x, 2, 1:nrow(x),"+")
# x

sweep(x, 1, 1:nrow(x),"+")
x

x <- matrix(1:15, 5, 3, byrow = TRUE)
x <- sweep(x, 2, 1:ncol(x), FUN = "+")
x

x <- matrix(1:15, 5, 3, byrow = TRUE)
x
rowMeans(x)
colMeans(x)

# Q6 For each digit in the mnist training data, compute the proportion of pixels that are in 
# the grey area, defined as values between 50 and 205. (To visualize this, you can make a 
# boxplot by digit class.) What proportion of pixels are in the grey area overall, defined as 
# values between 50 and 205?
library(broom)
if(!exists("mnist")) mnist <- read_mnist()
class(mnist$train$images)
x <- mnist$train$images
y <- mnist$train$labels
# avg <- rowMeans(x)
# sums <- rowSums(x)

data.frame(labels = as.factor(y), row_averages = avg) %>%
  ggplot(aes(labels, row_averages)) +
  geom_boxplot()

a <- sum(x > 50 & x < 205)
b <- sum(x >= 0)
a/b

