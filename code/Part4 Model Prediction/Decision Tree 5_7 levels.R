# 5 Levels

test_5level <- read.csv("~/Desktop/test_5level.csv")
View(test_5level)
train_5level <- read.csv("~/Desktop/train_5level.csv")
View(train_5level)

library(C50)

# Model Training
# x: a data frame containing the training data without the class labels
# This means in our data, the cannabis will be excluded.
# y: a factor vector of the class labels
# We want to find the relationship with other elements, so we select cannabis as response.
# Also, R accepts factor as response, so we need to turn the int into factor to perform the result.

train_ct <- C5.0(x = train_5level[, -32], y = factor(train_5level$Cannabis))


# Classification of the training data
# This shows the table of train prediction of each variable in which class level.
# For example, in class level 2, there are 649 observations in the correct position, while 15 are wrong.
# Also, if we measure the train accuracy, it is about 89.24%
# The errors value is about 10.8% which is acceptable.

train_predict_table <- table(factor(train_5level$Cannabis), predict(train_ct, train_5level))
train_predict_table
train_accuracy <- sum(diag(train_predict_table)) / sum(train_predict_table)
train_accuracy


# Visualize the decision tree
# We can also plot the graph and visualize the decision tree.
# However, because of the large data, it need a while, and the graph is quite messy.
# Thus, I just comment this code here.

# plot(ct)


# Summary the training data
# We can find more detailed information here.
summary(train_ct)


# Evaluate the performance using testing data
# This shows the table of testing data evaluation of each variable in which class level.
# For example, in class level 2, there are 191 observations in the correct position, while 30 are wrong.
# Also, if we measure the test accuracy, it is about 55.96%
# The errors value is about 44.04%.
test_predict_table <- table(factor(test_5level$Cannabis), predict(train_ct, test_5level))
test_predict_table
test_accuracy <- sum(diag(test_predict_table)) / sum(test_predict_table)
test_accuracy





# 7 Levels

train_7level <- read.csv("~/Desktop/train_nofactor.csv")
View(train_7level)
test_7level <- read.csv("~/Desktop/test_nofactor.csv")
View(test_7level)

library(C50)

# Model Training
# x: a data frame containing the training data without the class labels
# This means in our data, the cannabis will be excluded.
# y: a factor vector of the class labels
# We want to find the relationship with other elements, so we select cannabis as response.
# Also, R recept factor as response, so we need to turn the int into factor to perform the result.

train_7level2 <- train_7level[, -32]
test_7level2 <- test_7level[, -32]

train_ct_7level <- C5.0(x = train_7level2[, -32], y = factor(train_7level2$Cannabis))


# Classification of the training data
# This shows the table of train prediction of each variable in which class level.
# For example, in class level 0, there are 294 observations in the correct position, while 15 are wrong.
# Also, if we measure the train accuracy, it is about 87.33%
# The errors value is about 12.7% which is acceptable.

train_predict_table_7level <- table(factor(train_7level2$Cannabis), predict(train_ct_7level, train_7level2))
train_predict_table_7level
train_accuracy_7level <- sum(diag(train_predict_table_7level)) / sum(train_predict_table_7level)
train_accuracy_7level


# Visualize the decision tree
# We can also plot the graph and visualize the decision tree.
# However, because of the large data, it need a while, and the graph is quite messy.
# Thus, I just comment this code here.

# plot(ct)


# Summary the training data
# We can find more detailed information here.
summary(train_ct_7level)


# Evaluate the performance using testing data
# This shows the table of testing data evaluation of each variable in which class level.
# For example, in class level 0, there are 71 observations in the correct position, while 32 are wrong.
# Also, if we measure the test accuracy, it is about 36.73%
# The errors value is about 63.27%.
test_predict_table_7level <- table(factor(test_7level2$Cannabis), predict(train_ct_7level, test_7level2))
test_predict_table_7level
test_accuracy_7level <- sum(diag(test_predict_table_7level)) / sum(test_predict_table_7level)
test_accuracy_7level

