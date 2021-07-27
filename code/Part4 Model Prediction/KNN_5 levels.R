
# 7 level:


wbcd_train = read.csv('content/train.csv', header = TRUE)
wbcd_test = read.csv('content/test.csv', header = TRUE)

# last element is target
wbcd_train_n <- wbcd_train[,-ncol(wbcd_train)]
wbcd_test_n <- wbcd_test[,-ncol(wbcd_train)]
wbcd_train_labels <- as.character(wbcd_train$Cannabis)
wbcd_test_labels <- as.character(wbcd_test$Cannabis)

set.seed(1)
library(class)
knn_predicted <- knn(train = wbcd_train_n, test = wbcd_test_n, 
                     cl = wbcd_train_labels, k = 17)


result_table <- table(wbcd_test_labels, knn_predicted)
result_table
paste('Testing accuracy is:', sum(diag(result_table)) / sum(result_table))

# 7 level with factor, 0.4416



wbcd_train = read.csv('content/train_nofactor.csv', header = TRUE)
wbcd_test = read.csv('content/test_nofactor.csv', header = TRUE)

# last element is target
wbcd_train_n <- wbcd_train[,-ncol(wbcd_train)]
wbcd_test_n <- wbcd_test[,-ncol(wbcd_train)]
wbcd_train_labels <- as.character(wbcd_train$Cannabis)
wbcd_test_labels <- as.character(wbcd_test$Cannabis)

library(class)
set.seed(1)
knn_predicted <- knn(train = wbcd_train_n, test = wbcd_test_n, 
                     cl = wbcd_train_labels, k = 17)


result_table <- table(wbcd_test_labels, knn_predicted)
result_table
paste('Testing accuracy is:', sum(diag(result_table)) / sum(result_table))

# 7 level with no factor, 0.4564

wbcd_train = read.csv('content/train_raw.csv', header = TRUE)
wbcd_test = read.csv('content/test_raw.csv', header = TRUE)

# last element is target
wbcd_train_n <- wbcd_train[,-18]
wbcd_test_n <- wbcd_test[,-18]
wbcd_train_labels <- as.character(wbcd_train$Cannabis)
wbcd_test_labels <- as.character(wbcd_test$Cannabis)

train_min <- apply(wbcd_train_n, 2, min)
train_max <- apply(wbcd_train_n, 2, max)
for (i in 1:ncol(wbcd_train_n)) {
  wbcd_train_n[, i] <- (wbcd_train_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  wbcd_test_n[, i] <- (wbcd_test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

library(class)
set.seed(1)
knn_predicted <- knn(train = wbcd_train_n, test = wbcd_test_n, 
                     cl = wbcd_train_labels, k = 17)


result_table <- table(wbcd_test_labels, knn_predicted)
result_table
paste('Testing accuracy is:', sum(diag(result_table)) / sum(result_table))

# with factor, 0.4565

# compare the performance, choose 2nd one.


# 5_level 

wbcd_train = read.csv('/Users/zhengqisun/Downloads/train_5level.csv', header = TRUE)
wbcd_test = read.csv('/Users/zhengqisun/Downloads/test_5level.csv', header = TRUE)

# last element is target
wbcd_train_n <- wbcd_train[,-ncol(wbcd_train)]
wbcd_test_n <- wbcd_test[,-ncol(wbcd_train)]
wbcd_train_labels <- as.character(wbcd_train$Cannabis)
wbcd_test_labels <- as.character(wbcd_test$Cannabis)

train_min <- apply(wbcd_train_n, 2, min)
train_max <- apply(wbcd_train_n, 2, max)
for (i in 1:ncol(wbcd_train_n)) {
  wbcd_train_n[, i] <- (wbcd_train_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  wbcd_test_n[, i] <- (wbcd_test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

library(class)
set.seed(1)
knn_predicted <- knn(train = wbcd_train_n, test = wbcd_test_n, 
                     cl = wbcd_train_labels, k = 17)


result_table <- table(wbcd_test_labels, knn_predicted)
result_table
paste('Testing accuracy is:', sum(diag(result_table)) / sum(result_table))



# 5 level: 0.6404
