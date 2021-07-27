set.seed(1)
wbcd_train = read.csv('/Users/zhengqisun/Downloads/train_5level.csv', header = TRUE)
wbcd_test = read.csv('/Users/zhengqisun/Downloads/test_5level.csv', header = TRUE)

# last element is target
wbcd_train_n <- wbcd_train[,-ncol(wbcd_train)]
wbcd_test_n <- wbcd_test[,-ncol(wbcd_train)]
wbcd_train_labels <- wbcd_train$Cannabis
wbcd_test_labels <- wbcd_test$Cannabis

train_min <- apply(wbcd_train_n, 2, min)
train_max <- apply(wbcd_train_n, 2, max)
for (i in 1:ncol(wbcd_train_n)) {
  wbcd_train_n[, i] <- (wbcd_train_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  wbcd_test_n[, i] <- (wbcd_test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}


k <- c(1, 5, 11, 15, 17, 21, 27)
result <- matrix(0, nrow = length(k), ncol = 4)
result[, 1] <- k
colnames(result) = c("k value", "False Negatives", "False Positives", 
                     "Percent Classified Correctly")

for (i in 1:length(k)) {
  knn_predicted <- knn(train = wbcd_train_n, test = wbcd_test_n,
                       cl = wbcd_train_labels, k = k[i])
  confusion_matrix <- table(wbcd_test_labels, knn_predicted)
  result[i, 2] <- confusion_matrix[2, 1]
  result[i, 3] <- confusion_matrix[1, 2]
  result[i, 4] <- sum(diag(confusion_matrix)) / length(wbcd_test_labels)
}

result

