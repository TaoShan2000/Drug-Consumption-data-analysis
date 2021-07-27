rm(list = ls())
df_new <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/dfnew.csv")
library(class)
library(randomForest)
library(MLmetrics)
library(caret)
library(fastDummies)

dat1 <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/feature_selection_dummies.csv")
dat2 <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/feature_selection_facdummies.csv")

train <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/train.csv")
test <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/test.csv")

train_raw <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/train_raw.csv")
test_raw <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/test_raw.csv")

trainDat = read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/train_nofactor.csv", header = TRUE)
testDat = read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/test_nofactor.csv", header = TRUE)
testDat <- na.omit(testDat)

train_5level <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/train_5level.csv")
test_5level <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/test_5level.csv")

set.seed(1)

train_n <- train[,-ncol(train)]
test_n <- test[,-ncol(train)]
train_labels <- train$Cannabis
test_labels <- test$Cannabis

train_min <- apply(train_n, 2, min)
train_max <- apply(train_n, 2, max)
for (i in 1:ncol(train_n)) {
  train_n[, i] <- (train_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  test_n[, i] <- (test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}


k <- c(1, 5, 11, 15, 17, 21, 27)
result <- matrix(0, nrow = length(k), ncol = 4)
result[, 1] <- k
colnames(result) = c("k value", "False Negatives", "False Positives", 
                     "Percent Classified Correctly")

for (i in 1:length(k)) {
  knn_predicted <- knn(train = train_n, test = test_n,
                       cl = train_labels, k = k[i])
  confusion_matrix <- table(test_labels, knn_predicted)
  result[i, 2] <- confusion_matrix[2, 1]
  result[i, 3] <- confusion_matrix[1, 2]
  result[i, 4] <- sum(diag(confusion_matrix)) / length(test_labels)
}

result

set.seed(362)

# 1
test <- na.omit(test)
train$Cannabis <- as.character(train$Cannabis)
test$Cannabis <- as.character(test$Cannabis)

modelA <- randomForest(factor(Cannabis) ~ ., data = train, importance = TRUE)
print(modelA)
plot(modelA)

tab1 <- table(predict(modelA, test[, -ncol(test)]), test$Cannabis)
tab1
paste('Testing accuracy is:', sum(diag(tab1)) / sum(tab1)) # 0.484

# 2
test_raw <- na.omit(test_raw)
train_raw$Cannabis <- as.character(train_raw$Cannabis)
test_raw$Cannabis <- as.character(test_raw$Cannabis)

modelB <- randomForest(factor(Cannabis) ~ ., data = train_raw, importance = TRUE)
print(modelB)
plot(modelB)

tab2 <- table(predict(modelB, test_raw[, -ncol(test)]), test_raw$Cannabis)
tab2
paste('Testing accuracy is:', sum(diag(tab2)) / sum(tab2)) # 0.493

# 3
test_5level <- na.omit(test_5level)
train_5level$Cannabis <- as.character(train_5level$Cannabis)
test_5level$Cannabis <- as.character(test_5level$Cannabis)

modelC <- randomForest(factor(Cannabis) ~ ., data = train_5level, importance = TRUE)
print(modelC)
plot(modelC)

tab3 <- table(predict(modelC, test_5level[, -ncol(test)]), test_5level$Cannabis)
tab3
paste('Testing accuracy is:', sum(diag(tab3)) / sum(tab3)) # 0.636

ctrl <- trainControl(method = "cv", number = 15)

#
rf.Grid = expand.grid(mtry = 2:12) 
rf.cv.model = train(factor(Cannabis) ~ ., data = train,
                    method = "rf",
                    trControl = ctrl,
                    tuneGrid = rf.Grid) 

rf.cv.model 
rf.cv.model$bestTune # mtry = 8
rf.cv.model$pred

# a
tab_a <- table(predict(rf.cv.model, test[, -ncol(test)]), test$Cannabis)
tab_a
paste('Testing accuracy is:', sum(diag(tab1)) / sum(tab1)) # 0.484

# b
rf.Grid = expand.grid(mtry = 2:12) 
rf.cv.model = train(factor(Cannabis) ~ ., data = train_raw,
                    method = "rf",
                    trControl = ctrl,
                    tuneGrid = rf.Grid) 

rf.cv.model 
rf.cv.model$bestTune # mtry = 9
rf.cv.model$pred

tab_b <- table(predict(rf.cv.model, test_raw[, -ncol(test_raw)]), test_raw$Cannabis)
tab_b
paste('Testing accuracy is:', sum(diag(tab1)) / sum(tab1)) # 0.484

# c
rf.Grid = expand.grid(mtry = 2:12) 
rf.cv.model = train(factor(Cannabis) ~ ., data = train_5level,
                    method = "rf",
                    trControl = ctrl,
                    tuneGrid = rf.Grid) 

rf.cv.model 
rf.cv.model$bestTune # mtry = 6
rf.cv.model$pred

tab_c <- table(predict(rf.cv.model, test_5level[, -ncol(test_5level)]), test_5level$Cannabis)
tab_c
paste('Testing accuracy is:', sum(diag(tab_c)) / sum(tab_c)) # 0.636

train_labels <- train$Cannabis
test_labels <- test$Cannabis
# predict on train set
train_pred <- predict(modelB, train_labels, type = "class")
# check classification accuracy
table1 <- table(train_pred, train$Cannabis)
sum(diag(table1)) / sum(table1)

# predict on test set
test_pred <- predict(modelB, test, type = "class")
# check classification accuracy
mean(test_pred == test$Cannabis)
table(test_pred, test$Cannabis)

# To check important variables
importance(modelA)
varImpPlot(modelA)

n_features <- length(setdiff(names(train), "Cannabis"))
rfA <- ranger(Cannabis ~ ., data = train, mtry = floor(n_features / 3), respect.unordered.factors = "order", seed = 2)
default_rmse <- sqrt(rfA$prediction.error)

pred_rfA <- predict(rfA, test)
head(pred_rfA)

pred_ranger <- predict(ames_ranger, ames_test)
head(pred_ranger$predictions)


























































