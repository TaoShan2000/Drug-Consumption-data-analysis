trainDat=read.csv('E:/STAT 362/362 proj/train_nofactor.csv',header=TRUE)
testDat=read.csv('E:/STAT 362/362 proj/test_nofactor.csv',header=TRUE)

library(caret)
library('MLmetrics')

trainDat3 = trainDat
testDat3 = testDat

trainDat3$Cannabis = as.factor(trainDat3$Cannabis)
testDat3$Cannabis = as.factor(testDat3$Cannabis)
trainDat3$Cannabis = make.names(trainDat3$Cannabis)
testDat3$Cannabis = make.names(testDat3$Cannabis)
trainDat3 = na.omit(trainDat3)
testDat3 = na.omit(testDat3)

objControl <- trainControl(method='cv', number=10,summaryFunction = multiClassSummary, classProbs = TRUE)

objModel <- train(Cannabis~., trainDat3, 
                  method='gbm', 
                  trControl=objControl)

gbm.Grid = expand.grid(interaction.depth = c(2,3,4,5,6), 
                       n.trees = 500, 
                       shrinkage = c(0.1, 0.05, 0.01),
                       n.minobsinnode = 10,
                       tuneGrid = gbm.Grid,
                       verbose = TRUE)

summary(objModel)
predictions <- predict(object=objModel, testDat3, type='raw')
table = table(predictions, testDat3$Cannabis)

cat("The  accuracy is:",sum(diag(table)) / sum(table))

###############This function returns the accuracy and kappa
cat(postResample(pred=predictions, obs=as.factor(testDat3$Cannabis))) 
