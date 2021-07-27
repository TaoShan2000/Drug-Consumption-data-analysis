trainDat=read.csv('E:/STAT 362/362 proj/train_5level.csv',header=TRUE)
testDat=read.csv('E:/STAT 362/362 proj/test_5level.csv',header=TRUE)

str(trainDat)

library(caret)
library(MLmetrics)
trainDat$Cannabis = as.factor(trainDat$Cannabis)
testDat$Cannabis = as.factor(testDat$Cannabis)
trainDat$Cannabis = make.names(trainDat$Cannabis)
testDat$Cannabis = make.names(testDat$Cannabis)


objControl <- trainControl(method='cv', number=10, summaryFunction = multiClassSummary, classProbs = TRUE)


gbm.Grid = expand.grid(interaction.depth = c(2,3,4,5,6), 
                       n.trees = 500, 
                       shrinkage = c(0.1, 0.05, 0.01),
                       n.minobsinnode = 10)

objModel <- train(Cannabis~., trainDat, 
                  method='gbm', 
                  trControl=objControl,
                  tuneGrid = gbm.Grid)
summary(objModel)
predictions <- predict(object=objModel, testDat, type='raw')

table = table(predictions, testDat$Cannabis)


cat("The  accuracy is:",sum(diag(table)) / sum(table))

cat(postResample(pred=predictions, obs=as.factor(testDat$Cannabis)))


# predictions  X2  X3  X4  X5  X6 0.6255319
# X2 205  18   6   9  20
# X3   5   7   2   3   7
# X4   1   0   2   1   3
# X5   2   3   4   3   7
# X6   8  25  21  31  77


