trainDat=read.csv('E:/STAT 362/362 proj/train_raw.csv',header=TRUE)
testDat=read.csv('E:/STAT 362/362 proj/test_raw.csv',header=TRUE)
library(caret)
library('MLmetrics')

trainDat$Cannabis = as.factor(trainDat$Cannabis)
testDat$Cannabis = as.factor(testDat$Cannabis)
trainDat$Cannabis = make.names(trainDat$Cannabis)
testDat$Cannabis = make.names(testDat$Cannabis)

str(trainDat3)


objControl <- trainControl(method='cv', number=10,  summaryFunction = multiClassSummary, classProbs = TRUE)

gbm.Grid = expand.grid(interaction.depth = c(2,3,4,5,6), 
                       n.trees = 500, 
                       shrinkage = c(0.1, 0.05, 0.01),
                       n.minobsinnode = 10)

objModel <- train(Cannabis~., trainDat, 
                  method='gbm', 
                  trControl=objControl,
                  tuneGrid = gbm.Grid,
                  verbose = TRUE)
objModel

summary(objModel)
predictions <- predict(object=objModel, testDat, type='raw')
table = table(predictions, testDat$Cannabis)

cat("The  accuracy is:",sum(diag(table)) / sum(table))

cat(postResample(pred=predictions, obs=as.factor(testDat$Cannabis))) 

# predictions X0 X1 X2 X3 X4 X5 X6 0.5010616
# X0 85 25 14  4  2  2  5
# X1 10 17  5  2  0  1  3
# X2  6  7 35 10  3  4  7
# X3  0  0  5  4  3  2  5
# X4  0  0  0  0  1  0  0
# X5  0  0  0  1  1  1  2
# X6  2  2  8 32 25 37 93
