trainDat3=read.csv('E:/STAT 362/362 proj/train.csv',header=TRUE)
testDat3=read.csv('E:/STAT 362/362 proj/test.csv',header=TRUE)
target=read.csv('E:/STAT 362/362 proj/dfnew.csv',header=TRUE)
trainDat3$Cannabis<-as.factor(target$Cannabis[1:1413])
length(target$Cannabis)
#install.packages('MLmetrics')
library('MLmetrics')

trainDat3$Cannabis = as.factor(trainDat3$Cannabis)
testDat3$Cannabis = as.factor(testDat3$Cannabis)
trainDat3$Cannabis = make.names(trainDat3$Cannabis)
testDat3$Cannabis = make.names(testDat3$Cannabis)
trainDat3 = na.omit(trainDat3)
testDat3 = na.omit(testDat3)

str(trainDat3)


objControl <- trainControl(method='cv', number=10, returnResamp='none', summaryFunction = multiClassSummary, classProbs = TRUE)

objModel <- train(Cannabis~., trainDat3, 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))
summary(objModel)
predictions <- predict(object=objModel, testDat3, type='raw')

head(predictions)

cat(postResample(pred=predictions, obs=as.factor(testDat3$Cannabis))) #0.4659574 0.3329262
