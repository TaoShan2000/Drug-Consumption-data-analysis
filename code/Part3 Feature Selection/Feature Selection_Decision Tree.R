train <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/train.csv")
test <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/test.csv")

library(rpart)
library(caret)

model_A <- rpart(factor(Cannabis) ~ . , data = train, method = "class")
varImp(model_A)

library(C50)

set.seed(1)
random_index <- sample(nrow(df_new), 100)
train1 <- df_new[random_index, ]
test1 <- df_new[-random_index, ]

# To create a scatterplot matrix
pairs(train1)

library(rpart.plot)
model1 <- rpart(Cannabis ~ ., train, method = "class")
rpart.plot(model1, tweak = 1.2)

rpart.plot(model1, type = 4, extra = 101, tweak = 1.2)

pred <- predict(model1, test, type = "class")
table(test$Cannabis)
