rm(list = ls())
library(randomForest)
library(caret)

df_new <- read.csv("/Users/jannyliu/Desktop/STAT 362/Group Project/new data/dfnew.csv")
summary(df_new) # no missing data appears
sample.ind <- sample(2, nrow(df_new), replace = T, prob = c(0.05, 0.95))
data.dev <- df_new[sample.ind == 1, ]
data.val <- df_new[sample.ind == 2, ]

# Original data
table(df_new$Cannabis) / nrow(df_new)

# Training data
table(data.dev$Cannabis) / nrow(data.dev)

# Testing data
table(data.val$Cannabis) / nrow(data.val)

rf <- randomForest(Cannabis ~ ., ntree = 100, data = data.dev)
plot(rf)
print(rf)

# Variable Importance
varImpPlot(rf, sort = T, n.var = 10, main = "Top 10 - Variable Importance")
# LSD is the most important variable

var.imp <- data.frame(importance(rf, type = 2))

# make row names as columns
var.imp$Variables <- row.names(var.imp)

