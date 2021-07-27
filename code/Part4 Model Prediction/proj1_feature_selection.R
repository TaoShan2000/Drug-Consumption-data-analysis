library(MASS)
library(ggplot2)
library(ggpubr)
library(tidyverse)

str(df1)
df1 = read.csv("E:/STAT 362/df1.csv", header = TRUE)
  
df_new = read.csv("E:/STAT 362/dfnew.csv", header = TRUE)


ff1 <- ggplot(df_new, aes(x = Education)) + 
  geom_histogram()

ff2 <- ggplot(df_new, aes(x = Nscore)) + 
  geom_histogram()

ff3 <- ggplot(df_new, aes(x = Escore)) + 
  geom_histogram()

ggarrange(ff1, ff2, ff3)

ff9 <- ggplot(df_new, aes(x = Oscore)) + 
  geom_histogram()

ff10 <- ggplot(df_new, aes(x = Ascore)) + 
  geom_histogram()

ff11 <- ggplot(df_new, aes(x = Cscore)) + 
  geom_histogram()

ff12 <- ggplot(df_new, aes(x = Impulsive)) + 
  geom_histogram()

ggarrange(ff9, ff10, ff11, ff12)

ff13 <- ggplot(df_new, aes(x = SS)) + 
  geom_histogram()

ff14 <- ggplot(df_new, aes(x = Alcohol)) + 
  geom_histogram()

ff15 <- ggplot(df_new, aes(x = log(Alcohol + 1))) + 
  geom_histogram() # not getting better
ggarrange(ff13,ff14,ff15)


################################ Create new features
df_new2 = df_new
unique(df_new2$Ethnicity)
summary(as.factor(df_new2$Ethnicity))

## choose df_new2$Ethnicity == -0.31685, other = 0
df_new2['Ethnicity_max'] = as.numeric(ifelse(df_new2$Ethnicity == -0.31685,1,0))
str(df_new2)

## choose 
unique(df_new2$Semer) #0 2 3 4 1

filter(df_new2, Semer == 0)

df_new2['Semer_0'] = as.integer(ifelse(df_new2$Semer == 0,1,0)) 

str(df_new2)


################### Correlation  matrix
cor(df_new2, df_new2$Cannabis)
?cor
########################
colnames(df_new2)
model2 = lm(Cannabis~., data = df_new2)
step(model2, direction = 'both', trace = 0)
# #~ Age + Gender + Education + Country + 
# Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet + 
#   Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth + 
#   Mushrooms + Nicotine + Semer + Semer_0

model3 = lm(Cannabis~ Age + Gender + Education + Country + 
              Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet +
                Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth +
                Mushrooms + Nicotine + Semer + Semer_0 + Education:Nscore+Education:Escore + Education:Oscore
            + Education:Cscore + Education:Oscore:Ascore:Cscore:Impulsive:Escore:SS,
            data = df_new2)
step(model3, direction = 'both', trace = 0)
# Cannabis ~ Age + Gender + Education + Country + 
#   Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet + 
#   Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth + 
#   Mushrooms + Nicotine + Semer + Semer_0

model4 = lm(Cannabis~(.)^2, data = df_new2)
step(model3, direction = 'both', trace = 0)

# Cannabis ~ Age + Gender + Education + Country + 
#      Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet + 
#      Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth + 
#      Mushrooms + Nicotine + Semer + Semer_0

# According to the correlation matrix, choose those with high correlation coeff
model5 = lm(Cannabis~ Age + Gender + Education + Country + 
                   Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet +
                   Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth +
                   Mushrooms + Nicotine + Semer + Semer_0 + Age:Gender + Age:Country+
              Age:Oscore + Country:Oscore + Legalh:LSD + LSD:Mushrooms + Ecstasy :Mushrooms,
            data = df_new2)
step(model5, direction = 'both', trace = 0)


# Cannabis ~ Age + Gender + Education + Country + 
#   Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet + 
#   Coke + Ecstasy + Heroin + Legalh + Meth + Mushrooms + Nicotine + 
#   Semer + Semer_0 + Age:Gender + Age:Country + Age:Oscore + 
#   Legalh:LSD + Ecstasy:Mushrooms


library(rpart)
library('caret')
df_new2['Age_Gender'] = df_new2$Age * df_new2$Gender
df_new2['Age_Country'] = df_new2$Age * df_new2$Country
df_new2['Age_Oscore'] = df_new2$Age * df_new2$Oscore
df_new2['Country_Oscore'] = df_new2$Country  * df_new2$Oscore
df_new2['Legalh_LSD'] = df_new2$Legalh  * df_new2$LSD
df_new2['LSD_Mushrooms'] = df_new2$Mushrooms  * df_new2$LSD
df_new2['Ecstasy_Mushrooms'] = df_new2$Mushrooms  * df_new2$Ecstasy

trainID = sample(1885, round(0.7 * 1885))
sum(is.na(trainID))
trdf_new2= rpart(Cannabis~ Age + Gender + Education + Country + 
                   Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet +
                   Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth +
                   Mushrooms + Nicotine + Semer + Semer_0 + Age_Gender + Age_Country+
                   Age_Oscore + Country_Oscore + Legalh_LSD + LSD_Mushrooms + Ecstasy_Mushrooms,
                 data = df_new2, 
                 subset = trainID, control = list(cp=0.01))

varImp(trdf_new2) #?????????

# > varImp(trdf_new2) #?????????
# Overall
# Age               0.27085659
# Age_Country       0.41171401
# Age_Gender        0.12459741
# Amphet            0.23414228
# Amyl              0.06345388
# Coke              0.41005944
# Country           0.74303554
# Country_Oscore    0.23037463
# Cscore            0.09421525
# Ecstasy           0.80335935
# Education         0.07934527
# Legalh            0.35072966
# Legalh_LSD        0.04596230
# LSD_Mushrooms     0.15573708
# Mushrooms         1.07675879
# Nicotine          1.02642410
# Oscore            0.09368484
# SS                0.05042265
# Gender            0.00000000
# Ethnicity         0.00000000
# Nscore            0.00000000
# Escore            0.00000000
# Heroin            0.00000000
# Ketamine          0.00000000
# Meth              0.00000000
# Semer             0.00000000
# Semer_0           0.00000000
# Age_Oscore        0.00000000
# Ecstasy_Mushrooms 0.00000000

############################################
library(randomForest)
rfModel2 = randomForest(Cannabis~ Age + Gender + Education + Country + 
                          Ethnicity + Nscore + Escore + Oscore + Cscore + SS + Amphet +
                          Amyl + Coke + Ecstasy + Heroin + Ketamine + Legalh + Meth +
                          Mushrooms + Nicotine + Semer + Semer_0 + Age_Gender + Age_Country+
                          Age_Oscore + Country_Oscore + Legalh_LSD + LSD_Mushrooms + Ecstasy_Mushrooms,
                        data = df_new2,
                        subset = trainID,
                       importance = TRUE, 
                       ntree=1000)
sum(is.na((df_new2)))
str(df_new2)
sum(is.na(df_new2[trainID,]))
str(df_new2[trainID,])

df_new2[is.na(df_new2[trainID,])]


##############################

trainDat=read.csv('E:/STAT 362/feature_selection_dummies.csv',header=TRUE)
#trainDat = read.csv("E:/STAT 362/df1.csv", header = TRUE)
target=read.csv('E:/STAT 362/dfnew.csv',header=TRUE)
trainDat$Cannabis<-as.factor(target$Cannabis)


library(caret)
ctrl <- trainControl(method = "cv",
                     number = 10)

gbm.Grid = expand.grid(interaction.depth = c(2,3,4,5,6), 
                       n.trees = 100,  #(1:5)*100
                       shrinkage = c(0.1, 0.05, 0.01),
                       n.minobsinnode = 10) 
gbm.cv.model <- train(Cannabis ~ ., data = trainDat,
                      method = "gbm",
                      trControl = ctrl,
                      tuneGrid = gbm.Grid,
                      verbose = FALSE,na.action = na.omit)
gbm.cv.model
str(trainDat)
str(target)
boost.cv.pred = predict(gbm.cv.model, newdata =target)
mean(boost.cv.pred != target$Cannabis)

#Error in eval(predvars, data, env) : 找不到对象'Alcohol.Legalh'


################################################
# Stochastic Gradient Boosting 
# 
# 1884 samples
# 32 predictor
# 7 classes: '0', '1', '2', '3', '4', '5', '6' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 1692, 1693, 1692, 1690, 1691, 1692, ... 
# Resampling results across tuning parameters:
#   
#   shrinkage  interaction.depth  Accuracy   Kappa    
# 0.01       2                  0.4707804  0.3301497
# 0.01       3                  0.4787621  0.3404850
# 0.01       4                  0.4872701  0.3518705
# 0.01       5                  0.4899691  0.3558140
# 0.01       6                  0.4889055  0.3548623
# 0.05       2                  0.4803864  0.3513500
# 0.05       3                  0.4798656  0.3533292
# 0.05       4                  0.4883314  0.3663236
# 0.05       5                  0.4877970  0.3641876
# 0.05       6                  0.4909910  0.3708142
# 0.10       2                  0.4761141  0.3530788
# 0.10       3                  0.4771268  0.3572363
# 0.10       4                  0.4765836  0.3562658
# 0.10       5                  0.4734228  0.3529407
# 0.10       6                  0.4627478  0.3398094
# 
# Tuning parameter 'n.trees' was held constant at a value of 100
# Tuning parameter 'n.minobsinnode' was
# held constant at a value of 10
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were n.trees = 100, interaction.depth = 6, shrinkage = 0.05
# and n.minobsinnode = 10.