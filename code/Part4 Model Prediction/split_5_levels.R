# change 7 levels -> 5 levels for our target

trainDat = read.csv('train_nofactor.csv', header = TRUE)
testDat = read.csv('test_nofactor.csv', header = TRUE)
testDat <- na.omit(testDat)

# delete country.choc(too many 0s)
trainDat <- trainDat[,-(ncol(trainDat)-1)]
testDat <- testDat[,-(ncol(testDat)-1)]

# Combine CL0,1,2 to CL2
intClass = trainDat$Cannabis
intClass[trainDat$Cannabis == 0] = 2
intClass[trainDat$Cannabis == 1] = 2
trainDat$Cannabis = intClass

intClass_test = testDat$Cannabis
intClass_test[testDat$Cannabis == 0] = 2
intClass_test[testDat$Cannabis == 1] = 2
testDat$Cannabis = intClass_test

# the remaining is CL2,3,4,5,6

write.csv(trainDat, file = 'train_5level.csv', row.names = FALSE)
write.csv(testDat, file = 'test_5level.csv', row.names = FALSE)



