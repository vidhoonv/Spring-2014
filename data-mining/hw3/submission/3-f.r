library('randomForest')
#load data
trainData = read.csv("Machine1.csv", header = TRUE)
trainData2 = read.csv("Machine2.csv", header = TRUE)
testData = read.csv("Machinetest.csv", header = TRUE)

#seperate X and Y in train and test data
trainData_x = trainData[,-1];
trainData_y = trainData[,1];

testData_x = testData[,-1];
testData_y = testData[,1];


calcRsq = function(predictedY,target) {
	errsq = (predictedY-target)^2
	meansq = (target-mean(target))^2
	
	frac = sum(errsq)/sum(meansq)
	rsq = 1-frac

	return(rsq)
}

set.seed(105) 
myForest = randomForest(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX, data =trainData, ntree=10)
forestTrainOut = predict(myForest,newdata=trainData)


print("Forest Train R squared:")
print(myForest$rsq[length(myForest$rsq)])

forestTestOut = predict(myForest,newdata=testData)


print("Forest Test R squared:")
print(calcRsq(forestTestOut,testData_y))
 
myForest1 = randomForest(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX, data =trainData2, ntree=10)
forestTrainOut = predict(myForest1,newdata=trainData2)


print("Forest Train R squared:")
print(myForest1$rsq[length(myForest1$rsq)])

forestTestOut = predict(myForest1,newdata=testData)


print("Forest Test R squared:")
print(calcRsq(forestTestOut,testData_y))

