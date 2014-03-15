#load data
trainData = read.csv("Machine1.csv", header = TRUE)
testData = read.csv("Machinetest.csv", header = TRUE)

#load rpart
library(rpart)
library('kernlab')


#seperate X and Y in train and test data
trainData_x = trainData[,-1];
trainData_y = trainData[,1];

testData_x = testData[,-1];
testData_y = testData[,1];

#calcMSE method
calcMSE = function(predictedY,target) {
	error = predictedY-target
	return(mean(error^2))
}

#calcMAE method
calcMAE = function(predictedY,target) {
	error = abs(predictedY-target)
	return(mean(error))
}

#calcMAE method
calcRsq = function(predictedY,target) {
	errsq = (predictedY-target)^2
	meansq = (target-mean(target))^2
	
	frac = sum(errsq)/sum(meansq)
	rsq = 1-frac

	return(rsq)
}
#set random seed for doing cross validation
set.seed(100)

#create regression tree with cv=5
myTree <- rpart(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX, data =trainData, control = rpart.control(xval = 5))


#prune tree by choosing CP with min error
cpIndex <- myTree$cptable[which.min(myTree$cptable[,"xerror"]),"CP"]
myPrunedTree <- prune(myTree,cpIndex)

#print text version of tree
#print(myTree)

#print summary
#print(summary(myTree))

#plot tree
#plot(myTree)
#text(myTree,use.n=T)



#plot tree
png("RT.png")
par(xpd = TRUE)
plot(myPrunedTree,compress=T,branch=0,uniform=T)
text(myPrunedTree,use.n=T,fancy=T,all=T)
dev.off()




#testing pruned tree
ptTrainOut = predict(myPrunedTree,newdata=trainData);
ptMSE = calcMSE(ptTrainOut,trainData_y);
ptMAE = calcMAE(ptTrainOut,trainData_y);

ptCP = printcp(myPrunedTree)
ptCP.rsq <- 1-ptCP[,c(3,4)] 

#testing pruned tree test data
ptTestOut = predict(myPrunedTree,newdata=testData);
ptTestMSE = calcMSE(ptTestOut,testData_y);
ptTestMAE = calcMAE(ptTestOut,testData_y);

ptTestCP = printcp(myPrunedTree)
ptTestCP.rsq <- 1-ptTestCP[,c(3,4)] 

#SVR

mySvr =  ksvm(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=trainData,scaled=TRUE,type="nu-svr",kernel="vanilladot", kpar="automatic", class.weights=NULL,cache=100,cross=5);
svrTrainOut = predict(mySvr,newdata=trainData,type="response");

svrMSE = calcMSE(svrTrainOut,trainData_y);
svrMAE = calcMAE(svrTrainOut,trainData_y);
svrRsq = calcRsq(svrTrainOut,trainData_y)


svrTestOut = predict(mySvr,newdata=testData,type="response");

svrTestMSE = calcMSE(svrTestOut,testData_y);
svrTestMAE = calcMAE(svrTestOut,testData_y);
svrTestRsq = calcRsq(svrTestOut,testData_y)




#MLR
myMlr = lm(PRP~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=trainData)
mlrTrainOut = predict(myMlr,newdata=trainData)

mlrMSE = calcMSE(mlrTrainOut, trainData_y);
mlrMAE = calcMAE(mlrTrainOut, trainData_y);
mlrSummary = summary(myMlr)


mlrTestOut = predict(myMlr,newdata=testData)

mlrTestMSE = calcMSE(mlrTestOut, testData_y);
mlrTestMAE = calcMAE(mlrTestOut, testData_y);
mlrTestSummary = summary(myMlr)

#plot of results from all three models
png("predict-results.png")
plot(mlrTestOut,xlab="testcase",ylab="PRP",ylim=c(1,700))
points(ptTestOut,col='red')
points(svrTestOut,col='blue')
points(testData_y,col='green')
legend("top",legend=c("MLR","RT","SVR","target"),pch='o',col=c("black","red","blue","green"));
dev.off()

#print MSE results
print("Train MSE-RT")
print(ptMSE)
print("Train MSE-SVR")
print(svrMSE)
print("Train MSE-MLR")
print(mlrMSE)

#print MAE results
print("Train-MAE-RT")
print(ptMAE)
print("Train-MAE-SVR")
print(svrMAE)
print("Train-MAE-MLR")
print(mlrMAE)

#print R-squared results
print("RT train r-squared:")
print(ptCP.rsq[nrow(ptCP.rsq),1])
print("SVR train r-squared:")
print(svrRsq)
print("MLR train r-squared:")
print(mlrSummary$r.squared)


#print MSE results
print("Test MSE-RT")
print(ptTestMSE)
print("Test MSE-SVR")
print(svrTestMSE)
print("Test MSE-MLR")
print(mlrTestMSE)

#print MAE results
print("Test-MAE-RT")
print(ptTestMAE)
print("Test-MAE-SVR")
print(svrTestMAE)
print("Test-MAE-MLR")
print(mlrTestMAE)

#print R-squared results
print("RT Test r-squared:")
print(calcRsq(ptTestOut,testData_y))
print("SVR Test r-squared:")
print(svrTestRsq)
print("MLR Test r-squared:")
print(calcRsq(mlrTestOut,testData_y))
