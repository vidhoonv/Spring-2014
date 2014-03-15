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
ptTestOut = predict(myPrunedTree,newdata=trainData);
ptMSE = calcMSE(ptTestOut,trainData_y);
ptMAE = calcMAE(ptTestOut,trainData_y);

ptCP = printcp(myPrunedTree)
ptCP.rsq <- 1-ptCP[,c(3,4)] 



#SVR

mySvr =  ksvm(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=trainData,scaled=TRUE,type="nu-svr",kernel="vanilladot", kpar="automatic", class.weights=NULL,cache=100,cross=5);
svrTestOut = predict(mySvr,newdata=trainData,type="response");

svrMSE = calcMSE(svrTestOut,trainData_y);
svrMAE = calcMAE(svrTestOut,trainData_y);
svrRsq = calcRsq(svrTestOut,trainData_y)

#MLR
myMlr = lm(PRP~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=trainData)
mlrTestOut = predict(myMlr,newdata=trainData)

mlrMSE = calcMSE(mlrTestOut, trainData_y);
mlrMAE = calcMAE(mlrTestOut, trainData_y);
mlrSummary = summary(myMlr)

#plot of results from all three models
png("predict-results.png")
plot(mlrTestOut,xlab="testcase",ylab="PRP",ylim=c(1,700))
points(ptTestOut,col='red')
points(svrTestOut,col='blue')
points(testData_y,col='green')
legend("top",legend=c("MLR","RT","SVR","target"),pch='o',col=c("black","red","blue","green"));
dev.off()

#print MSE results
print("MSE-RT")
print(ptMSE)
print("MSE-SVR")
print(svrMSE)
print("MSE-MLR")
print(mlrMSE)

#print MAE results
print("MAE-RT")
print(ptMAE)
print("MAE-SVR")
print(svrMAE)
print("MAE-MLR")
print(mlrMAE)

#print R-squared results
print("RT r-squared:")
print(ptCP.rsq[nrow(ptCP.rsq),1])
print("SVR r-squared:")
print(svrRsq)
print("MLR r-squared:")
print(mlrSummary$r.squared)
