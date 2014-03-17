#load data
trainData = read.csv("Machine2.csv", header = TRUE)
testData = read.csv("Machinetest.csv", header = TRUE)

#load kernlab
library('rpart')
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
set.seed(105)

#SVR

mySvr =  ksvm(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=trainData,scaled=TRUE,type="eps-svr",kernel="vanilladot", kpar="automatic", class.weights=NULL,cache=100,cross=5);
svrTrainOut = predict(mySvr,newdata=trainData,type="response");

svrMSE = calcMSE(svrTrainOut,trainData_y);
svrMAE = calcMAE(svrTrainOut,trainData_y);
svrRsq = calcRsq(svrTrainOut,trainData_y)


svrTestOut = predict(mySvr,newdata=testData,type="response");

svrTestMSE = calcMSE(svrTestOut,testData_y);
svrTestMAE = calcMAE(svrTestOut,testData_y);
svrTestRsq = calcRsq(svrTestOut,testData_y)


#create regression tree with cv=5
myTree <- rpart(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX, data =trainData, control = rpart.control(xval = 5,cp=0.001))


ltCP = printcp(myTree)
ltCP.rsq <- 1-ltCP[,c(3,4)] 


pruneIndex = which.min(myTree$cptable[,"xerror"])
pruneCP = myTree$cptable[pruneIndex,"CP"]
prunedTree = prune(myTree,cp=pruneCP)

ptCP = printcp(prunedTree)
ptCP.rsq <- 1-ptCP[,c(3,4)] 

#testing pruned tree
ptTrainOut = predict(prunedTree,newdata=trainData);
ptMSE = calcMSE(ptTrainOut,trainData_y);
ptMAE = calcMAE(ptTrainOut,trainData_y);


ptTestOut = predict(prunedTree,newdata=testData);
ptTestMSE = calcMSE(ptTestOut,testData_y);
ptTestMAE = calcMAE(ptTestOut,testData_y);

#plot tree
png("RT-pruned-distored-data.png")
par(xpd = TRUE)
plot(prunedTree,compress=T,branch=0,uniform=T)
text(prunedTree,use.n=T,fancy=T,all=T)
dev.off()

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


trainpred = numeric()
trainpred = append(trainpred,svrTrainOut)
trainpred = append(trainpred,ptTrainOut)
trainpred = append(trainpred,mlrTrainOut)

traintarget = numeric()
traintarget = append(traintarget,trainData_y)
traintarget = append(traintarget,trainData_y)
traintarget = append(traintarget,trainData_y)


trainmod = rep("SVR",length(trainData_y))
trainmod = append(trainmod,rep("RT",length(trainData_y)))
trainmod = append(trainmod,rep("MLR",length(trainData_y)))


trainFrame = data.frame(Pred = trainpred, Target = traintarget, Model = trainmod)
trainPlot = ggplot(trainFrame, aes(x = trainFrame$Pred, y = trainFrame$Target, color=trainFrame$Model, shape=trainFrame$Model)) + theme_bw() +
 geom_point() + geom_abline(intercept=0, slope=1, color="grey") +
 scale_colour_brewer(palette="Set1") + xlab("Predicted PRP") +
  ylab("Target PRP") + theme(legend.position = "top") 

ggsave(filename="train-results-ver2.png", plot=trainPlot)

pred = numeric()
pred = append(pred,svrTestOut)
pred = append(pred,ptTestOut)
pred = append(pred,mlrTestOut)

target = numeric()
target = append(target,testData_y)
target = append(target,testData_y)
target = append(target,testData_y)


mod = rep("SVR",length(testData_y))
mod = append(mod,rep("RT",length(testData_y)))
mod = append(mod,rep("MLR",length(testData_y)))


testFrame = data.frame(Pred = pred, Target = target, Model = mod)
testPlot = ggplot(testFrame, aes(x = testFrame$Pred, y = testFrame$Target, color=testFrame$Model, shape=testFrame$Model)) + theme_bw() +
 geom_point() + geom_abline(intercept=0, slope=1, color="grey") +
 scale_colour_brewer(palette="Set1") + xlab("Predicted PRP") +
  ylab("Target PRP") + theme(legend.position = "top") 

ggsave(filename="test-results-ver2.png", plot=testPlot)
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

