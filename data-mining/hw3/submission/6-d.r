library(nlme)
library(ggplot2)

#calcMSE method
calcMSE = function(predictedY,target) {
	error = predictedY-target
	return(mean(error^2))
}


#load data
myData = read.csv("oxboys.csv", header = TRUE)

#split train and test data
trainData = subset(myData, year <= 6)
testData = subset(myData, year > 6)

model = lm(height ~ year,data=myData);
png('lm-plot-ver2.png')
plot(myData[,'year'],myData[,'height'])
abline(model)
dev.off()


fp = ggplot(myData, aes(x=myData[,'year'], y=myData[,'height'], group=myData[,'id'])) + stat_smooth(method="lm")
fplot = fp + geom_point() 
ggsave(filename="lm-by-id-ver2.png", plot=fplot)


globalModel = lm(height ~ year,data=trainData);
globalTestOut = predict(globalModel,newdata=testData);
print('MSE for global model:')
print(calcMSE(globalTestOut,testData$height))

for( key in unique(trainData$id)){
	print(paste('MSE for student: ',key))
	
	#take data for each id
	studentTrain <- subset(trainData, id==key)
	studentTest <- subset(testData, id==key)
	localModel = lm(height ~ year, data=studentTrain)
	localTestOut = predict(localModel,newdata=studentTest)
	print(calcMSE(localTestOut,studentTest$height))

}

multiLevelModel =  lme(height~year, data=trainData, random=list(id=pdDiag(~year)))
multiLevelTestOut = predict(multiLevelModel, newdata=testData, level=1)
print('MSE for multi level model:')
print(calcMSE(multiLevelTestOut,testData$height))



