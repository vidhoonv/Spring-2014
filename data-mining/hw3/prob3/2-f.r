library('randomForest')
#load data
trainData = read.csv("Machine1.csv", header = TRUE)
testData = read.csv("Machinetest.csv", header = TRUE)

#seperate X and Y in train and test data
trainData_x = trainData[,-1];
trainData_y = trainData[,1];

testData_x = testData[,-1];
testData_y = testData[,1];


set.seed(100) 
myForest = randomForest(PRP	~ MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX, data =trainData, ntree=10)
forestTrainOut = predict(myForest,newdata=trainData)


print("Forest Train R squared:")
print(myForest$rsq[length(myForest$rsq)])

