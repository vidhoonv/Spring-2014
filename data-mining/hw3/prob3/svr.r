#load data
trainData = read.csv("Machine1.csv", header = TRUE)
testData = read.csv("Machinetest.csv", header = TRUE)

#seperate X and Y in train and test data
trainData_x = trainData[,-1];
trainData_y = trainData[,1];

testData_x = testData[,-1];
testData_y = testData[,1];


#load kernlab



print(svrTestOut);

#plot(testOut);
#points(testData_y,col='red');

