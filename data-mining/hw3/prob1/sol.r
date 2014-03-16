#load data
ff_train = read.csv("forestfire-train.csv", header = TRUE)
ff_test = read.csv("forestfire-test.csv", header = TRUE)

#center and scale data
pffTrain = scale(ff_train,center=TRUE,scale=TRUE)
pffTest =  scale(ff_test,center=TRUE,scale=TRUE)


#suggested method to center and scale data
#returns data frame
# use this line to convert to matrix

#mat = data.matrix(frm)

for(i in 1:ncol(ff_train)) {
     ff_train[,i] = (ff_train[,i]-mean(ff_train[,i]))/sd(ff_train[,i]);
 	}


for(i in 1:ncol(ff_test)) {
     ff_test[,i] = (ff_test[,i]-mean(ff_test[,i]))/sd(ff_test[,i]);
 	}




pffTrain_x = pffTrain[,-9];
pffTrain_y = pffTrain[,9];

pffTest_x = pffTest[,-9];
pffTest_y = pffTest[,9];

#there is some deviation between two methods - defering analysis for later time
#compare.reals = function(a,b,tolerance=1e-5) {
# 	abs(a-b) < tolerance
#}

#print(compare.reals(pffTrain_x,ff_train));

#SGD

# steps
# 1) start with random weight matrix W of dimension 9*1
# for each iteration of SGD do
# 2) find error
# 3) find delta
# 4) update weight matrix
# 5) store RMSE
#


calcRMSE = function(err) {
	#error = x %*% w - y;
	meanError = mean((err^2));
	return(sqrt(meanError));
}

#add intercept column
#pffTrain_x = cbind(1, pffTrain_x)


#num of iterations and learning rate
epochs = 1500 #some large count for array bound

rate = double(3);

rate[1] = 0.0065
rate[2] = 0.00575
rate[3] = 0.00025

er = double(nrow(pffTrain));
erTestLm = double(nrow(pffTest));

#LM 

lm_fit = lm(area~-1+FFMC+DMC+DC+ISI+temp+RH+wind+rain,data.frame(pffTrain))
print(summary(lm_fit))

er =pffTrain[,"area"] - predict(lm_fit, data.frame(pffTrain))
print("LM train RMSE")
print(sqrt(mean(er^2)))

#LM TEST RMSE
erTestLm =pffTest[,"area"] - predict(lm_fit, data.frame(pffTest))
print("LM test RMSE")
print(sqrt(mean(erTestLm^2)))


for(rateIndex in 1:3) {


w = matrix(0.5,nrow=ncol(pffTrain_x));
gradient = matrix(0.0,nrow=ncol(pffTrain_x));


error = double(nrow(pffTrain_x));
testError = double(nrow(pffTest_x));
trainRMSE = double(epochs);
testRMSE = double(epochs);




########################
# GRADIENT DESCENT

#for(i in 1:epochs) {
# 
#	error = pffTrain_x %*% w - pffTrain_y; 
#	trainRMSE[i] = calcRMSE(error);
#	temp =  t(pffTrain_x) %*% error/length(pffTrain_y);
#	gradient = rate * temp;
#	w = w - gradient;
#
#	#test RMSE
#	testError = pffTest_x %*% w - pffTest_y;
#	testRMSE[i] = calcRMSE(testError) ;
#	print(testRMSE[i]);
#
#}

##################	


delta = 1.0000;
prev_w = w;
i=0;


while(delta > 0.0001 ) {

	i=i+1;

	for(j in 1:nrow(pffTrain_x)) {


		Xj =  as.matrix(pffTrain_x[j,]);
		
		error[j] =  pffTrain_y[j] -  ( t(Xj) %*% w ); 

		for(k in 1:ncol(pffTrain_x)) {	
			gradient[k,1] = rate[rateIndex] *  error[j] * Xj[k] / j ;
			w[k,1] = w[k,1] + gradient[k,1];
		}
		
		

	}	
	
	trainRMSE[i] = calcRMSE(error);

	#test RMSE
	testError = pffTest_x %*% w - pffTest_y;
	testRMSE[i] = calcRMSE(testError) ;
	


	delta = max(abs(w - prev_w));
	prev_w = w;
	
}		

print("Total iterations:")
print(i);

print("w found:")
print(w);

print("Train RMSE")
print(trainRMSE[i]);

print("Test RMSE")
print(testRMSE[i]);

filename = paste(getwd(),"/trainRMSE-plot-",rate[rateIndex],".png",sep='');
png(filename);
plot(trainRMSE[1:i],col="red",pch="+");
points(i,sqrt(mean(er^2)),col="blue",pch="*");
legend("topright",legend=c("SGD-trainRMSE","LM-trainRMSE"),pch=c("+","*"),col=c("red","blue"));
dev.off();


filename = paste(getwd(),"/testRMSE-plot-",rate[rateIndex],".png",sep='');
png(filename);
plot(testRMSE[1:i],col="red",pch="+");
points(i,sqrt(mean(erTestLm^2)),col="blue",pch="*");
legend("topright",legend=c("SGD-testRMSE","LM-testRMSE"),pch=c("+","*"),col=c("red","blue"));
dev.off();

}


