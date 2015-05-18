# Read and clean the data
setwd("C:/Users/Sasha/Desktop/Rclass")
valid2<-read.table("pml-testing.csv",sep=",",header=TRUE,quote="\"")
data2<-read.table("pml-training.csv",sep=",",header=TRUE,quote="\"")
data<-data2[,colMeans(is.na(data2)) < 0.95]
valid<-valid2[,colMeans(is.na(data2)) < 0.95]

# partition the data onto training, testing and prevalidation sets
in1<-createDataPartition(y=data$classe,p=80/3200,list=FALSE)
prevalid<-data[in1,];
traintest<-data[-in1,];
in2<-createDataPartition(y=traintest$classe,p=0.7,list=FALSE)
train<-data[in2,];
test<-data[-in2,];

# try RPART
modfit1<-train(classe~.,method="rpart",data=train[,6:60])
print(modfit1$finalModel)
library(rattle)
fancyRpartPlot(modfit1$finalModel)
p1<-predict(modfit1,newdata=test[,6:59])
sum(p1==test[,60])
confusionMatrix(p1, test[,60])

# try GBM
modfit<-train(classe~.,method="gbm",data=train[,6:60])
p2<-predict(modfit,newdata=test[,6:59])
sum(p2==test[,60])

# capture confusiin matrices
p4train<-predict(modfit,newdata=train[,6:59])
p4test<-predict(modfit,newdata=test[,6:59])
p4prevalid<-predict(modfit,newdata=prevalid[,6:59])
confusionMatrix(p4train, train[,60])
confusionMatrix(p4test, test[,60])
confusionMatrix(p4prevalid, prevalid[,60])
