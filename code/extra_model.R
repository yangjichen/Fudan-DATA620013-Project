setwd('/Users/yangjichen/Desktop/课程/研一上/高等统计学习/project/')
load('digits.RData')

acc = function(x_pred,x_label){
  ac = sum(as.numeric(x_pred==x_label))/length(x_pred)
  return(ac)
}
image(t(1-training.data[2,1,,])[,20:1],
      col=gray(seq(0, 1, length.out=256)),
      axes=FALSE, asp=1)

# Number of classes
num_class <- dim(training.data)[1]
# Number of training data per class
num_training <- dim(training.data)[2]
# Dimension of each training image (rowsxcolumns)
d <- prod(dim(training.data)[3:4])
# Number of test data
num_test <- dim(test.data)[2]
# Reshape training data to 2-dim matrix
#每一行是一个样本
dim(training.data) <- c(num_class * num_training, d)
# Same for test.
dim(test.data) <- c(num_class * num_test, d)
# Labels of training data.
training_label <- rep(0:9, num_training)
# Labels of test data
test_label <- rep(0:9, num_test)

training.data = data.matrix(training.data)*1
test.data = data.matrix(test.data)*1

#xgboost
library(xgboost)
model <- xgboost(data =  training.data, label= training_label,nrounds = 100, nthread = 64, nfold = 5, 
             max_depth = 10, eta = 0.1,objective = "multi:softmax",num_class = 10)
preds = predict(model,test.data)
acc_test = acc(preds,test_label)
acc_test

#knn
library(class)
pred<-knn(train=training.data,test=test.data,cl=training_label,k=10)
acc_test = acc(pred,test_label)
acc_test

#softmax
library(softmaxreg)
model  = softmaxReg(training.data, training_label, hidden = c(), funName = 'sigmoid', maxit =100 ,
                    rang = 0.1, type = "class", algorithm = "adagrad", rate = 0.02, batch = 1000)
pred = predict(model,test.data )
acc_test = acc(pred-1,test_label)
acc_test

