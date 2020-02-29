setwd('/Users/yangjichen/Desktop/课程/研一上/高等统计学习/project/')
load('digits.RData')
library(ggplot2)
pred_fun = function(x){
  pred = rep(0,10)
  for (i in 1:10){
    pred[i] = as.numeric(x) %*% beta[,i] + beta0[i]
  }
  final = which.max(pred)-1
  return(final)
}

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

training.data = as.data.frame(data.matrix(training.data))
test.data = as.data.frame(data.matrix(test.data))

#划分数据集
library(caret)
folds = createFolds(y=training_label,k=5)

lambda_list = seq(0.01, 0.4,by = 0.01)
pres_list = c()

for(lambda in lambda_list){
  #k-fold cross validation
  acc_list = rep(0,5)
  for (j in 1:5){
    fold_train = training.data[-folds[[j]],]
    fold_train_label = training_label[-folds[[j]]]
    fold_test = training.data[folds[[j]],]
    fold_test_label = training_label[folds[[j]]]
    #mu
    mu  = matrix(0,10,400)
    for (i in 0:9){
      idx = fold_train_label==i
      mu[i+1,] = apply(fold_train[idx,],2,mean)
    }
    #sigma
    sigma = matrix(0,400,400)
    for (i in 0:9){
      idx = fold_train_label==i
      sigma = sigma + (dim(fold_train)[1]/10)*var(fold_train[idx,])
    }
    sigma = sigma/dim(fold_train)[1]
    sigma = (1-lambda)*sigma + lambda/4*diag(400)
    
    #pi_k
    prior = as.numeric(table(fold_train_label))/length(fold_train_label)
    beta = solve(sigma) %*% t(mu)
    
    beta0=c()
    for (i in 1:10){
      beta0k = log(prior[i])-0.5* mu[i,] %*% solve(sigma) %*% mu[i,]
      beta0 = append(beta0, beta0k)
    }
    pred_test = apply(fold_test,1,pred_fun)
    acc_test = acc(pred_test,fold_test_label)
    
    acc_list[j] = acc_test
  }
  print(acc_list)
  print(paste('lam =',lambda,',acc is',mean(acc_list)))
  
  pres_list = append(pres_list,mean(acc_list))
}
tuning = list(lambda_list,pres_list)
print(tuning)
qplot(lambda_list,pres_list)

# Find best lambda
final_lambda = lambda_list[which.max(pres_list)]
# learning parameter
mu  = matrix(0,10,400)
for (i in 0:9){
  idx = training_label==i
  mu[i+1,] = apply(training.data[idx,],2,mean)
}
#sigma
sigma = matrix(0,400,400)
for (i in 0:9){
  idx = training_label==i
  sigma = sigma + (dim(training.data)[1]/10)*var(training.data[idx,])
}
sigma = sigma/dim(training.data)[1]
sigma = (1-final_lambda)*sigma + final_lambda/4*diag(400)

#pi_k
prior = as.numeric(table(training_label))/length(training_label)
beta = solve(sigma) %*% t(mu)

beta0=c()
for (i in 1:10){
  beta0k = log(prior[i])-0.5* mu[i,] %*% solve(sigma) %*% mu[i,]
  beta0 = append(beta0, beta0k)
}
# Here for predict and calculate accuracy rate
pred_test = apply(test.data,1,pred_fun)
acc_test = acc(pred_test,test_label)
print(paste('The accuracy in test set is', acc_test))
print(paste('The best lambda is', final_lambda))
