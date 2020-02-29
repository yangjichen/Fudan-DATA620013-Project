setwd('/Users/yangjichen/Desktop/课程/研一上/高等统计学习/project/')
load('digits.RData')

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

#(b)Initialize


em_alg = function(M = 2, digit = 8, step = 2){
  D = 400
  mixture_class = M
  train_3 = training.data[training_label==digit,]
  train_mix_component = sample(1:M, size = dim(train_3)[1],replace = TRUE)
  #initialize Gamma 5000*M
  Gamma = matrix(0, dim(train_3)[1], M)
  for (i in 1:dim(train_3)[1]){
    idx = train_mix_component[i]
    Gamma[i,idx] = 1
  }
  #mu M*D M*400
  mu = matrix(0, M, D)
  for (m in 1:M){
    for(j in 1:D){
      mu[m,j] = (1+Gamma[, m] %*%train_3[, j]) / (2+sum(Gamma[,m]))
    }
  }
  #pi_prior
  pi_prior = rep(0,M) 
  for (m in 1:M){
    pi_prior[m] = (1+sum(Gamma[, m])) / (M+sum(Gamma))
  }
  #Updata
  log_gamma = matrix(0,dim(train_3)[1],M)
  
  for(em_step in 1:step){
    for (i in 1:dim(train_3)[1]){
      for (m in 1:M){
        l = log(pi_prior[m]) + train_3[i,] %*% log(mu[m,]) + (1-train_3[i,]) %*% log(1-mu[m,])
        log_gamma[i,m] = l
      }
    }
    #update gamma
    for (i in 1:dim(train_3)[1]){
      for (m in 1:M){
        Gamma[i,m] = exp(log_gamma[i,m]) / sum(exp(log_gamma[i,]))
      }
    }
    
    #updata mu
    for (m in 1:M){
      for(j in 1:D){
        mu[m,j] = (1+Gamma[, m] %*% train_3[, j]) / (2+sum(Gamma[,m]))
      }
    }
    #update pi_prior
    for (m in 1:M){
      pi_prior[m] = (1+sum(Gamma[, m])) / (M+sum(Gamma))
    }
  }
  output = list(mu = mu, pi_prior = pi_prior)
  return(output)
}

para = em_alg(M=3,digit=1,step = 100)

p1 = matrix(para$mu[1,],20,20)
p2 = matrix(para$mu[2,],20,20)
p3 = matrix(para$mu[3,],20,20)

image(t(1-p1)[,20:1],col=gray(seq(0, 1, length.out=256)),
      axes=FALSE, asp=1)
image(t(1-p2)[,20:1],col=gray(seq(0, 1, length.out=256)),
      axes=FALSE, asp=1)
image(t(1-p3)[,20:1],col=gray(seq(0, 1, length.out=256)),
      axes=FALSE, asp=1)

pred = function(x,mu,pi_prior,M){
  log_p = rep(0,M)
  p = rep(0,M)
  for (m in 1:M){
    log_p[m] = x %*% log(mu[m,]) + (1-x) %*% log(1-mu[m,])
  }
  p = exp(log_p)
  #print(p)
  finalp = pi_prior %*% p
  return(finalp)
}

#build model
digit = c(1,8)
parameter1 = em_alg(M=3,digit[1],step = 100)
parameter2 = em_alg(M=3,digit[2],step = 100)

test1 = test.data[test_label==1,]
test2 = test.data[test_label==8,]
test = rbind(test1,test2)
testlabel = c(test_label[test_label==1],test_label[test_label==8])
predd = matrix(0, 2, dim(test)[1])

for (iter in 1:dim(test)[1]){
  p1 = pred(test[iter,],parameter1$mu,parameter1$pi_prior,3)
  p2 = pred(test[iter,],parameter2$mu,parameter2$pi_prior,3)
  predd[,iter] = c(p1,p2)
}
compare_idx = predd[1,]-predd[2,]>=0

pred_value = rep(0,dim(test)[1])
pred_value[compare_idx] = digit[1]
pred_value[!compare_idx] = digit[2]

#测试准确率
acc = function(x_pred,x_label){
  ac = sum(as.numeric(x_pred==x_label))/length(x_pred)
  return(ac)
}

presision= acc(pred_value,testlabel)
