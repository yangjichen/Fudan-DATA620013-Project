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
    p1 = matrix(mu[1,],20,20)
    p2 = matrix(mu[2,],20,20)
    p3 = matrix(mu[3,],20,20)
    p4 = matrix(mu[4,],20,20)
    p5 = matrix(mu[5,],20,20)
    
    image(t(1-p1)[,20:1],col=gray(seq(0, 1, length.out=256)),
           asp=1)
    image(t(1-p2)[,20:1],col=gray(seq(0, 1, length.out=256)),
           asp=1)
    image(t(1-p3)[,20:1],col=gray(seq(0, 1, length.out=256)),
          asp=1)
    image(t(1-p4)[,20:1],col=gray(seq(0, 1, length.out=256)),
          asp=1)
    image(t(1-p5)[,20:1],col=gray(seq(0, 1, length.out=256)),
          asp=1)

    #update pi_prior
    for (m in 1:M){
      pi_prior[m] = (1+sum(Gamma[, m])) / (M+sum(Gamma))
    }
  }

  output = list(mu = mu, pi_prior = pi_prior)
  return(output)
}
par(mfrow=c(5, 5))
para = em_alg(M=5,digit=3,step = 5)



