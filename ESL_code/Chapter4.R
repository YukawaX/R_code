library(openxlsx)
library(ggplot2)
library(MASS)
library(pracma)
train<-read.xlsx('train.xlsx')
train$y<-as.factor(train$y)
train_samples<-length(train$y)
test<-read.xlsx('test.xlsx')
test$y<-as.factor(test$y)
test_samples<-length(test$y)
classes = 11   #y共有11种分类
samples = 48   #每种分类有48个样本
sigma = list()#各类协方差矩阵的估计
inverse_sigma = list()#协方差矩阵的逆
mu = list()#各类均值向量的估计
pi = table(train$y)/length(train$y)
for(i in 1:classes){
  dat = train[train$y==i,]#取出y分类为i的数据
  sig<-cov(dat[,-1])
  sigma[[i]]<-sig
  inverse_sigma[[i]]<-solve(sig)
  mu[[i]]<-colMeans(dat[,-1])
}
#LDA情形下sigma矩阵的估计 common covariance matrix
Sigma = 0
for(i in 1:classes){
  Sigma = Sigma + sigma[[i]]
}
Sigma = Sigma/11
# qda_list=list()
# for(i in 1:classes){
# 
#   score_func<-function(x){
#     x=as.numeric(x)
#     score = -0.5*log(abs(det(sigma[[i]])))-0.5*t(x-mu[[i]])%*%inverse_sigma[[i]]%*%(x-mu[[i]])+log(pi[i])
#     return(score)
#   }
#   qda_list[[i]]<-score_func
# }

qda_classifier<-function(x){
  x = as.numeric(x)
  scores=c()#各类别上的打分
  for(i in 1:classes){
    score_i = -0.5*log(abs(det(sigma[[i]])))-0.5*t(x-mu[[i]])%*%inverse_sigma[[i]]%*%(x-mu[[i]])+log(pi[i])
    scores=c(scores,score_i)
  }
  label = which.max(scores)
  return(label)
}
result<-apply(test[,-1],1,qda_classifier)

regularized_qda_classifier<-function(x,alpha){
  x = as.numeric(x)
  scores = c()
  for(i in 1:classes){
    sigma_i_regularised<-alpha*sigma[[i]]+(1-alpha)*Sigma
    inverse_sigma_i_regularised<-solve(sigma_i_regularised)
    score_i = -0.5*log(abs(det(sigma_i_regularised)))-0.5*t(x-mu[[i]])%*%inverse_sigma_i_regularised%*%(x-mu[[i]])+log(pi[i])
    scores = c(scores,score_i)
  }
  label = which.max(scores)
  return(label)
}
##choose hyperparameter alpha
train_set_result=c()
test_set_result=c()
N = 50
for(i in 25:N){
  alpha = i/N
  train_result<-apply(train[,-1],1,regularized_qda_classifier,alpha = alpha)
  train_set_result<-c(train_set_result,1-sum(diag(table(train$y,train_result)))/train_samples)
  test_result<-apply(test[,-1],1,regularized_qda_classifier,alpha = alpha)
  test_set_result<-c(test_set_result,1-sum(diag(table(test$y,test_result)))/test_samples)
  print(i)
}
Result<-data.frame(accuracy_trainset = train_set_result,accuracy_testset = test_set_result,alpha =25:N/N)
ggplot(Result,aes(x=alpha,aes(x=alpha)))+geom_line(aes(y=accuracy_trainset),data = Result)+geom_line(aes(y=accuracy_testset),data = Result)


#Reduced-Rank Linear Discriminant Analysis
M = as.matrix(aggregate(train[,-1],list(labels=train$y),mean)[,-1])
W = Sigma  #estimate for common covariance matrix
decomp_W = eigen(W)# eigenvalue decomposition of W
W_inverse_squareroot = decomp_W$vectors%*%(diag(1./sqrt(decomp_W$values)))%*%t(decomp_W$vectors)
M_star = M%*%W_inverse_squareroot
B_star = cov(M_star)
decomp_B = eigen(B_star) 
Z = as.matrix(train[,-1])%*%W_inverse_squareroot%*%(decomp_B$vectors) #transform train dataset
Z = as.data.frame(Z)
Z$label<-train$y
M_trans = M%*%W_inverse_squareroot%*%(decomp_B$vectors)
M_trans = as.data.frame(M_trans)
M_trans$label <-as.factor(1:11)
ggplot(Z,aes(x=V1,y=V2,color=label))+geom_point()+geom_point(mapping=aes(x=V1,y=V2),data = M_trans,size = 3,color = "BLACK")

train_trans = Z
test_trans = as.matrix(test[,-1])%*%W_inverse_squareroot%*%(decomp_B$vectors)#transform test dataset
#估计变换后的协方差矩阵
# Sigma_trans = matrix(data = 0 ,ncol = 10 ,nrow = 10)
# for(i in 1:classes){
#   Sigma_trans = Sigma_trans + cov(Z[Z$label==i,-11])
# }
# Sigma_trans = Sigma_trans/classes
LDA_classifier<-function(x,coordinates){
  x = as.numeric(x)
  scores=c()#各类别上的打分
  for(i in 1:classes){
    point = x[1:coordinates]#样本在前coordinates个坐标的坐标值
    centroids_i = M_trans[i,1:coordinates]
    score_i = -0.5*sum((point-centroids_i)^2)+log(pi[i])
    scores=c(scores,score_i)
  }
  label = which.max(scores)
  return(label)
}
trainset_result<-c()
testset_result<-c()
for (i in 1:10){
  train_classification<-apply(Z[,-11],1,LDA_classifier,coordinates = i)
  trainset_result<-c(trainset_result,1-sum(diag(table(Z$label,train_classification)))/train_samples)
  test_classification<-apply(test_trans,1,LDA_classifier,coordinates = i)
  testset_result<-c(testset_result,1-sum(diag(table(test$y,test_classification)))/test_samples)
  print(i)
}
result<-data.frame(coordinates=1:10,train_result=trainset_result,test_result=testset_result)
ggplot(result,aes(x=coordinates))+geom_line(aes(y=train_result))+geom_line(aes(y=test_result))


