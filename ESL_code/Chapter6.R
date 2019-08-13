#Ex6.12
Train<-ElemStatLearn::zip.train
Test<-ElemStatLearn::zip.test
Train_y<-Train[,1]
Test_y<-Test[,1]
Train_x<-Train[,-1]
Test_x<-Test[,-1]

#Gaussian Kernel 除去了常数项减小数值误差
gau_kernel<-function(x,x0,lambda){
  p<-length(x0)
  f_hat<-exp(-sum((x-x0)^2)/(2*lambda^2))
  return(f_hat)
}

#Local_LDA
Local_LDA<-function(x,Train_y,Train_x,classes=10){#class_label = 0,1,2,...,
  N = length(Train_y)
  dimen<-length(Train_x[1,])
  pi_hat<-table(as.factor(y))/N
  mu<-c()
  Sigma<-matrix(0,nrow = dimen,ncol=dimen)
  for(i in 0:(classes-1)){
    Train_x_i = Train_x[which(Train_y==i),]  #选出label为i的数据
    weights <- apply(Train_x_i,MARGIN = 1,FUN=gau_kernel,x0=x,lambda = lambda)
    weighted_Train_x_i<-(Train_x_i*matrix(rep(weights,ncol(Train_x_i)),nrow = nrow(Train_x_i)))/sum(weights)
    mu_i = colSums(weighted_Train_x_i)
    mu = c(mu,mu_i)
    Sigma = Sigma + 
  }
}

######   "aa" coded as 0; "ao"coded as 1
phoneme<-ElemStatLearn::phoneme
library(splines)
Train<-phoneme[grep("train",phoneme$speaker),-258]
Test<-phoneme[grep("test",phoneme$speaker),-258]
N_train=nrow(Train)
N_test=nrow(Test)
aa_Train<-Train[Train$g=="aa",]
ao_Train<-Train[Train$g=="ao",]
aa_Test<-Test[Test$g=="aa",]
ao_Test<-Test[Test$g=="ao",]
Train_set<-rbind(aa_Train,ao_Train)
Test_set<-rbind(aa_Test,ao_Test)
Train_set$g=as.factor(as.character(Train_set$g))#将原有levels数调整为现在的两个level：“aa”和“ao”
Test_set$g=as.factor(as.character(Test_set$g))
Nums<-c(2,4,8,16,32,64,128,256)##Number of basis functions

Train_loglik=c()
Test_loglik=c()
AIC=c()
logit_loglikelihood<-function(sample,beta){#logistic回归下计算单样本的对数似然值 sample=(x,response)
    xbeta<-beta%*%as.numeric(c(1,sample[1:length(sample)-1]))#x与beta乘积
   if(sample[length(sample)]=="aa"){
    return(-log(1+exp(xbeta)))
  }
  else{
    return(xbeta-log(1+exp(xbeta)))
  }
}

for(i in 1:8){
  print(i)#输出循环进行次数
  num<-Nums[i]
  H<-bs(1:256,df = num)
  x_train_trans = as.matrix(Train_set[,-257])%*%H # linear transformation for x using b-splines
  Train_trans=as.data.frame(x_train_trans)
  Train_trans$g=Train_set$g
  x_test_trans = as.matrix(Test_set[,-257])%*%H
  Test_trans=as.data.frame(x_test_trans)
  Test_trans$g=Test_set$g
  glm_model<-glm(g~.,family = binomial,data=Train_trans)
  Train_loglik=c(Train_loglik,-2/N_train*logLik(glm_model))
  AIC=c(AIC,1/N_train*AIC(glm_model))
  Test_loglik_samples<-apply(Test_trans,MARGIN =1,FUN =logit_loglikelihood,beta=glm_model$coefficients)
  Test_loglik=c(Test_loglik,-2*mean(Test_loglik_samples))
}
result<-data.frame(num_of_splines = nums,Train_loglik=Train_loglik,Test_loglik=Test_loglik,AIC=AIC)
library(ggplot2)
ggplot(result,aes(x=num_of_splines))+geom_line(aes(y=Train_loglik))+geom_line(aes(y=Test_loglik))+geom_line(aes(y=AIC))+scale_x_log10()



