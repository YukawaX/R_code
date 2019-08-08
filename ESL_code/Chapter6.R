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