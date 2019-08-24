##Chapter9  Generalized addictive model
##Predicting Email Spam
###ElemStatLearn::spam
library(ElemStatLearn)
response=c()
response[spam$spam=="spam"]=1
response[spam$spam=="email"]=0
inputs<-spam[,-58]
inputs_logtrans<-log(inputs+0.1)#+0.1防止log0情况
library(mgcv)##gam函数位于该包中
dataset<-data.frame(response=as.factor(response),inputs_logtrans)
samples<-nrow(dataset)
ind<-sample(1:4601,size=3065)
Train_set<-dataset[ind,]
Test_set<-dataset[-ind,]

library(splines)
library(stringr)
##Train model #调包 
pattern=str_replace(" bs(a,df=4) ",pattern="a",replacement = colnames(Train_set)[-1])
form<-paste("response ~",paste(pattern,collapse = "+"))
gam_model<-gam(formula(form),family = binomial(),data = Train_set)
anova(gam_model)
summary(gam_model)
#predict and result  ##total_error_rate=0.05013021
p_hat<-predict(gam_model,Test_set[,-1],type="response")
predicted_y<-c()
predicted_y[p_hat>0.5]=1
predicted_y[p_hat<=0.5]=0
result=table(Test_set$response,predicted_y)

##logistic   ##total_error_rate=0.05078125
new_form = paste("response ~ ",paste(colnames(Train_set)[-1],collapse = "+"))
glm_model<-glm(formula(new_form),family = binomial(),data=Train_set)
p_hat<-predict(glm_model,Test_set[,-1],type="response")
predicted_y<-c()
predicted_y[p_hat>0.5]=1
predicted_y[p_hat<=0.5]=0
result=table(Test_set$response,predicted_y)

##backfitting for linear models
##data generation
dat=prostate[,c("lcavol","lweight","svi","lpsa")]
x=dat[,c("lcavol","lweight","svi")]
y=dat[,"lpsa"]
x_means=colMeans(x)
x_centered = t(t(x)-x_means)
y_centered = y-mean(y)
beta=0*(1:3)
gamma=0*(1:3)
tol = 10e-8
iter=0
while(TRUE){
  iter = iter +1
  print(iter)
for(j in 1:3){
  y_j = y_centered -x_centered[,-j]%*%beta[-j]
  gamma[j]=lm(y_j~x_centered[,j])$coefficients[2]#sum(x_centered[,j]*y_j)/(sum((x_centered)^2))
}
  if(max(abs(beta-gamma))<tol){
    break
  }
  else{
    beta = gamma
  }
}
intercept<-mean(y)-mean(as.matrix(x)%*%gamma)

###Local Scoring Algorithm for the Additive Logistic Regression Model

##CART
library(rpart)
library(maptree)
library(rpart.plot)
ClassifiTree_model<-rpart(spam~.,data=spam,parms = list(split="information"))
draw.tree(ClassifiTree_model)
rpart.plot(ClassifiTree_model)
printcp(ClassifiTree_model)
plotcp(ClassifiTree_model)


