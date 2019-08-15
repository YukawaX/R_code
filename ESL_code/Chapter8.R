###MCMC Gibbs Sampling
pi_hat<-0.546
sigma_1<-0.87
sigma_2<-0.77
y<-c(-0.39,0.12,0.94,1.67,1.76,2.44,3.72,4.28,4.92,5.53,0.06,0.48,1.01,1.68,1.80,3.25,4.12,4.60,5.28,6.22)
theta_init<-c(mean(y),mean(y))
iter<-1000 #iteration times
theta<-matrix(theta_init,nrow=1)
delta_proportion<-c()#在各次迭代中，delta=1所占比例

for(i in 1:iter){
  delta=c()
  for(j in 1:length(y)){
    Pr<-pi_hat*dnorm(y[j],mean = theta[i,2],sd = sqrt(sigma_2))/((1-pi_hat)*dnorm(y[j],mean=theta[i,1],sd=sqrt(sigma_1))+pi_hat*dnorm(y[j],mean = theta[i,2],sd = sqrt(sigma_2)))
    delta_j<-rbinom(n=1,size=1,prob = Pr)
    delta=c(delta,delta_j)
  }
  mu_1_hat = weighted.mean(y,1-delta)
  mu_2_hat = weighted.mean(y,delta)
  mu_1_i<-rnorm(1,mean=mu_1_hat,sd=sqrt(sigma_1))
  mu_2_i<-rnorm(1,mean=mu_2_hat,sd=sqrt(sigma_2))
  theta = rbind(theta,c(mu_1_i,mu_2_i))
  delta_proportion=c(delta_proportion,mean(delta))
}

library(ggplot2)
gibbs_iteration<-data.frame(iteration=c(0:iter,0:iter),mean_parameters=as.vector(theta),delta=as.factor(c(1+0*(0:iter),2+0*(0:iter))))
ggplot(gibbs_iteration,aes(x=iteration,y=mean_parameters,color=delta))+geom_line()+geom_hline(aes(yintercept=4.62),linetype="dashed")+geom_hline(aes(yintercept=1.06),linetype="dashed")


###8.7.1Trees with Simulated Data








