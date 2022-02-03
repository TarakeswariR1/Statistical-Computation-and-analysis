#Question 1

set.seed(195021)
x=rexp(n=50,rate=2)
n=50
neg_log_like_lambda <- function(lambda, x){
  ll<-length(x) * log(lambda) - lambda*length(x)*mean(x)
  return(-ll)
}

log_like_lambda <- function(lambda){
  l<-n * log(lambda) - lambda*n*mean(x)
  return(l)
}

fm=optimize(f = neg_log_like_lambda, x=x, interval = c(0,100), maximum =  F)
estimates=1/mean(x)
library(numDeriv)
H=hessian(neg_log_like_lambda,x=fm$minimum)
fisher_info<-solve(-H)
prop_sigma<-sqrt(diag(fisher_info))
upper<-fm$minimum+1.96*prop_sigma
lower<-fm$minimum-1.96*prop_sigma


#Question 2
DATA=read.table('https://raw.githubusercontent.com/gdlc/STAT_COMP/master/DATA/goutData.txt',header=TRUE)
DATA$y=ifelse(is.na(DATA$gout),NA,ifelse(DATA$gout=='Y',1,0))
X=as.matrix(model.matrix(~race+sex+age,data=DATA))
GLM=glm(y~X,family=binomial(link=logit),data=DATA) 
var=vcov(GLM)
race=c(1, 1, 0, 0)
sex=c(1, 0, 1, 0)
age=c(55, 55, 55, 55)
newdata=data.frame(race, sex, age)
X1=as.matrix(model.matrix(~race+sex+age,data=newdata))
log_odds<-function(A){
  exp(A)/(1+exp(A))
}
coef1=GLM$coefficients
theta=log_odds(eta)

SE=sqrt(diag(X1 %*% var %*% t(X1)))

CI_lower=log_odds(eta-(1.96*SE))
CI_upper=log_odds(eta+(1.96*SE))


#Question 3
B=1000
SU=DATA$su
Age=DATA$age
SEX=DATA$sex
RACE=DATA$race
n1=nrow(DATA)
SE1=rep(NA, 4)
CI_l=rep(NA, 4)
CI_u=rep(NA, 4)
correlation=rep(NA, B)
for (j in 1:4){
  predProb=rep(NA,n)
  CI.Bootstrap=NULL
  means=rep(NA, n)
for(i in 1:B){
  tmp=DATA[sample(1:n1, size = n1, replace=T), ]
  model1=glm(y~sex+race+age, family=binomial(link=logit), data=tmp)
  LP=X1[j,]%*%coef(model1)
  d[i,]=exp(LP)/(1+exp(LP))
  tmp1=sample(1:n1, size = n1, replace=T)
  #bootstrap_sample=X[tmp1]
  #means[i]=mean(bootstrap_sample)
  #correlation[i]=cor(DATA$y[tmp1], X[tmp1])
  
}
CI.Bootstrap=quantile(predProb, prob=c(.025,.975))
SE.Bootstrap=sd(correlation)
print(SE.Bootstrap)
SE1[j]=SE.Bootstrap
CI_l[j]=CI.Bootstrap[1]
CI_u[j]=CI.Bootstrap[2]
print(CI.Bootstrap)
}
se2=matrix(, 1000, 4)
for(i in 1:1000){
  tmp2=DATA[sample(1:n1, size=n1, replace=T),]
  model1=glm(y~sex+race+age, family=binomial(link=logit), data=tmp2)
  V=vcov(model1)
  SE_e=sqrt(diag(X1 %*% V %*% t(X1)))
  se2[i,1]=SE_e[1]
  se2[i,2]=SE_e[2]
  se2[i,3]=SE_e[3]
  se2[i,4]=SE_e[4]
}
se=sd(se2)







