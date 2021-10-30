# Chaper 5 Lab: Cross-Validation and the Bootstrap

# The Validation Set Approach

library(ISLR)
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)#poly函数估计二次
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)#和三次
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation：留一交叉验证

glm.fit=glm(mpg~horsepower,data=Auto)#没有设定family就是线性回归
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot)#其中有cv.glm函数
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)#glm与cv.glm可以一起使用
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error# 24.23151 19.24821 19.33498 19.42443 19.03321

# k-Fold Cross-Validation：k折交叉验证

set.seed(17)
cv.error.10=rep(0,10)
cv.error.11=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  cv.error.11[i]=cv.glm(Auto,glm.fit,K=10)$delta[2]
}
cv.error.10#：24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201 18.95140 19.50196
cv.error.11#24.22677 19.25471 19.22640 19.49643 19.15860 18.74707 18.99148 19.23236 19.06929 19.52286
#第一个是标准k折CV估计，第二个是偏差校正后的结果，在这里只有细微差别
#速度快了很多

# The Bootstrap#自助法
  # 两个步骤：第一，创建一个计算感兴趣的统计量的函数
  # 第二，用boot库中的boot()有放回地抽取观测来执行自助法
  
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)#[1] 0.5758321
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))#[1] 0.5963833
boot(Portfolio,alpha.fn,R=1000)#可以多次自动运行这个命令
# original        bias    std. error
# t1* 0.5758321 -7.315422e-05  0.08861826

# Estimating the Accuracy of a Linear Regression Model
#估计线性回归模型的精度
boot.fn=function(data,index)#函数只有一行时不需要{}
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
