# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression

fix(Boston)
names(Boston)
lm.fit=lm(medv~lstat)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)#这样访问更安全？
confint(lm.fit)#得到系数估计值的置信区间
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")#计算置信区间
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")#计算预测区间
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)#生成4幅诊断图
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
library(car)
vif(lm.fit)#计算方差膨胀因子
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms 交互项

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))#函数I是必要的，^在公式中具有特殊含义，是标准方法
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)#anova进一步量化二次拟合如何由于线性拟合，通过假设检验比较两个模型
# 零假设是两个模型同样出色，备择假设是全模型更优
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))#5阶多项式拟合，更简便的写法
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))#对数转换

# Qualitative Predictors 定性预测变量 汽车座椅数据

fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)#含有交互项
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)#返回虚拟变量的编码，此函数还有其他编码方式，可自行设置

# Writing Functions  编写函数


LoadLibraries=function(){#读取ISLR与MASS库的函数
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
