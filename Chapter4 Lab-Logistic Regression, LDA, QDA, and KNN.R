# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

library(ISLR)
names(Smarket)
fix(Smarket)#lag1~lag5表示5个交易日的股票投资回报率
# [1] "Year"      "Lag1"      "Lag2"      "Lag3"      "Lag4"     
# [6] "Lag5"      "Volume"    "Today"     "Direction"
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)#direction是定性的，所以出错
cor(Smarket[,-9])#当前回报率与之前回报率相关性几乎为0
attach(Smarket)
plot(Volume)#volume随时间不断增长

# Logistic Regression

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
#glm函数用于拟合广义线性模型，family=binomial
summary(glm.fits)
coef(glm.fits)#用于输出拟合模型系数
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response")#type="response"表示明确输出概率
glm.probs[1:10]
contrasts(Direction)#创建哑变量
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)#混淆矩阵
(507+145)/1250
mean(glm.pred==Direction)#表示正确预测的比例
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit

plot(lda.fit)#线性判别图像
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
data.frame(lda.pred$posterior,lda.pred$class)
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)
max(lda.pred$posterior[,1])
# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit#没有线性判别系数，因为QDA是二次分类器
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
# 0.5992063，结果比较精确

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)#设置随机种子保证结果的可复制性
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data
# 大篷车保险数据的一个应用
dim(Caravan)
fix(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15
glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)
