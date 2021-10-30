# Chapter 2 Lab: Introduction to R

# Basic Commands

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
ls()
rm(x,y)
ls()
rm(list=ls())
?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x=matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)#���������ȫ��ͬ��һ�������
# �����趨��������ӣ�һ���ض������ӿ��Բ���һ���ض���α������У������������ҪĿ�ģ�
# �������ģ���ܹ����ظ����֣���Ϊ�ܶ�ʱ��������Ҫȡ�����������δ�������һ�ε�ʱ��
# ����Ͳ�һ���ˣ������Ҫ�ظ�����ͬ����ģ�����Ļ����Ϳ�����set.seed()���ڵ��Գ���
# ������չʾ��ʱ�򣬽���Ŀ��ظ����Ǻ���Ҫ�ģ��������������Ҳ�ͺ��б�Ҫ��
rnorm(50)
set.seed(3)#����Ĳ�����������������
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics

x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
pdf("Figure.pdf")#��jpeg������������jpegͼ
plot(x,y,col="green")
dev.off()
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
# contour()����һ�����ȸ���ͼ����ʾ��ά����
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)#?contour���ո����÷��������������
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)#��ͼ���ܲ���һ������ɫ��ͼ
persp(x,y,fa)#����һ����άͼ
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)#����theta��phi���ƹۿ��ĽǶ�
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)
library(rgl)
open3d()
surface3d(x,y,fa,back="lines",color=terrain.colors(x^2))#��������
# Indexing Data
dev.off
A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)

# Loading Data

Auto=read.table("Auto.data")#������ݿ��ʽ
fix(Auto)#���ӱ����������
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)
Auto=read.csv("customer.csv",header=T,na.strings="?")#��ָ���ı����ȱʧ���
fix(Auto)#ͦ���õ�����
dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto)#���޳�ȱʧֵ���ڵ���
dim(Auto)
names(Auto)

# Additional Graphical and Numerical Summaries

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)#�������ı���ת��Ϊ���Ա���
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
#���������x���ϵı����Ƕ��Եģ�����ͼ��boxplot�����Զ�ͨ��plot��������������û�У�������֤
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)#����ָ����ʾĳ������ֵ
summary(Auto)
summary(mpg)

# savehistory()��loadhistory()