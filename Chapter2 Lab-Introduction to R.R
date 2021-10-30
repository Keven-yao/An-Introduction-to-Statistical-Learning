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
set.seed(1303)#代码产生完全相同的一组随机数
# 用于设定随机数种子，一个特定的种子可以产生一个特定的伪随机序列，这个函数的主要目的，
# 是让你的模拟能够可重复出现，因为很多时候我们需要取随机数，但这段代码再跑一次的时候，
# 结果就不一样了，如果需要重复出现同样的模拟结果的话，就可以用set.seed()。在调试程序
# 或者做展示的时候，结果的可重复性是很重要的，所以随机数种子也就很有必要。
rnorm(50)
set.seed(3)#里面的参数可以是任意整数
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
pdf("Figure.pdf")#用jpeg（）函数绘制jpeg图
plot(x,y,col="green")
dev.off()
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
# contour()产生一个个等高线图，表示三维数据
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)#?contour掌握更多用法，调整函数输出
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)#热图，能产生一个有颜色的图
persp(x,y,fa)#产生一个三维图
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)#参数theta与phi控制观看的角度
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)
library(rgl)
open3d()
surface3d(x,y,fa,back="lines",color=terrain.colors(x^2))#绘制曲面
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

Auto=read.table("Auto.data")#输出数据框格式
fix(Auto)#电子表格浏览数据
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)
Auto=read.csv("customer.csv",header=T,na.strings="?")#用指定的标号作缺失标记
fix(Auto)#挺好用的哇塞
dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto)#简单剔除缺失值所在的行
dim(Auto)
names(Auto)

# Additional Graphical and Numerical Summaries

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)#将定量的变量转换为定性变量
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
#如果绘制在x轴上的变量是定性的，箱线图（boxplot）将自动通过plot函数产生：数据没有，需作验证
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)#交互指定显示某个变量值
summary(Auto)
summary(mpg)

# savehistory()与loadhistory()