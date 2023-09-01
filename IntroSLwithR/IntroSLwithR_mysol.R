# 'An Introduction to Statistical Learning: With Applications in R'
# By Daniela Witten, Gareth M. James, Trevor Hastie, Robert Tibshirani
# Edition: 2013th
# Selected Exercises Solutions
# Author: Xiaoyu Li
# LinkedIn Profile: https://www.linkedin.com/in/xiaoyu-li-84435220/
# Created: 11/16/2018

# Contents:
# Chapter 1. Introduction
# Chapter 2. Statistical Learning
# Chapter 3. Linear Regression
# Chapter 4. Classification
# Chapter 5. Resampling Methods
# Chapter 6. Linear Model Selection and Regularization
# Chapter 7. Moving Beyond Linearity
# Chapter 8. Tree-Based Methods
# Chapter 9. Support Vector Machines
# Chapter 10. Unsupervised Learning

install.packages("ISLR")
# Chapter library(ISLR)
rm(list=ls())

############################################################
#### Chapter 2

##2.8
college=read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")
fix(college)  ## It causes R restart.
head(college)
pairs(college)
summary(college)
Elite=rep("No",nrow(college))
Elite[college$Top.10>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
summary(Elite)

##2.9
Auto=read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data", header=T)
Auto=na.omit(Auto)
head(Auto)

autoq=Auto[,c(1,3,5,6)]
rangeq=matrix(0,4,2)
meanq <-sdq <-rep(0,4)
for (i in 1:4) {
  rangeq[i,]=range(autoq[,i])
  meanq[i]=mean(autoq[,i])
  sdq[i]=sd(autoq[,i])
}
rangeq; meanq; sdq

autoqq=autoq[-c(10:85),]
rangeqq=matrix(0,4,2)
meanqq <-sdqq <-rep(0,4)
for (i in 1:4) {
  rangeqq[i,]=range(autoqq[,i])
  meanqq[i]=mean(autoqq[,i])
  sdqq[i]=sd(autoqq[,i])
}
rangeqq; meanqq; sdqq
pairs(Auto)

##2.10
library(MASS)
head(Boston)
pairs(Boston)

############################################################
#### Chapter 3

##3.8
attach(Auto)
Auto$horsepower<-as.numeric(Auto$horsepower)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
plot(horsepower,mpg)
abline(lm.fit)
predict(lm.fit,data.frame(horsepower=98), interval="confidence")
#fit      lwr      upr
#1 28.66647 27.36905 29.96389
predict(lm.fit,data.frame(horsepower=98), interval="prediction")
#fit     lwr      upr
#1 28.66647 14.6462 42.68674
par(mfrow=c(2,2))
plot(lm.fit)

pairs(Auto)
cor(Auto[,-9])
lm.fit1=lm(mpg~.-name, data=Auto)
summary(lm.fit1)
coef(lm.fit1)
plot(lm.fit1)
par(mfrow=c(1,1))
plot(hatvalues(lm.fit1))  
which.max(hatvalues(lm.fit1))  ## 14

summary(lm(mpg~weight*year, data=Auto))
summary(lm(mpg~weight:year, data=Auto))
lm.fit2=lm(log(mpg)~.-name, data=Auto)
summary(lm.fit2)
plot(lm.fit2)  ## best model among lm.fit2-4

lm.fit3=lm(sqrt(mpg)~.-name, data=Auto)
summary(lm.fit3)
plot(lm.fit3)
lm.fit4=lm(mpg^2~.-name, data=Auto)
summary(lm.fit4)
plot(lm.fit4)

##3.10
?Carseats
attach(Carseats)
lm.fit=lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)
lm.fit1=lm(Sales~Price+US, data=Carseats)
summary(lm.fit1)
confint(lm.fit1)
#2.5 %      97.5 %
#(Intercept) 11.79032020 14.27126531
#Price       -0.06475984 -0.04419543
#USYes        0.69151957  1.70776632
plot(lm.fit1)
plot(hatvalues(lm.fit1))  
which.max(hatvalues(lm.fit1))  ## 43

##3.11
set.seed (1)
x=rnorm (100)
y=2*x+rnorm (100)
lm.fit=lm(y~x+0)
summary(lm.fit)
#Estimate Std. Error t value Pr(>|t|)    
#x   1.9939     0.1065   18.73   <2e-16 ***
lm.fit1=lm(x~y+0)
summary(lm.fit1)
#Estimate Std. Error t value Pr(>|t|)    
#y  0.39111    0.02089   18.73   <2e-16 ***

lm.fit3=lm(y~x)
summary(lm.fit3)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.03769    0.09699  -0.389    0.698    
#x            1.99894    0.10773  18.556   <2e-16 ***
lm.fit4=lm(x~y)
summary(lm.fit4)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.03880    0.04266    0.91    0.365    
#y            0.38942    0.02099   18.56   <2e-16 ***

##3.13
set.seed (1)
x=rnorm(100)
eps=rnorm(100, mean=0, sd=0.5)
y=-1+0.5*x+eps
plot(x,y)
lm.fit=lm(y~x)
summary(lm.fit)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.01885    0.04849 -21.010  < 2e-16 ***
#  x          0.49947    0.05386   9.273 4.58e-15 ***
abline(lm.fit)
lines(x,(-1+0.5*x),col="red")
legend("topright",legend=c("fitted","true"),lty=c(1,1),col=c("black","red"))
lm.fit1=lm(y~poly(x,2))
summary(lm.fit1)
confint(lm.fit)
#2.5 %     97.5 %
#(Intercept) -1.1150804 -0.9226122
#x            0.3925794  0.6063602

eps=rnorm(100, mean=0, sd=0.1)
y=-1+0.5*x+eps
plot(x,y)
lm.fit2=lm(y~x)
summary(lm.fit2)
abline(lm.fit2)
lines(x,(-1+0.5*x),col="red")
legend("topright",legend=c("fitted","true"),lty=c(1,1),col=c("black","red"))
confint(lm.fit2)
#2.5 %     97.5 %
#(Intercept) -1.0141340 -0.9743329
#x            0.4723272  0.5165356

eps=rnorm(100, mean=0, sd=0.9)
y=-1+0.5*x+eps
plot(x,y)
lm.fit3=lm(y~x)
summary(lm.fit3)
abline(lm.fit3)
lines(x,(-1+0.5*x),col="red")
legend("topright",legend=c("fitted","true"),lty=c(1,1),col=c("black","red"))
confint(lm.fit3)
#2.5 %     97.5 %
#(Intercept) -1.2631978 -0.8447747
#x            0.4399594  0.9047159

##3.14
set.seed(1)
x1=runif(100)
x2 =0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
plot(x1,x2)
lm.fit=lm(y~x1+x2)
summary(lm.fit)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.1305     0.2319   9.188 7.61e-15 ***
#x1            1.4396     0.7212   1.996   0.0487 *  
#x2            1.0097     1.1337   0.891   0.3754
lm.fit1=lm(y~x1)
summary(lm.fit1)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.1124     0.2307   9.155 8.27e-15 ***
#x1            1.9759     0.3963   4.986 2.66e-06 ***
lm.fit2=lm(y~x2)
summary(lm.fit2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.3899     0.1949   12.26  < 2e-16 ***
#x2            2.8996     0.6330    4.58 1.37e-05 ***

x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)
lm.fit3=lm(y~x1+x2)
summary(lm.fit3)
#(Intercept)   2.2267     0.2314   9.624 7.91e-16 ***
#x1            0.5394     0.5922   0.911  0.36458    
#x2            2.5146     0.8977   2.801  0.00614 ** 
lm.fit4=lm(y~x1)
summary(lm.fit4)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.2569     0.2390   9.445 1.78e-15 ***
#x1            1.7657     0.4124   4.282 4.29e-05 ***
lm.fit5=lm(y~x2)
summary(lm.fit5)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.3451     0.1912  12.264  < 2e-16 ***
#x2            3.1190     0.6040   5.164 1.25e-06 ***
par(mfrow=c(2,2))
plot(lm.fit3)
plot(lm.fit4)
plot(lm.fit5)
plot(hatvalues(lm.fit3))  
which.max(hatvalues(lm.fit3))  ## 101
plot(hatvalues(lm.fit4))  
which.max(hatvalues(lm.fit4))  ## 27
plot(hatvalues(lm.fit5))  
which.max(hatvalues(lm.fit5))  ## 101

##3.15
library(MASS)
attach(Boston)
pairs(Boston)
lm.fit0=lm(crim~.,data=Boston)
lm.fit1=lm(crim~zn,data=Boston)
lm.fit2=lm(crim~indus,data=Boston)
lm.fit3=lm(crim~chas,data=Boston)
lm.fit4=lm(crim~nox,data=Boston)
lm.fit5=lm(crim~rm,data=Boston)
lm.fit6=lm(crim~age,data=Boston)
lm.fit7=lm(crim~dis,data=Boston)
lm.fit8=lm(crim~rad,data=Boston)
lm.fit9=lm(crim~tax,data=Boston)
lm.fit10=lm(crim~ptratio,data=Boston)
lm.fit11=lm(crim~black,data=Boston)
lm.fit12=lm(crim~lstat,data=Boston)
lm.fit13=lm(crim~medv,data=Boston)
x=coef(lm.fit0)[-1]
y=rep(0,13)
y[1]=coef(lm.fit1)[2]
y[2]=coef(lm.fit2)[2]
y[3]=coef(lm.fit3)[2]
y[4]=coef(lm.fit4)[2]
y[5]=coef(lm.fit5)[2]
y[6]=coef(lm.fit6)[2]
y[7]=coef(lm.fit7)[2]
y[8]=coef(lm.fit8)[2]
y[9]=coef(lm.fit9)[2]
y[10]=coef(lm.fit10)[2]
y[11]=coef(lm.fit11)[2]
y[12]=coef(lm.fit12)[2]
y[13]=coef(lm.fit13)[2]
plot(x,y)

############################################################
#### Chapter 4

##4.10
# The Weekly Data
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)

# Logistic Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
summary(glm.fit)  ## Lag 2 is significant.
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
#Direction
#glm.pred Down  Up
#Down   54  48
#Up    430 557
mean(glm.pred==Direction)
#[1] 0.5610652
train=(Year<2009)
Weekly.2009=Weekly[!train,]
Direction.2009=Direction[!train]
glm.fit=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.2009,type="response")
sum(!train)  ## 104
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2009)
#Direction.2009
#glm.pred Down Up
#Down    9  5
#Up     34 56
mean(glm.pred==Direction.2009)
#[1] 0.625

# Linear Discriminant Analysis
library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Weekly.2009)
lda.class=lda.pred$class
table(lda.class,Direction.2009)
#Direction.2009
#lda.class Down Up
#Down    9  5
#Up     34 56
mean(lda.class==Direction.2009)
#[1] 0.625

# Quadratic Discriminant Analysis
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.class=predict(qda.fit,Weekly.2009)$class
table(qda.class,Direction.2009)
#Direction.2009
#qda.class Down Up
#Down    0  0
#Up     43 61
mean(qda.class==Direction.2009)
#[1] 0.5865385

# K-Nearest Neighbors
library(class)
train.X=matrix(Lag2[train],ncol=1)
test.X=matrix(Lag2[!train],ncol=1)
train.Direction=Direction[train]
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2009)
#Direction.2009
#knn.pred Down Up
#Down   21 29
#Up     22 32
mean(knn.pred==Direction.2009)
#[1] 0.5096154

##4.11
# The Auto Data
summary(Auto)
mp=median(Auto$mpg)
Auto[,"mpg01"]<-1*(Auto$mpg>=mp)
attach(Auto)
pairs(Auto)
boxplot(displacement~mpg01,data=Auto)
boxplot(horsepower~mpg01,data=Auto)
boxplot(weight~mpg01,data=Auto)
train=(year<80)
train.auto=Auto[train,]
test.auto=Auto[!train,]
test.mpg01=mpg01[!train]

# Linear Discriminant Analysis
library(MASS)
lda.fit=lda(mpg01~displacement,data=Auto,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,test.auto)
lda.class=lda.pred$class
table(lda.class,test.mpg01)
#         test.mpg01
#lda.class  0  1
#        0  5  3
#        1  0 77
mean(lda.class!=test.mpg01)
#[1] 0.03529412

# Quadratic Discriminant Analysis
qda.fit=qda(mpg01~displacement,data=Auto,subset=train)
qda.fit
qda.class=predict(qda.fit,test.auto)$class
table(qda.class,test.mpg01)
#         test.mpg01
#qda.class  0  1
#        0  5  6
#        1  0 74
mean(qda.class!=test.mpg01)
#[1] 0.07058824

# Logistic Regression
glm.fit=glm(mpg01~displacement,data=Auto,family=binomial,subset=train)
glm.probs=predict(glm.fit,test.auto,type="response")
sum(!train)  ## 85
glm.pred=rep(0,85)
glm.pred[glm.probs>.5]=1
table(glm.pred,test.mpg01)
#        test.mpg01
#glm.pred  0  1
#       0  5  9
#       1  0 71
mean(glm.pred!=test.mpg01)
#[1] 0.1058824

# K-Nearest Neighbors
library(class)
train.X=matrix(displacement[train],ncol=1)
test.X=matrix(displacement[!train],ncol=1)
train.mpg01=matrix(mpg01[train],ncol=1)
knn.pred=knn(train.X,test.X,train.mpg01,k=1)
table(knn.pred,test.mpg01)
#test.mpg01
#knn.pred  0  1
#       0  5 15
#       1  0 65
mean(knn.pred!=test.mpg01)
#[1] 0.1764706
knn.pred=knn(train.X,test.X,train.mpg01,k=3)
table(knn.pred,test.mpg01)
#test.mpg01
#knn.pred  0  1
#       0  5  8
#       1  0 72
mean(knn.pred!=test.mpg01)
#[1] 0.09411765
knn.pred=knn(train.X,test.X,train.mpg01,k=5)
table(knn.pred,test.mpg01)
#test.mpg01
#knn.pred  0  1
#       0  5  8
#       1  0 72
mean(knn.pred!=test.mpg01)
#[1] 0.09411765
knn.pred=knn(train.X,test.X,train.mpg01,k=7)  ## best k
table(knn.pred,test.mpg01)
#test.mpg01
#knn.pred  0  1
#       0  5  7
#       1  0 73
mean(knn.pred!=test.mpg01)
#[1] 0.08235294
knn.pred=knn(train.X,test.X,train.mpg01,k=9)
table(knn.pred,test.mpg01)
#test.mpg01
#knn.pred  0  1
#       0  5  9
#       1  0 71
> mean(knn.pred!=test.mpg01)
#[1] 0.1058824

##4.12
# The Boston Data
library(MASS)
summary(Boston)
names(Boston)
pairs(Boston)
crimmd=median(Boston$crim)
Boston[,"crim01"]=1*(Boston$crim>=crimmd)
attach(Boston)
boxplot(indus~crim01)
boxplot(nox~crim01)
boxplot(age~crim01)
boxplot(dis~crim01)
boxplot(rad~crim01)

# Logistic Regression
test=401:506
train.Y=crim01[-test]
test.Y=crim01[test]
glm.fit=glm(crim01~.-crim,data=Boston,family=binomial,subset=-test)
summary(glm.fit)
glm.fit=glm(crim01~nox+rad+ptratio,data=Boston,family=binomial,subset=-test)
summary(glm.fit)
glm.fit=glm(crim01~nox+rad,data=Boston,family=binomial,subset=-test)
summary(glm.fit)
glm.probs=predict(glm.fit,Boston[test,],type="response")
glm.pred=rep(0,106)
glm.pred[glm.probs>.5]=1
table(glm.pred,test.Y)
#        test.Y
#glm.pred  0  1
#       0  5  0
#       1 10 91
mean(glm.pred==test.Y)
#[1] 0.9056604

# K-Nearest Neighbors
library(class)
X=cbind(nox,rad)
train.X=X[-test,]
test.X=X[test,]
knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred,test.Y)
#       test.Y
#knn.pred  0  1
#       0 10  3
#       1  5 88
mean(test.Y==knn.pred)
#[1] 0.9245283
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
#     test.Y
#knn.pred  0  1
#       0 10  3
#       1  5 88
mean(knn.pred==test.Y)
#[1] 0.9245283

# Linear Discriminant Analysis
library(MASS)
lda.fit=lda(crim01~nox+rad,data=Boston,subset=-test)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Boston[test,])
lda.class=lda.pred$class
table(lda.class,test.Y)
#       test.Y
#lda.class  0  1
#        0  5  0
#        1 10 91
mean(lda.class==test.Y)
#[1] 0.9056604

############################################################
#### Chapter 5

##5.5, 5.6
set.seed(1)
attach(Default)
glm.fit=glm(default~income+balance,data=Default,family=binomial)
train=sample(10000,5000)
glm.fit=glm(default~income+balance,data=Default,family=binomial,subset=train)
glm.probs=predict(glm.fit,Default[-train,],type="response")
glm.pred=rep("No",5000)
glm.pred[glm.probs>.5]="Yes"
mean(glm.pred!=default[-train])
#[1] 0.027
glm.fit=glm(default~income+balance+student,data=Default,family=binomial)
glm.fit=glm(default~income+balance+student,data=Default,family=binomial,subset=train)
glm.probs=predict(glm.fit,Default[-train,],type="response")
glm.pred=rep("No",5000)
glm.pred[glm.probs>.5]="Yes"
mean(glm.pred!=default[-train])
#[1] 0.0272

glm.fit=glm(default~income+balance,data=Default,family=binomial)
summary(glm.fit)$coef
#Estimate   Std. Error    z value      Pr(>|z|)
#(Intercept) -1.154047e+01 4.347564e-01 -26.544680 2.958355e-155
#income       2.080898e-05 4.985167e-06   4.174178  2.990638e-05
#balance      5.647103e-03 2.273731e-04  24.836280 3.638120e-136
boot.fn=function(data,index)
  return(coef(glm(default~income+balance,data=data,family=binomial,subset=index)))
boot(Default,boot.fn,10)
#Bootstrap Statistics :
#  original        bias     std. error
#t1* -1.154047e+01 -1.863001e-01 4.337560e-01
#t2*  2.080898e-05  2.438560e-06 5.224970e-06
#t3*  5.647103e-03  5.069617e-05 2.430507e-04

##5.7
attach(Weekly)
glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
train=2:1089
glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly[-train,],type="response")
glm.pred=rep("Down",1)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred!=Direction[-train])
#[1] 1

loocv.err=rep(0,1089)
for (i in 1:1089) {
  train=(1:1089)[-i]
  glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial,subset=train)
  glm.probs=predict(glm.fit,Weekly[-train,],type="response")
  glm.pred=rep("Down",1)
  glm.pred[glm.probs>.5]="Up"
  loocv.err[i]=1*(glm.pred!=Direction[-train])
}
mean(loocv.err)
#[1] 0.4499541

library(boot)
glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
cv.err=cv.glm(Weekly,glm.fit)
cv.err$delta[1]
#[1] 0.2464536

##5.8
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=-2*x^2+x+rnorm (100)
plot(x,y)
xy=data.frame(x,y)
set.seed(3)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i))
  print(summary(glm.fit))
  cv.error[i]=cv.glm(xy,glm.fit)$delta[1]
}
cv.error
#[1] 5.890979 1.086596 1.102585 1.114772

##5.9
library(MASS)
?Boston
attach(Boston)
#mean
mu.hat=mean(medv)
#[1] 22.53281
mu.hat.sd=sd(medv)/sqrt(506); mu.hat.sd
#[1] 0.4088611
----------
#Bootstrap
alpha.fn=function(data,index) {return(mean(data[index]))}
set.seed(1)
alpha.fn(medv,sample(506,506,replace=T))
boot(medv,alpha.fn,R=1000)
#Bootstrap Statistics :
#  original  bias    std. error
#t1* 22.53281 0.008857905   0.4127343
ci.boot=c(22.53281-1.96*0.4127343,22.53281+1.96*0.4127343); ci.boot
#[1] 21.72385 23.34177
t.test(medv)
#95 percent confidence interval:
#21.72953 23.33608

#median
mu.med=median(medv)
#[1] 21.2
----------
#Bootstrap
alpha.fn=function(data,index) {return(median(data[index]))}
set.seed(1)
alpha.fn(medv,sample(506,506,replace=T))
boot(medv,alpha.fn,R=1000)
#Bootstrap Statistics :
#  original  bias    std. error
#t1*     21.2 -0.01615   0.3803503

#10% quantile
mu.q10=quantile(medv,0.1); mu.q10
#  10% 
#12.75
----------
#Bootstrap
alpha.fn=function(data,index) {return(quantile(data[index],0.1))}
set.seed(1)
alpha.fn(medv,sample(506,506,replace=T))
boot(medv,alpha.fn,R=1000)
#Bootstrap Statistics :
#  original  bias    std. error
#t1*    12.75 0.00835   0.5066255

############################################################
#### Chapter 6

##6.8
library(leaps)
n=100
x=rnorm(n)
e=rnorm(n)
b0=5
b1=3
b2=1
b3=0.5
y=b0+b1*x+b2*x^2+b3*x^3+e
xy=data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)
names(xy)

# Best subset selection
regfit.full=regsubsets(y~.,data=xy,nvmax=10)
reg.summary=summary(regfit.full)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
adjr2=which.max(reg.summary$adjr2)
points(adjr2,reg.summary$adjr2[adjr2], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
cp=which.min(reg.summary$cp)  
points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
bic=which.min(reg.summary$bic)
points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
coef(regfit.full,cp)

# Forward Stepwise Selection
regfit.fwd=regsubsets(y~.,data=xy,nvmax=10,method="forward")
reg.summary=summary(regfit.fwd)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
adjr2=which.max(reg.summary$adjr2)
points(adjr2,reg.summary$adjr2[adjr2], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
cp=which.min(reg.summary$cp)  
points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
bic=which.min(reg.summary$bic)
points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
coef(regfit.full,cp)

# Backward Stepwise Selection
regfit.bwd=regsubsets(y~.,data=xy,nvmax=10,method="backward")
reg.summary=summary(regfit.bwd)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
adjr2=which.max(reg.summary$adjr2)
points(adjr2,reg.summary$adjr2[adjr2], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
cp=which.min(reg.summary$cp)  
points(cp,reg.summary$cp[cp],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
bic=which.min(reg.summary$bic)
points(bic,reg.summary$bic[bic],col="red",cex=2,pch=20)
coef(regfit.full,cp)


# The Lasso
library(glmnet)
z=model.matrix(y~.,xy)[,-1]
grid=10^seq(10,-2,length=100)
train=sample(1:n, n/2)
test=(-train)
y.test=y[test]
lasso.mod=glmnet(z[train,],y[train],alpha=1,lambda=grid)
par(mfrow=c(2,2))
plot(lasso.mod)
cv.out=cv.glmnet(z[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min; bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=z[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(z,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

b7=7
y=b0+b7*x^7+e
xy=data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)
names(xy)
z=model.matrix(y~.,xy)[,-1]
y.test=y[test]
lasso.mod=glmnet(z[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(z[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min; bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=z[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(z,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

##6.9
?College
dim(College)  ## 777 18
x=model.matrix(Apps~.,College)[,-1]
y=College$Apps
set.seed(10)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
par(mfrow=c(2,2))

## LSE
linear.mod=lm(Apps~., data=College, subset=train)
linear.pred=predict(linear.mod, newdata=College[test,])
mean((linear.pred-y.test)^2)  ## 1016996

# Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
plot(ridge.mod)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  ## 391.6761
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)      ## 903149.5
out=glmnet(x,y,alpha=0,lambda=grid)
predict(out,type="coefficients",s=bestlam)[1:18,]

# The Lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  ## 23.48036
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)  ## 930235
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:18,]
lasso.coef
lasso.coef[lasso.coef!=0]

# PCR
library(pls)
pcr.fit=pcr(Apps~.,data=College,subset=train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=10)
mean((pcr.pred-y.test)^2)  ## 1371446
pcr.fit=pcr(Apps~.,data=College,scale=TRUE,ncomp=10)
summary(pcr.fit)
#TRAINING: % variance explained
#1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
#X      31.670    57.30    64.30    69.90    75.39    80.38    83.99    87.40    90.50
#Apps    2.316    73.06    73.07    82.08    84.08    84.11    84.32    85.18    85.88
#10 comps
#X        92.91
#Apps     86.06

# PLS Regression
pls.fit=plsr(Apps~.,data=College,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=10)
mean((pls.pred-y.test)^2)  ## 1044772
pls.fit=plsr(Apps~.,data=College,scale=TRUE,ncomp=10)
summary(pls.fit)
#TRAINING: % variance explained
#      1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
#X       25.76    40.33    62.59    64.97    66.87    71.33    75.39    79.37    82.36
#Apps    78.01    85.14    87.67    90.73    92.63    92.72    92.77    92.82    92.87
#10 comps
#X        85.04
#Apps     92.89

#6.10
library(leaps)
set.seed(12)
n=1000
p=20
x=rnorm(n)
e=rnorm(n)
z=matrix(0,n,p)
b=rep(0,p)
for (i in 1:20) z[,i]=x^i
for (i in 1:10) b[i*2]=i
b0=10
y=z%*%b+b0+e
xy=data.frame(y,z)
names(xy)

# Best subset selection
train=sample(1:n, n/10)
test=(-train)
y.test=y[test]
regfit.best=regsubsets(y~.,data=xy[train,],nvmax=p)
summary(regfit.best)

# Train MSE
train.mat=model.matrix(y~.,data=xy[train,])
train.errors=rep(NA,p)
for(i in 1:p){
  coefi=coef(regfit.best,id=i)
  pred=train.mat[,names(coefi)]%*%coefi
  train.errors[i]=mean((xy$y[train]-pred)^2)
}
train.errors
plot(1:p,train.errors)
#[1] 8.375801e+10 2.348182e+07 1.019880e+04 4.324230e+01 1.326346e+00 1.036861e+00 1.000280e+00
#[8] 9.966330e-01 9.911634e-01 9.826485e-01 9.770659e-01 9.695977e-01 9.620533e-01 9.569597e-01
#[15] 9.443061e-01 9.436569e-01 9.426772e-01 9.412489e-01 9.406397e-01 9.406062e-01

# Test MSE
test.mat=model.matrix(y~.,data=xy[test,])
val.errors=rep(NA,p)
for(i in 1:p){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((xy$y[test]-pred)^2)
}
val.errors
plot(1:p,val.errors)
#[1] 7.879376e+16 1.049231e+15 1.215701e+13 1.890220e+10 5.861964e+10 2.559822e+10 3.568411e+08
#[8] 1.360040e+09 6.564037e+07 1.355633e+11 1.489193e+11 1.184314e+14 5.716304e+12 2.675647e+13
#[15] 2.105092e+13 5.191369e+13 6.801179e+13 2.251288e+14 2.723266e+14 2.568100e+14

m=which.min(val.errors)  ## 9
cf=coef(regfit.best,m); cf
#(Intercept)          X3          X4          X5          X8         X10         X14 
#9.9634553   0.2622482   7.8421862  -0.1106993  -3.9655768  16.1626688   8.6426772 
#X16         X18         X20 
#7.7627640   9.0165399   9.9996013 
cf=c(9.9634553,0,0,0.2622482,7.8421862,-0.1106993,0,0,-3.9655768,0,16.1626688,0,0,0,8.6426772,0,7.7627640,0,9.0165399,0,9.9996013)
mean((c(b0,b)-cf)^2)  ## 12.90591

##6.11
library(MASS)
?Boston
dim(Boston)  ## 506 14
x=model.matrix(crim~.,Boston)[,-1]
y=Boston$crim
set.seed(18)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
par(mfrow=c(2,2))

## LSE
linear.mod=lm(crim~., data=Boston, subset=train)
linear.pred=predict(linear.mod, newdata=Boston[test,])
mean((linear.pred-y.test)^2)  ## 25.03812

# Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
plot(ridge.mod)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  ## 1.406245
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)      ## 21.79243
out=glmnet(x,y,alpha=0,lambda=grid)
predict(out,type="coefficients",s=bestlam)[1:14,]

# The Lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  ## 0.1473205
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)  ## 22.72533
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:15,]
lasso.coef
lasso.coef[lasso.coef!=0]

# PCR
library(pls)
pcr.fit=pcr(crim~.,data=Boston,subset=train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=4)
mean((pcr.pred-y.test)^2)  ## 25.54514
pcr.fit=pcr(crim~.,data=Boston,scale=TRUE,ncomp=4)
summary(pcr.fit)
#TRAINING: % variance explained
#      1 comps  2 comps  3 comps  4 comps
#X       47.70    60.36    69.67    76.45
#crim    30.69    30.87    39.27    39.61

# PLS Regression
pls.fit=plsr(crim~.,data=Boston,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)  ## 25.26623
pls.fit=plsr(crim~.,data=Boston,scale=TRUE,ncomp=2)
summary(pls.fit)
#TRAINING: % variance explained
#      1 comps  2 comps
#X       47.27    56.79
#crim    34.32    41.81

############################################################
#### Chapter 7

##7.6
attach(Wage)

# Polynomial Regression
library(boot)
k=20
cv.error=rep(0,k)
for (i in 1:k){
  glm.fit=glm(wage~poly(age,i),data=Wage)
  cv.error[i]=cv.glm(Wage,glm.fit,K=10)$delta[1]
}
plot(1:k, cv.error, type="b")

fit1=lm(wage~age,data=Wage,subset=train)
fit2=lm(wage~poly(age,2),data=Wage,subset=train)
fit3=lm(wage~poly(age,3),data=Wage,subset=train)
fit4=lm(wage~poly(age,4),data=Wage,subset=train)
fit5=lm(wage~poly(age,5),data=Wage,subset=train)
fit6=lm(wage~poly(age,6),data=Wage,subset=train)
fit7=lm(wage~poly(age,7),data=Wage,subset=train)
fit8=lm(wage~poly(age,8),data=Wage,subset=train)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8)

fit=lm(wage~poly(age,3),data=Wage)
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-3 Polynomial")
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="green")

# Step Functions
library(boot)
k=40  ## number of age intervals
cv.error=rep(0,k)
m=10  ## number of folds in cv
folds=sample(1:m,3000,replace=TRUE)
for (i in 2:k){
  cutage=cut(age,i,labels=F)
  wacu=data.frame(wage,cutage)
  mse=rep(0,m)
  for (j in 1:m) {
    lm.fit=lm(wage~cutage,data=wacu[folds!=j,])
    preds=predict(lm.fit,newdata=list(cutage=cutage[folds==j]))
    mse[j]=mean((preds-wage[folds==j])^2)
  }
  cv.error[i]=mean(mse)
}
plot(2:k, cv.error[2:k], type="b")

fit=lm(wage~cut(age,7),data=Wage)
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Step-7 Function")
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="green")

##7.9
library(MASS)
attach(Boston)
fit=lm(nox~poly(dis,3),data=Boston)
summary(lm.fit)
pred=predict(lm.fit)
plot(dis,nox,col="darkgrey")
par(new=T)
plot(dis,pred,pch=20,col="red")

# degree 1:10 polynomials
k=10
rss=rep(0,k)
for (i in 1:k) {
  fit=lm(nox~poly(dis,i),data=Boston)
  rss[i]=sum(fit$res^2)
}
plot(1:k, rss, type="b")

# choose the best degree for polynomials by cross-validation
library(boot)
k=10
cv.error=rep(0,k)
for (i in 1:k){
  glm.fit=glm(nox~poly(dis,i),data=Boston)
  cv.error[i]=cv.glm(Boston,glm.fit,K=10)$delta[1]
}
plot(1:k, cv.error, type="b")

# basic spline
library(splines)
attr(bs(dis,df=4),"knots")  ## 50% 3.20745
fit=lm(nox~bs(dis,df=4),data=Boston)
dislims=range(dis)
dis.grid=seq(from=dislims[1],to=dislims[2])
pred=predict(fit,newdata=list(dis=dis.grid),se=T)
plot(dis,nox,col="gray")
lines(dis.grid,pred$fit,lwd=2)
lines(dis.grid,pred$fit+2*pred$se,lty="dashed")
lines(dis.grid,pred$fit-2*pred$se,lty="dashed")

# degree 3:10 basic splines
k=10
rss=rep(0,k)
for (i in 3:k) {
  fit=lm(nox~bs(dis,df=i),data=Boston)
  rss[i]=sum(fit$res^2)
}
plot(3:k, rss[3:k], type="b")

# choose the best degree for polynomials by cross-validation
library(boot)
k=10
cv.error=rep(0,k)
for (i in 4:k){
  kns=quantile(dis, probs=(1:(i-3))/(i-2))
  glm.fit=glm(nox~bs(dis,knots=kns),data=Boston)
  cv.error[i]=cv.glm(Boston,glm.fit,K=10)$delta[1]
}
plot(4:k, cv.error[4:k], type="b")

##7.10
# Forward Stepwise Selection
set.seed(20)
train=sample(c(TRUE,FALSE), nrow(College),rep=TRUE)
test=(!train)
library(leaps)
regfit.best=regsubsets(Outstate~.,data=College[train,],nvmax=17,method="forward")
test.mat=model.matrix(Outstate~.,data=College[test,])
val.errors=rep(NA,17)
for(i in 1:17){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((College$Outstate[test]-pred)^2)
}
val.errors
plot(1:17, val.errors, type="b")
coef(regfit.best,6)
#(Intercept)    PrivateYes    Room.Board           PhD   perc.alumni        Expend 
#-3568.3949939  2209.7779954     1.0574136    35.3762417    54.6330058     0.2442649 
#Grad.Rate 
#25.4142057

# GAM on selected predictors
pairs(College)
gam=lm(Outstate~Private+Room.Board+ns(PhD,2)+perc.alumni+Expend+Grad.Rate,data=College[test,])
test.error=mean(gam$res^2)
test.error; val.errors[6]
# ANOVA on GAMs
gam1=lm(Outstate~Private+Room.Board+PhD+perc.alumni+Expend+Grad.Rate,data=College)
gam2=lm(Outstate~Private+Room.Board+ns(PhD,2)+perc.alumni+Expend+Grad.Rate,data=College)
anova(gam1,gam2)

##7.11
n=100
x1=rnorm(n)
x2=rnorm(n,sd=2)
e=rnorm(n,sd=5)
y=5+2*x1-x2+e

# Initialize b1
b1=1
a=y-b1*x1
b2=lm(a~x2)$coef[2]

# fix b2
a=y-b2*x2
b1=lm(a~x1)$coef[2]

# loop
b0 <- b1 <- b2 <- rep(0,1000)
for (i in 2:1000) {
  a=y-b1[i-1]*x1
  b2[i]=lm(a~x2)$coef[2]
  a=y-b2[i]*x2
  b1[i]=lm(a~x1)$coef[2]
  b0[i]=mean(y)-b1[i]*mean(x1)-b2[i]*mean(x2)
}
b0[1000]; b1[1000]; b2[1000]
lm(y~x1+x2)

############################################################
#### Chapter 8

##8.7
library(MASS)
library(randomForest)
train=sample(1:nrow(Boston), nrow(Boston)/2)
x=1:8
y=1:20
mse=matrix(0,8,20)
for (i in x) {
  for (j in y) {
    bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=5+i,ntree=25*j)
    yhat.bag=predict(bag.boston,newdata=Boston[-train,])
    mse[i,j]=mean((yhat.bag-boston.test)^2)
  }
}
persp(x,y,mse,theta=-70,phi=20)
plot(25*(1:20), mse[1,], xlab="Number of Trees", 
     ylab="Test MSE", ylim=c(min(mse)-10,max(mse)+10), type="l", col=1)
for (i in 2:8) 
  lines(25*(1:20), mse[i,], type="l", col=i)
legend("topright",legend=c("mtree=6","mtree=7","mtree=8","mtree=9",
                           "mtree=10","mtree=11","mtree=12","mtree=13"),lty=1,col=c(1:8))

##8.8
# Regression Tree
library(tree)
set.seed(30)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)
tree.carseats=tree(Sales~.,Carseats,subset=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
yhat=predict(tree.carseats,newdata=Carseats[-train,])
carseats.test=Carseats[-train,"Sales"]
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)  # 4.874406

# Cross-Validation to Prune
cv.carseats=cv.tree(tree.carseats)  
plot(cv.carseats$size,cv.carseats$dev,type='b')
prune.carseats=prune.tree(tree.carseats,best=9)
summary(prune.carseats)
plot(prune.carseats)
text(prune.carseats,pretty=0)
yhat.prune=predict(prune.carseats,newdata=Carseats[-train,])
plot(yhat.prune,carseats.test)
abline(0,1)
mean((yhat.prune-carseats.test)^2)  # 4.683368

# Bagging
library(randomForest)
bag.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=10,importance=TRUE)
bag.carseats
yhat.bag=predict(bag.carseats,newdata=Carseats[-train,])
plot(yhat.bag, carseats.test)
abline(0,1)
mean((yhat.bag-carseats.test)^2) # 2.693186
importance(bag.carseats)  ## ShelveLoc, Price
varImpPlot(bag.carseats)

# Random Forest
mse=0
for (i in 1:10) {
  rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=i)
  yhat.rf=predict(rf.carseats,newdata=Carseats[-train,])
  mse[i]=mean((yhat.rf-carseats.test)^2)
}
plot(1:10, mse, xlab="Number of Features", ylab="Test MSE", type="b")
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=8,importance=TRUE)
mse[8]  ## 2.608665
importance(rf.carseats)  ## ShelveLoc, Price
varImpPlot(rf.carseats)

##8.9
set.seed(50)
train=sample(1:nrow(OJ), 800)
OJ.test=OJ[-train,]

# Classification Tree
library(tree)
tree.oj=tree(Purchase~.,data=OJ,subset=train)
summary(tree.oj)
#Classification tree:
#  tree(formula = Purchase ~ ., data = OJ, subset = train)
#Variables actually used in tree construction:
#[1] "LoyalCH"     "PriceDiff"   "SpecialCH"   "SalePriceMM"
#Number of terminal nodes:  9 
#Residual mean deviance:  0.6983 = 552.4 / 791 
#Misclassification error rate: 0.1562 = 125 / 800 
plot(tree.oj)
text(tree.oj,pretty=0)
tree.pred=predict(tree.oj,OJ.test,type="class")
oj.test=OJ[-train,"Purchase"]
table(tree.pred,oj.test)
#oj.test
#tree.pred  CH  MM
#       CH 142  33
#       MM  16  79
(16+33)/270  ## Test error rate = 0.1814815

# Cross-Validation to Prune
cv.oj=cv.tree(tree.oj,FUN=prune.misclass)
cv.oj
par(mfrow=c(1,2))
plot(cv.oj$size,cv.oj$dev,type="b")
plot(cv.oj$k,cv.oj$dev,type="b")
prune.oj=prune.misclass(tree.oj,best=7)
summary(prune.oj)
#Classification tree:
#snip.tree(tree = tree.oj, nodes = c(4L, 13L))
#Variables actually used in tree construction:
#  [1] "LoyalCH"   "PriceDiff" "SpecialCH"
#Number of terminal nodes:  7 
#Residual mean deviance:  0.7325 = 580.9 / 793 
#Misclassification error rate: 0.1562 = 125 / 800 
plot(prune.oj)
text(prune.oj,pretty=0)
tree.pred=predict(prune.oj,OJ.test,type="class")
table(tree.pred,oj.test)
#oj.test
#tree.pred  CH  MM
#       CH 141  32
#       MM  17  80
(17+32)/270  ## Test error rate = 0.1814815

##8.10
hitters<-na.omit(Hitters)
hitters$Salary <- log(hitters$Salary)
train=1:200
hitters.test=hitters[-train,"Salary"]

# Boosting
library(gbm)
set.seed(15)
mse=0
for (i in 1:20) {
  boost.hitters=gbm(Salary~.,data=hitters[train,],distribution="gaussian",interaction.depth=4,n.trees=1000,shrinkage=0.05*i)
  yhat.boost=predict(boost.hitters,newdata=hitters[-train,],n.trees=1000)
  mse[i]=mean((yhat.boost-hitters.test)^2)
}
plot(0.05*(1:20),mse,xlab="alpha",ylab="Test MSE",type="b")
boost.hitters=gbm(Salary~.,data=hitters[train,],distribution="gaussian",interaction.depth=4,n.trees=1000,shrinkage=0.1)
summary(boost.hitters)
par(mfrow=c(1,2))
plot(boost.hitters,i="CAtBat")  ## most important
plot(boost.hitters,i="CWalks")  ## 2nd most important
yhat.boost=predict(boost.hitters,newdata=hitters[-train,],n.trees=1000)
mse=mean((yhat.boost-hitters.test)^2); mse  ## 0.2822064

# Best Variable Subset Selection
library(leaps)
test=201:nrow(hitters)
regfit.best=regsubsets(Salary~.,data=hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((hitters.test-pred)^2)
}
val.errors
which.min(val.errors)  ## 8
val.errors[8]  # 0.468457
coef(regfit.best,8)

# Lasso
library(glmnet)
x=model.matrix(Salary~.,hitters)[,-1]
y=hitters$Salary
grid=10^seq(10,-2,length=100)
y.test=y[test]
set.seed(25)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  ## 0.003158391
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)  ## 0.470371
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef[lasso.coef!=0]

# Boosting test mse is the smallest among these 3 methods.

# Bagging
library(randomForest)
bag.hitters=randomForest(Salary~.,data=hitters,subset=train,mtry=19,importance=TRUE)
bag.hitters
yhat.bag=predict(bag.hitters,newdata=hitters[-train,])
plot(yhat.bag, hitters.test)
abline(0,1)
mean((yhat.bag-hitters.test)^2) # 0.2323019
importance(bag.hitters)  ## CAtBat
varImpPlot(bag.hitters)
# Bagging test mse is smaller than Boosting test mse.

##8.11
train=1:1000
Caravan$Purchase<-ifelse(Caravan$Purchase=="Yes",1,0)
caravan.test=Caravan[-train,"Purchase"]

# Boosting
library(gbm)
set.seed(50)
boost.caravan=gbm(Purchase~.,data=Caravan[train,],n.trees=1000,shrinkage=0.01)
summary(boost.caravan)
yhat.boost=predict(boost.caravan,newdata=Caravan[-train,],n.trees=1000,type="response")
yhat.boost[yhat.boost>0.2]=1
yhat.boost[yhat.boost<=0.2]=0
table(yhat.boost,caravan.test)
#           caravan.test
#yhat.boost    0    1
#         0 4400  256
#         1  133   33
mean(yhat.boost!=caravan.test)  ## 0.08067192

# Logistic Regression
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=train)
summary(glm.fit)
yhat.glm=predict(glm.fit,newdata=Caravan[-train,],type="response")
yhat.glm[yhat.glm>0.2]=1
yhat.glm[yhat.glm<=0.2]=0
table(yhat.glm,caravan.test)
#       caravan.test
#yhat.glm    0    1
#       0 4183  231
#       1  350   58
mean(yhat.glm!=caravan.test)  ## 0.1204894

# K-Nearest Neighbors
library(class)
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
train.X=standardized.X[train,]
test.X=standardized.X[-train,]
train.Y=Caravan$Purchase[train]
test.Y=caravan.test
error=0
for (i in 1:10) {
  knn.pred=knn(train.X,test.X,train.Y,k=i)
  error[i]=mean(test.Y!=knn.pred)
}
plot(1:10,error,xlab="k", ylab="Test Error Rate",type="b")
yhat.knn=knn(train.X,test.X,train.Y,k=9)
table(yhat.knn,caravan.test)
#         caravan.test
#knn.pred    0    1
#       0 4532  289
#       1    1    0
mean(yhat.knn!=caravan.test)  ## 0.06014102

# Boosting test error rate is the smallest among these 3 methods.

############################################################
#### Chapter 9

##9.4
set.seed(1)
n=100
x=matrix(0,n,2)
x[,1]=runif(n,-5,10)
x[,2]=x[,1]^2+rnorm(n)
z=sample(n,n/2)
x[z,2]=x[z,2]+10
y=rep(-1,n)
y[z]=y[z]+2
dat=data.frame(x=x, y=as.factor(y))
plot(x, col=(3-y))

# linear kernel
library(e1071)
svmfit1=svm(y~., data=dat, kernel="linear", cost=10)
plot(svmfit1, dat)
summary(svmfit1)
table(svmfit1$fitted, dat$y)

# quadratic kernel
svmfit2=svm(y~., data=dat, kernel="polynomial", degree=3, cost=10)
plot(svmfit2, dat)
summary(svmfit2)
table(svmfit2$fitted, dat$y)

# radial kernel
svmfit3=svm(y~., data=dat, kernel="radial", cost=10)
plot(svmfit3, dat)
summary(svmfit3)
table(svmfit3$fitted, dat$y)

##9.5
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2>0)
x=data.frame(x1,x2)
plot(x, col=(y+1), data=z)

# logistic regression: linear
glm.fit=glm(y~x1+x2,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,x,type="response")
glm.pred=rep(0,500)
glm.pred[glm.probs>.5]=1
plot(x, col=(glm.pred+3))
table(glm.pred,y)

# logistic regression: non-linear
glm.fit=glm(y~x1*x2,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,x,type="response")
glm.pred=rep(0,500)
glm.pred[glm.probs>.5]=1
plot(x, col=(glm.pred+3))
table(glm.pred,y)

glm.fit=glm(y~I(x1^2),family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,x,type="response")
glm.pred=rep(0,500)
glm.pred[glm.probs>.5]=1
plot(x, col=(glm.pred+3))
table(glm.pred,y)

# support vector classifier: linear
dat=data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit=svm(y~.,data=dat,kernel="linear",cost=10)
plot(svmfit, dat)
ypred=predict(svmfit,dat)
table(ypred,y)

# support vector classifier: non-linear
svmfit=svm(y~.,data=dat,kernel="radial",cost=10)
plot(svmfit, dat)
ypred=predict(svmfit,dat)
table(ypred,y)

##9.6
set.seed(3)
n=200
x=matrix(0,n,2)
x[,1]=runif(n,-2,8)
x[,2]=x[,1]^2+rnorm(n)
z=sample(n,n/2)
x[z,2]=x[z,2]+10
y=rep(-1,n)
y[z]=y[z]+2
dat=data.frame(x=x, y=as.factor(y))
plot(x, col=(3-y))

# a range of cost
train=sample(n,n/2)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)  ## cost=100, gamma=0.5
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))
#    pred
#true -1  1
#  -1 26 33
#  1  15 26
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=0.5,cost=1)
table(true=dat[-train,"y"], pred=predict(svmfit,newx=dat[-train,]))
#    pred
#true -1  1
#  -1 25 34
#  1  14 27

##9.7
library(ISLR)
med=median(Auto$mpg)
y=1*(Auto$mpg>med)
auto=data.frame(Auto[,-1],y=as.factor(y))
train=sample(392,250)

# svm: linear kernel
library(e1071)
tune.out=tune(svm, y~., data=auto[train,], kernel="linear", ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod) ## cost=1
table(true=auto[-train,"y"], pred=predict(bestmod,auto[-train,]))
#   pred
#true  0  1
#   0 66 10
#   1  6 60
svmfit=svm(y~., data=auto[train,], kernel="linear", cost=0.1)
table(true=auto[-train,"y"], pred=predict(svmfit,auto[-train,]))
#   pred
#true  0  1
#   0 65 11
#   1  4 62
plot(svmfit,auto[-train,],displacement~weight)
plot(svmfit,auto[-train,],horsepower~origin)

# svm: radial kernel
tune.out=tune(svm, y~., data=auto[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod) ## cost=1, gamma=0.5
table(true=auto[-train,"y"], pred=predict(bestmod,auto[-train,]))
#   pred
#true  0  1
#   0 64 12
#   1  3 63
svmfit=svm(y~., data=auto[train,], kernel="radial", cost=10, gamma=0.5)
table(true=auto[-train,"y"], pred=predict(svmfit,auto[-train,]))
#   pred
#true  0  1
#   0 65 11
#   1  2 64
plot(svmfit,auto[-train,],displacement~weight)
plot(svmfit,auto[-train,],horsepower~origin)

##9.8
library(ISLR)
train=sample(1070,800)

# svm: linear kernel
library(e1071)
svmfit=svm(Purchase~., data=OJ[train,], kernel="linear", cost=0.01)
summary(svmfit)
table(true=OJ[train,"Purchase"], pred=svmfit$fitted)
#     pred
#true  CH  MM
#  CH 430  58
#  MM  82 230  
# training error rate: 0.175
table(true=OJ[-train,"Purchase"], pred=predict(svmfit,OJ[-train,]))
#     pred
#true  CH  MM
#  CH 152  13
#  MM  24  81
# test error rate: 0.137

# svm: linear kernel, a range of cost 
tune.out=tune(svm, Purchase~., data=OJ[train,], kernel="linear", ranges=list(cost=c(0.1,1,5,10)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod) ## cost=10
table(true=OJ[train,"Purchase"], pred=bestmod$fitted)
#     pred
#true  CH  MM
#  CH 429  59
#  MM  77 235  
# training error rate: 0.170
table(true=OJ[-train,"Purchase"], pred=predict(bestmod,OJ[-train,]))
#     pred
#true  CH  MM
#  CH 152  13
#  MM  25  80
# test error rate: 0.141

# svm: radial kernel
svmfit=svm(Purchase~., data=OJ[train,], kernel="radial", cost=0.01)
summary(svmfit)
table(true=OJ[train,"Purchase"], pred=svmfit$fitted)
#     pred
#true  CH  MM
#  CH 488   0
#  MM 312   0  
# training error rate: 0.390
table(true=OJ[-train,"Purchase"], pred=predict(svmfit,OJ[-train,]))
#     pred
#true  CH  MM
#  CH 165   0
#  MM 105   0
# test error rate: 0.389

# svm: polynomial kernel
svmfit=svm(Purchase~., data=OJ[train,], kernel="polynomial", degree=2, cost=0.01)
summary(svmfit)
table(true=OJ[train,"Purchase"], pred=svmfit$fitted)
#     pred
#true  CH  MM
#  CH 488   0
#  MM 311   1  
# training error rate: 0.389
table(true=OJ[-train,"Purchase"], pred=predict(svmfit,OJ[-train,]))
#     pred
#true  CH  MM
#  CH 165   0
#  MM 105   0
# test error rate: 0.389
# linear kernel is the best of all.

############################################################
