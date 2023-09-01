# 'Introductory Time Series with R (Use R!)'
# By Andrew V. Metcalfe, Paul S.P. Cowpertwait
# Edition: 2009th
# Selected Exercises Solutions
# Author: Xiaoyu Li
# LinkedIn Profile: https://www.linkedin.com/in/xiaoyu-li-84435220/
# Created: 5/29/2014

# Contents:
# Chapter 1. Time Series Data 
# Chapter 2. Correlation
# Chapter 3. Forecasting Strategies
# Chapter 4. Basic Stochastic Models
# Chapter 5. Regression
# Chapter 6. Stationary Models
# Chapter 7. Non-stationary Models
# Chapter 8. Lond-Memory Processes
# Chapter 9. Spectral Analysis
# Chapter 10. System Identification
# Chapter 11. Multivariate Models
# Chapter 12. State Space Models 

############################################################
#### Chapter 1

##1
www = "http://elena.aut.ac.nz/~pcowpert/ts/cbe.dat"
cbe = read.table(www, head=T)
choc.ts <- ts(cbe[,1], st=1958, fr=12)
plot(choc.ts)
plot(aggregate(choc.ts))
boxplot(choc.ts ~ cycle(choc.ts))

##3
q0 <- c(0.33, 2000, 40, 3, 2)
p0 <- c(18000, 0.8, 40, 80, 200)
qt <- c(0.5, 1500, 20, 2, 1)
pt <- c(20000, 1.6, 60, 120, 360)
LI <- sum(q0 * pt)/sum(q0 * p0)
PI <- sum(qt * pt)/sum(qt * p0)
c(LI, PI)
##[1] 1.357873 1.250000
sqrt(LI * PI)
##[1] 1.302821

############################################################
#### Chapter 2

##1
www <- "http://elena.aut.ac.nz/~pcowpert/ts/varnish.dat"
varnish <- read.table(www, header=T)
plot(varnish)
cor(varnish)
##x          y
##x  1.0000000 -0.2528782
##y -0.2528782  1.0000000
www = "http://elena.aut.ac.nz/~pcowpert/ts/guesswhat.dat"
guesswhat = read.table(www, head=T)
plot(guesswhat)
cor(guesswhat)
##x y
##x 1.00000000 0.06457764
##y 0.06457764 1.00000000

##2
Serendipity <- c(39, 35, 16, 18, 7, 22, 13, 18, 20, 9, -12, -11, -19, -9, -2, 16)
Cagey <- c(47, -26, 42, -10, 27, -8, 16, 6, -1, 25, 11, 1, 25, 7, -5, 3)
ch2ex2 <- data.frame(Serendipity, Cagey)
plot(ts(ch2ex2))
attach(ch2ex2)
plot(Serendipity[1:15], Serendipity[2:16], xlab="", ylab="", main="Serendipity lag 1 scatter plot")
plot(Cagey[1:15], Cagey[2:16], xlab="", ylab="", main="Cagey lag 1 scatter plot")
acf(ts(ch2ex2))

##3
www <- "http://elena.aut.ac.nz/~pcowpert/ts/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                fr = 12)
Global.decom <- decompose(Global.ts)
plot(Global.decom)
sd(Global.ts)
##[1] 0.273536
sd(Global.ts - Global.decom$seasonal)
##[1] 0.2715033
boxplot(Global.ts ~ cycle(Global.ts))
ts.plot(Global.decom$trend, Global.decom$trend+Global.decom$seas, lty=1:2)
length(Global.decom$rand)
##[1] 1800
acf(Global.decom$rand[-c(1:6,1795:1800)])

############################################################
#### Chapter 3

##1
w <- 1:100
for (k in c(1,10,100)) {
  x <- w + k * rnorm(100)
  y <- w + k * rnorm(100)
  ccf(x, y)
}
Time <- 1:370
x <- sin(2 * pi * Time / 37)
y <- sin(2 * pi * (Time + 4) / 37)
ccf(x, y)

##2
F.logis = function(t) 1/(1+exp(-pi*t/sqrt(3)))
T = seq(-3,3, length=1000)
plot(pnorm(T), F.logis(T), type='l')

##6
www <- "http://elena.aut.ac.nz/~pcowpert/ts/wine.dat"
wine.dat <- read.table(www, header = T) ; attach (wine.dat)
sweetw.ts <- ts(sweetw, start = c(1980,1), freq = 12)
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)")
sweetw.hw.exe <- HoltWinters (sweetw.ts, alpha = 0.2, beta = 0.2, gamma = 0.2)
sweetw.hw.exe; sweetw.hw.exe$SSE
#Holt-Winters exponential smoothing with trend and additive seasonal component.
#
#Call:
#  HoltWinters(x = sweetw.ts, alpha = 0.2, beta = 0.2, gamma = 0.2)
#
#Smoothing parameters:
#alpha: 0.2
#beta : 0.2
#gamma: 0.2
#
#Coefficients:
#  [,1]
#a   243.053682
#b     1.845197
#s1    5.008604
#s2   11.368582
#s3   16.449351
#s4   68.786461
#s5  147.452725
#s6  -58.058742
#s7  -27.811882
#s8  -43.420420
#s9  -28.713225
#s10 -50.389279
#s11 -52.451059
#s12  13.755825
#[1] 693521.9
sweetw.hw.exe.log <- HoltWinters (log(sweetw.ts), alpha = 0.2, beta = 0.2, gamma = 0.2)
sweetw.hw.exe.log; sweetw.hw.exe.log$SSE
#Holt-Winters exponential smoothing with trend and additive seasonal component.
#
#Call:
#  HoltWinters(x = log(sweetw.ts), alpha = 0.2, beta = 0.2, gamma = 0.2)
#
#Smoothing parameters:
#alpha: 0.2
#beta : 0.2
#gamma: 0.2
#
#Coefficients:
#  [,1]
#a    5.461556754
#b    0.007002783
#s1   0.041643423
#s2   0.046497240
#s3   0.062209200
#s4   0.248598370
#s5   0.452407033
#s6  -0.238777716
#s7  -0.074977453
#s8  -0.136700659
#s9  -0.080183471
#s10 -0.189102051
#s11 -0.213964710
#s12  0.064425844
#[1] 9.704333

##7. exploratory time series analysis using the global temperature series from §1.4.5.
www <- "http://elena.aut.ac.nz/~pcowpert/ts/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)
boxplot(Global.ts ~ cycle(Global.ts))
Global.decom <- decompose(Global.ts)
plot(Global.decom)
Global.trend <- Global.decom$trend
Global.seas <- Global.decom$seasonal
ts.plot(cbind(Global.trend, Global.trend + Global.seas), lty = 1:2)
Global.ran <- Global.decom$random
Global.ran.ts <- window (Global.ran, start = c(1856, 7), end = c(2005, 6) )
acf(Global.ran.ts)

Global.hw.add <- HoltWinters (Global.ts); Global.hw.add; Global.hw.add$SSE
#Holt-Winters exponential smoothing with trend and additive seasonal component.
#
#Call:
#  HoltWinters(x = Global.ts)
#
#Smoothing parameters:
#  alpha: 0.3439351
#beta : 0.004529469
#gamma: 0.1604094
#
#Coefficients:
#  [,1]
#a   0.395753789
#b   0.001105568
#s1  0.109997726
#s2  0.139733643
#s3  0.110591531
#s4  0.087396336
#s5  0.047118300
#s6  0.063959977
#s7  0.070717795
#s8  0.076409625
#s9  0.055984516
#s10 0.050662362
#s11 0.028158920
#s12 0.017137797
#[1] 30.59679

Global.hw.mult <- HoltWinters (Global.ts, seasonal = "mult"); Global.hw.mult; Global.hw.mult$SSE
#Holt-Winters exponential smoothing with trend and multiplicative seasonal component.
#
#Call:
#  HoltWinters(x = Global.ts, seasonal = "mult")
#
#Smoothing parameters:
#  alpha: 0.7616377
#beta : 0.5693481
#gamma: 0.01225672
#
#Coefficients:
#  [,1]
#a    0.1878636
#b   -0.1255702
#s1   1.9764446
#s2   0.4762221
#s3   0.6218828
#s4   0.9229188
#s5   0.8111828
#s6   0.4177652
#s7   0.5730167
#s8   0.6151843
#s9   0.8981872
#s10  1.2871224
#s11  1.6120281
#s12  1.4695998
#[1] 176.2149
## So we choose additive Holt-Winters model.

plot(Global.hw.add)
Global.predict <- predict(Global.hw.add, n.ahead = 6 * 12)
ts.plot(Global.ts, Global.predict, lty = 1:2)

##8
www <- "http://elena.aut.ac.nz/~pcowpert/ts/motororg.dat"
Motor.dat <- read.table(www, header = T)
attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
Comp.minus <- Comp.ts-18
Comp.minus.sum <- cumsum(Comp.minus)
plot(Comp.minus.sum)

##9
www <- "http://elena.aut.ac.nz/~pcowpert/ts/motororg.dat"
Motor.dat <- read.table(www, header = T)
attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
Comp.hw.exe1 <- HoltWinters(complaints, alpha = 0.01, beta = FALSE, gamma = FALSE); Comp.hw.exe1
#Holt-Winters exponential smoothing without trend and without seasonal component.
#
#Call:
#  HoltWinters(x = complaints, alpha = 0.01, beta = FALSE, gamma = FALSE)
#
#Smoothing parameters:
#  alpha: 0.01
#beta : FALSE
#gamma: FALSE
#
#Coefficients:
#  [,1]
#a 23.94929

Comp.hw.exe2 <- HoltWinters(complaints, alpha = 0.99, beta = FALSE, gamma = FALSE); Comp.hw.exe2
#Holt-Winters exponential smoothing without trend and without seasonal component.
#
#Call:
#  HoltWinters(x = complaints, alpha = 0.99, beta = FALSE, gamma = FALSE)
#
#Smoothing parameters:
#  alpha: 0.99
#beta : FALSE
#gamma: FALSE
#
#Coefficients:
#  [,1]
#a 22.9303

plot(Comp.hw.exe1, xlab = "Time / months", main = "Holt-Winters filtering with alpha=0.01")
plot(Comp.hw.exe2, xlab = "Time / months", main = "Holt-Winters filtering with alpha=0.99")

############################################################
#### Chapter 4

##1
w <- rexp(1000)-1
plot(w, type = "l")
x <- seq(-1, 7, length = 10000)
hist(w, prob = T); points(x, dexp(x+1), type = "l")
acf(w)

##2
x <- w.exe <- rnorm(100)
for (t in 2:100) x[t] <- 1.02*x[t-1] + w[t] ##-0.5,0.5,0.9,1.01,1.02,1.05
x.ar <- ar(x, method = "mle")
x.ar$order
#[1] 1
x.ar$ar
#[1] -0.8783216
x.predict <- predict(x.ar, n.ahead = 10)
x.predict$pred
#$pred
#Time Series:
#  Start = 101 
#End = 110 
#Frequency = 1 
#[1] -0.132620572  0.229658045 -0.088539106  0.190940339 -0.054532506  0.161071606 -0.028298152  0.138029405 -0.008059688
#[10]  0.120253525
##may have some AR(2) estimates and large predictions when alpha=1.01,1.02,1.05

##4
x <- w <- rnorm(2000)
for (t in 3:2000) x[t] <- (5/6)*x[t-1] - (1/6)*x[t-2] + w[t]
acf(x)
pacf(x)
x.ar <- ar(x, method = "mle")
x.ar$order
#[1] 2
x.ar$ar
#[1]  0.8402273 -0.1796553
x.ar$ar[1] + c(-2, 2) * sqrt( diag(x.ar$asy.var.coef)[1] )
#[1] 0.7962627 0.8841919  ##5/6 falls within this interval.
x.ar$ar[2] + c(-2, 2) * sqrt( diag(x.ar$asy.var.coef)[2] )
#[1] -0.2236199 -0.1356908  ##-1/6 falls within this interval.
##non-stationary model
acf(x.ar$res[-(1:2)])

##5
y <- x <- w <- rnorm(1000)
for (t in 3:1000) x[t] <- (3/2)*x[t-1] - (1/2)*x[t-2] + w[t]
for (t in 4:1000) y[t] <- x[t] - x[t-1]
y <- y[-(1:3)]
y.ar <- ar(y, method = "mle")
y.ar$order
#[1] 1
y.ar$ar
#[1] 0.5053991
y.ar$ar + c(-2, 2) * sqrt( y.ar$asy.var.coef )
#[1] 0.4507447 0.5600534  ##0.5 falls within this interval.
acf(y.ar$res[-1])

##6
www = "http://elena.aut.ac.nz/~pcowpert/ts/global.dat"
Global = scan(www)
Global.ts = ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.ts.mean <- aggregate(Global.ts, FUN = mean)
Global.ar <- ar(Global.ts.mean, method = "mle")
mean <- mean(Global.ts.mean); mean
#[1] -0.1382628
Global.ar$order
#[1] 4
Global.ar$ar
#[1] 0.58762026 0.01260254 0.11116731 0.26763656

length(Global.ts.mean)
y <- x <- Global.ts.mean - mean
for (t in 5:150) y[t] <- sum( c(Global.ar$ar)*c(x[t-1],x[t-2],x[t-3],x[t-4]) )
fit <- y + mean
resid <- Global.ts.mean - fit
diff <- resid - Global.ar$res
diff; plot(diff) ##within machine accuracy the residual series is identical to the series extracted from the fitted model
acf(Global.ts.mean)
pacf(Global.ts.mean)

Global.predict <- predict(Global.ar, n.ahead = 100)
Global.predict$pred
#Time Series:
#  Start = 2006 
#End = 2105 
#Frequency = 1 
#[1] 0.46848295 0.45753512 0.44876876 0.44889732 0.44329169 0.43609476 0.42946315 0.42488682 0.41981377 0.41411169
#[11] 0.40841349 0.40320451 0.39808018 0.39284383 0.38759815 0.38248592 0.37746219 0.37246114 0.36748685 0.36257415
#[21] 0.35772416 0.35292086 0.34815979 0.34344757 0.33878657 0.33417347 0.32960590 0.32508445 0.32060972 0.31618089
#[31] 0.31179694 0.30745749 0.30316234 0.29891107 0.29470310 0.29053795 0.28641526 0.28233460 0.27829554 0.27429762
#[41] 0.27034044 0.26642359 0.26254666 0.25870924 0.25491093 0.25115132 0.24743002 0.24374665 0.24010082 0.23649214
#[51] 0.23292023 0.22938472 0.22588525 0.22242144 0.21899292 0.21559935 0.21224036 0.20891559 0.20562472 0.20236737
#[61] 0.19914323 0.19595194 0.19279317 0.18966659 0.18657187 0.18350870 0.18047673 0.17747567 0.17450519 0.17156498
#[71] 0.16865473 0.16577414 0.16292291 0.16010073 0.15730732 0.15454237 0.15180560 0.14909671 0.14641544 0.14376149
#[81] 0.14113458 0.13853444 0.13596080 0.13341339 0.13089194 0.12839619 0.12592587 0.12348072 0.12106049 0.11866492
#[91] 0.11629377 0.11394678 0.11162371 0.10932432 0.10704835 0.10479558 0.10256577 0.10035868 0.09817408 0.0960117
ts.plot(Global.ts.mean, Global.predict$pred, mean, lty = c(1,2,1))

############################################################
#### Chapter 5

##1
Time <- 1:100
x <- z <- w <- rnorm(100, sd = 25)
for (t in 2:100) 
  z[t] <- 0.5 * z[t - 1] + w[t]
for (t in 1:100) 
  x[t] <- 70 + 2*t - 3*t^2 + z[t]

x.lm <- lm( x ~ Time + I(Time^2) )
x.lm$coef
#(Intercept)        Time   I(Time^2) 
#72.681298    1.924679   -3.000086
confint(x.lm)
#2.5 %    97.5 %
#(Intercept) 56.023072 89.339523
#Time         1.163352  2.686005
#I(Time^2)   -3.007389 -2.992783
acf(resid(x.lm))

library(nlme)
x.gls <- gls( x ~ Time + I(Time^2), cor = corAR1(0.4) )
x.gls$coef
#(Intercept)        Time   I(Time^2) 
#52.348541    2.476056   -3.00201
sqrt(diag(vcov(x.gls)))  ##or use "summary(x.gls)"
#(Intercept)        Time   I(Time^2) 
#12.93191600  0.59134623  0.00566747 
sqrt(diag(vcov(x.lm)))
#(Intercept)        Time   I(Time^2) 
#8.385076947 0.383220810 0.003676091
#The standard errors of the parameter estimates are underestimated when using "lm". 

##2
www <- "http://elena.aut.ac.nz/~pcowpert/ts/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
temp <- window(Global.ts, start = 1970)
SIN <- COS <- matrix(nr = length(temp), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * time(temp))
  SIN[, i] <- sin(2 * pi * i * time(temp))
}
TIME <- (time(temp) - mean(time(temp)))/sd(time(temp))
mean(time(temp))
#[1] 1987.958
sd(time(temp))
#[1] 10.40433
temp.lm1 <- lm(temp ~ TIME + I(TIME^2) +
                 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                 COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                 COS[,5] + SIN[,5] + COS[,6])  ## with SIN[,6] skipped
acf(resid(temp.lm1))  ## Lag 1 autocorrelation approximates 0.7 in the ACF correlogram of "lm" residuals.
summary(temp.lm1)
#Call:
#  lm(formula = temp ~ TIME + I(TIME^2) + COS[, 1] + SIN[, 1] + 
#       COS[, 2] + SIN[, 2] + COS[, 3] + SIN[, 3] + COS[, 4] + SIN[, 
#                                                                  4] + COS[, 5] + SIN[, 5] + COS[, 6] + SIN[, 6])
#
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.37661 -0.08010 -0.00295  0.07725  0.43124 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.166319   0.009108  18.261   <2e-16 ***
#  TIME         0.184235   0.006081  30.296   <2e-16 ***
#  I(TIME^2)    0.008713   0.006804   1.280   0.2011    
#COS[, 1]     0.006521   0.008587   0.759   0.4481    
#SIN[, 1]     0.020424   0.008589   2.378   0.0179 *  
#COS[, 2]     0.011854   0.008587   1.380   0.1682    
#SIN[, 2]     0.016157   0.008587   1.881   0.0606 .  
#COS[, 3]     0.005535   0.008587   0.645   0.5195    
#SIN[, 3]     0.003365   0.008587   0.392   0.6953    
#COS[, 4]     0.004693   0.008587   0.547   0.5850    
#SIN[, 4]     0.001454   0.008587   0.169   0.8656    
#COS[, 5]     0.002752   0.008587   0.320   0.7488    
#SIN[, 5]     0.003050   0.008587   0.355   0.7226    
#COS[, 6]    -0.001704   0.006072  -0.281   0.7791    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 0.1262 on 418 degrees of freedom
#Multiple R-squared: 0.6893,  Adjusted R-squared: 0.6797 
#F-statistic: 71.35 on 13 and 418 DF,  p-value: < 2.2e-16 

temp.gls <- gls(temp ~ TIME + I(TIME^2) +
                 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                 COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                 COS[,5] + SIN[,5] + COS[,6], cor = corAR1(0.7))    ## with SIN[,6] skipped
acf(resid(temp.gls))
summary(temp.gls)
#Generalized least squares fit by REML
#Model: temp ~ TIME + I(TIME^2) + COS[, 1] + SIN[, 1] + COS[, 2] + SIN[,      2] + COS[, 3] + SIN[, 3] + COS[, 4] + SIN[, 4] + COS[, 5] +      SIN[, 5] + COS[, 6]
#Data: NULL 
#AIC       BIC   logLik
#-731.9597 -667.392 381.9799
#
#Correlation Structure: AR(1)
#Formula: ~1 
#Parameter estimate(s):
#  Phi 
#0.7204967  
#
#Coefficients:
#  Value Std.Error   t-value p-value
#(Intercept)  0.16649076 0.022803110  7.301230  0.0000
#TIME         0.18059087 0.015047354 12.001503  0.0000
#I(TIME^2)    0.00843343 0.016645474  0.506650  0.6127
#COS[, 1]     0.00594926 0.011580102  0.513748  0.6077
#SIN[, 1]     0.01852083 0.011625545  1.593115  0.1119
#COS[, 2]     0.01119421 0.006764904  1.654748  0.0987
#SIN[, 2]     0.01505068 0.006773939  2.221851  0.0268
#COS[, 3]     0.00485444 0.004909727  0.988738  0.3234
#SIN[, 3]     0.00269522 0.004909727  0.548955  0.5833
#COS[, 4]     0.00400457 0.004046114  0.989732  0.3229
#SIN[, 4]     0.00106049 0.004041926  0.262372  0.7932
#COS[, 5]     0.00205998 0.003641559  0.565687  0.5719
#SIN[, 5]     0.00286624 0.003635231  0.788462  0.4309
#COS[, 6]    -0.00205051 0.002487454 -0.824341  0.4102
#
# Correlation: 
#(Intr) TIME   I(TIME COS[,1 SIN[,1 COS[,2 SIN[,2 COS[,3 SIN[,3 COS[,4 SIN[,4 COS[,5 SIN[,5
#TIME       0.000                                                                                    
#I(TIME^2) -0.745  0.000                                                                             
#COS[, 1]   0.009  0.004 -0.019                                                                      
#SIN[, 1]  -0.002  0.015  0.005  0.002                                                               
#COS[, 2]   0.004  0.004 -0.009 -0.002  0.002                                                        
#SIN[, 2]  -0.002  0.007  0.005  0.002  0.002  0.002                                                 
#COS[, 3]   0.002  0.005 -0.004 -0.001  0.003  0.001  0.003                                          
#SIN[, 3]  -0.002  0.005  0.004  0.002  0.002  0.002  0.002  0.002                                   
#COS[, 4]   0.001  0.005 -0.002  0.000  0.003  0.001  0.003  0.002  0.003                            
#SIN[, 4]  -0.001  0.003  0.003  0.001  0.001  0.001  0.001  0.002  0.001  0.002                     
#COS[, 5]   0.000  0.006  0.000  0.001  0.003  0.002  0.003  0.003  0.003  0.003  0.002              
#SIN[, 5]  -0.001  0.002  0.002  0.001  0.001  0.001  0.001  0.001  0.001  0.001  0.000  0.001       
#COS[, 6]   0.000  0.004  0.000  0.001  0.002  0.001  0.002  0.002  0.002  0.002  0.001  0.003  0.001
#
#Standardized residuals:
#Min           Q1          Med           Q3          Max 
#-2.956251193 -0.632901725 -0.008855557  0.601734064  3.413547995 
#
#Residual standard error: 0.1281438 
#Degrees of freedom: 432 total; 418 residual

## lm model fitting indicates that "TIME", "SIN[,1]", "SIN[,2]" are significant.
## gls model fitting indicates that "TIME", "COS[,2]", "SIN[,2]" are significant.

##3
www <- "http://elena.aut.ac.nz/~pcowpert/ts/cbe.dat"
CBE <- read.table(www, header = T)
class(CBE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts)
plot(log(Elec.ts))

Seas <- cycle(Elec.ts)
Time <- time(Elec.ts)
Elec.lm <- lm( log(Elec.ts) ~ Time + I(Time^2) + factor(Seas) )
step(Elec.lm)
#Start:  AIC=-2717.56
#log(Elec.ts) ~ Time + I(Time^2) + factor(Seas)
#
#Df Sum of Sq     RSS     AIC
#<none>                      0.38604 -2717.6
#- factor(Seas) 11    2.4917 2.87773 -1944.1
#- I(Time^2)     1    2.5629 2.94894 -1914.4
#- Time          1    2.6457 3.03174 -1903.4
#
#Call:
#  lm(formula = log(Elec.ts) ~ Time + I(Time^2) + factor(Seas))
#
#Coefficients:
#  (Intercept)            Time       I(Time^2)   factor(Seas)2   factor(Seas)3   factor(Seas)4   factor(Seas)5  
#-3.979e+03       3.977e+00      -9.911e-04      -1.991e-02       6.598e-02       3.288e-02       1.462e-01  
#factor(Seas)6   factor(Seas)7   factor(Seas)8   factor(Seas)9  factor(Seas)10  factor(Seas)11  factor(Seas)12  
#1.777e-01       2.375e-01       1.994e-01       1.074e-01       9.044e-02       4.278e-02       2.350e-02

SIN <- COS <- matrix(nr = length(Elec.ts), nc = 6)
for (i in 1:6) {
  SIN[, i] <- sin(2 * pi * i * Time)
  COS[, i] <- cos(2 * pi * i * Time)
}
Elec.lm1 <- lm(log(Elec.ts) ~ Time + I(Time^2) +
               SIN[,1] + COS[,1] + SIN[,2] + COS[,2] + SIN[,3] + COS[,3] +
               SIN[,4] + COS[,4] + SIN[,5] + COS[,5] + COS[,6])
step(Elec.lm1)
#...
#Step:  AIC=-2723.4
#log(Elec.ts) ~ Time + I(Time^2) + SIN[, 1] + COS[, 1] + SIN[, 
#                                                            2] + COS[, 2] + COS[, 3] + SIN[, 5] + COS[, 6]
#
#Df Sum of Sq     RSS     AIC
#<none>                   0.38815 -2723.4
#- SIN[, 2]   1   0.00578 0.39393 -2719.6
#- SIN[, 1]   1   0.00726 0.39541 -2718.1
#- COS[, 6]   1   0.02525 0.41340 -2700.4
#- COS[, 3]   1   0.04718 0.43533 -2680.0
#- COS[, 2]   1   0.08367 0.47182 -2648.1
#- SIN[, 5]   1   0.11380 0.50196 -2623.6
#- COS[, 1]   1   2.20668 2.59483 -1973.0
#- I(Time^2)  1   2.56289 2.95105 -1922.1
#- Time       1   2.64570 3.03386 -1911.2
#Call:
#  lm(formula = log(Elec.ts) ~ Time + I(Time^2) + SIN[, 1] + COS[, 
#                                                                1] + SIN[, 2] + COS[, 2] + COS[, 3] + SIN[, 5] + COS[, 6])
#
#Coefficients:
#  (Intercept)         Time    I(Time^2)     SIN[, 1]     COS[, 1]     SIN[, 2]     COS[, 2]     COS[, 3]     SIN[, 5]  
#-3.979e+03    3.977e+00   -9.911e-04   -6.057e-03   -1.056e-01   -5.402e-03    2.056e-02   -1.544e-02   -2.397e-02  
#COS[, 6]  
#7.985e-03

##The harmonic model Elec.lm1 is a better model with a smaller AIC.
acf(Elec.lm1$res)
pacf(Elec.lm1$res)

Elec.ar <- ar(Elec.lm1$res); Elec.ar
#
#Call:
#  ar(x = Elec.lm1$res)
#
#Coefficients:
#  1        2        3        4        5        6        7        8        9       10       11       12       13  
#0.3831   0.2226   0.0794   0.0272   0.0024   0.0328  -0.0223   0.0061   0.1076   0.0352   0.1201   0.1661  -0.0882  
#14       15       16       17       18       19       20       21       22       23  
#-0.0619  -0.0493  -0.0905   0.0250   0.0019  -0.1253   0.0172  -0.0564  -0.0139   0.1616  
#
#Order selected 23  sigma^2 estimated as  0.0003904
Elec.ar$order
#[1] 23
acf(Elec.ar$res[-(1:23)])

res.pred <- predict(Elec.ar, n.ahead = 10 * 12)
new.t <- time(ts(start = 1991, end = c(2000, 12), fr = 12))
SIN <- COS <- matrix(nr = length(new.t), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * new.t)
  SIN[, i] <- sin(2 * pi * i * new.t)
}
new.dat <- data.frame(Time = as.vector(new.t), SIN = SIN, COS = COS)
log.pred <- predict(Elec.lm1, new.dat)
sigmasq <- 0.0003904
elec.pred <- exp( log.pred + res.pred$pred + 0.5*sigmasq )
Elec.pred.ts <- ts(elec.pred, st = 1991, fr = 12)
ts.plot(Elec.ts, Elec.pred.ts, lty=1:2)

##6
www <- "http://elena.aut.ac.nz/~pcowpert/ts/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
temp <- window(Global.ts, start = 1970)
SIN <- COS <- matrix(nr = length(temp), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * time(temp))
  SIN[, i] <- sin(2 * pi * i * time(temp))
}
TIME <- (time(temp) - mean(time(temp)))/sd(time(temp))
mean(time(temp))
#[1] 1987.958
sd(time(temp))
#[1] 10.40433
temp.lm1 <- lm(temp ~ TIME + I(TIME^2) +
                 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                 COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                 COS[,5] + SIN[,5] + COS[,6])
pacf(temp.lm1$res)
temp.gls <- gls(temp ~ TIME + I(TIME^2) +
                 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                 COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                 COS[,5] + SIN[,5] + COS[,6], cor = corARMA(p=2) )

confint(temp.gls)
#Coefficients:
#  Value  Std.Error   t-value p-value
#(Intercept)  0.16491741 0.03321920  4.964522  0.0000
#TIME         0.17640856 0.02157702  8.175762  0.0000
#I(TIME^2)    0.01092502 0.02346649  0.465558  0.6418
#COS[, 1]     0.00608086 0.00857706  0.708968  0.4787
#SIN[, 1]     0.01800489 0.00860924  2.091345  0.0371
#COS[, 2]     0.01131070 0.00495997  2.280396  0.0231
#SIN[, 2]     0.01494851 0.00496650  3.009868  0.0028
#COS[, 3]     0.00497396 0.00406404  1.223894  0.2217
#SIN[, 3]     0.00262212 0.00406404  0.645200  0.5192
#COS[, 4]     0.00412855 0.00405406  1.018375  0.3091
#SIN[, 4]     0.00097855 0.00405285  0.241447  0.8093
#COS[, 5]     0.00219054 0.00453349  0.483191  0.6292
#SIN[, 5]     0.00279377 0.00453433  0.616136  0.5381
#COS[, 6]    -0.00198296 0.00346917 -0.571595  0.5679
temp.lm <- lm(temp ~ TIME + I(TIME^2) +
                  COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                  COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                  COS[,5] + SIN[,5] + COS[,6])
ts.plot(temp.gls$res,temp.lm$res, col=1:2)

res.ar <- ar(temp.gls$res); res.ar
#Call:
#  ar(x = temp.gls$res)
#Coefficients:
#  1       2  
#0.4886  0.3105  
#Order selected 2  sigma^2 estimated as  0.007011
summary(temp.gls)  ##residual's AR(2) model
#...
#Correlation Structure: ARMA(2,0)
#Formula: ~1 
#Parameter estimate(s):
#  Phi1      Phi2 
#0.4946496 0.3249836

tempann <- aggregate(temp, FUN = mean)
TIME <- 1970:2005
tempann.lm <- lm(tempann ~ TIME)
confint(tempann.lm)
#2.5 %       97.5 %
#  (Intercept) -41.00212133 -29.03549629
#TIME          0.01469715   0.02071801

############################################################
#### Chapter 6

##2
x <- w <- rnorm(1000)
for (t in 2:1000)   x[t] <- w[t] + 2 * w[t - 1]
plot(x, type = "l")
acf(x)

##3
## See 'http://elena.aut.ac.nz/~pcowpert/ts/solutions.pdf'.

##4
data(AirPassengers)
AP <- AirPassengers
Time <- 1:length(AP)
Imth <- cycle(AP)
AP.lm <- lm(log(AP) ~ Time + I(Time^2) + factor(Imth))
acf(AP.lm$res)  ## Significant autocorrelation on lag 1, 2, 3, and >=15.

best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) 
  for (j in 0:2) {
    arma <- arima( AP.lm$res, order = c(i, 0, j) )
    fit.aic <- AIC(arma)
    if (fit.aic < best.aic) {
      best.order <- c(i, 0, j)
      best.arma <- arma
      best.aic <- fit.aic
    }
  }
best.order
#[1] 1 0 2
acf(best.arma$res)  ## Significant autocorrelation on lag 16.

new.time <- seq(length(AP)+1, length = 12)
new.data <- data.frame(Time = new.time, Imth = 1:12)
predict.lm <- predict(AP.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 12)
AP.pred <- ts( exp(predict.lm + predict.arma$pred), start = 1961, freq = 12 )
ts.plot(cbind(AP, AP.pred), lty = 1:2)

##5
## ACF rho(k) of ARMA(1,1)
rho <- function(k, alpha, beta) {
  if (k == 0) ACF <- 1 else {
      ACF <- alpha^(k-1)*(alpha+beta)*(1+alpha*beta) / (1+2*alpha*beta+beta^2); ACF
  }
}
## Compute rho(k), k=1,...,10, alpha=0.7, beta=-0.5
rho.k <- rep(1, 20)
for (k in 1:20) rho.k[k] <- rho(k, 0.7, -0.5)
plot(0:20, c(1, rho.k), pch = 4, ylab = expression(rho[k]))
##The function 'expression' is used to get the Greek symbol 'rho'.
abline(0, 0)

## Simulate an ARMA(1,1) series 'x' with alpha=0.7, beta=-0.5
x <- w <- rnorm(100)
for (t in 2:100)  x[t] <- 0.7 * x[t-1] + w[t] - 0.5 * w[t - 1]
plot(x, type = "l")
acf(x)

## Fitting ARMA(1,1) to 'x' to get estimates of alpha and beta
x.arma <- arima(x, order = c(1, 0, 1))
x.arma
#Call:
#  arima(x = x, order = c(1, 0, 1))
#Coefficients:
#  ar1     ma1  intercept
#0.6654  -0.3183     0.1554
#s.e.  0.1912   0.2435     0.1969
#sigma^2 estimated as 0.9618:  log likelihood = -140.06,  aic = 288.11

##6
#a) 9/(4*n) - 1/n^2
#b) 1/(4*n) + 1/n^2
#c) 1/n; 9/(4*n) - 1/n^2 > 1/n > 1/(4*n) + 1/n^2
##d)
m <- rep(0, 100)
for (i in 1:100){
  x <- w <- rnorm(20)
  for (t in 2:20)  x[t] <- w[t] + 0.7 * w[t - 1]
  m[i] <- mean(x)
}
var(m)
#[1] 0.1305464
m <- rep(0, 100)
for (i in 1:100){
  x <- w <- rnorm(20)
  for (t in 2:20)  x[t] <- w[t] - 0.7 * w[t - 1]
  m[i] <- mean(x)
}
var(m)
#[1] 0.005367855

##7
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
acf(x)
pacf(x)

## fit arma model, p<=4, q<=4
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:4) 
  for (j in 0:4) {
    print (c(i,0,j))
    arma <- arima( x, order = c(i, 0, j) )
    fit.aic <- AIC(arma)
    if (fit.aic < best.aic) {
      best.order <- c(i, 0, j)
      best.arma <- arma
      best.aic <- fit.aic
    }
  }
best.order; best.aic
#[1] 1 0 2
#[1] 28339.07

## fit arma(1,1) model
x.arma <- arima(x, order = c(1, 0, 1))
AIC(x.arma)
#[1] 28344.96

############################################################
#### Chapter 7

##4
x <- scan("http://elena.aut.ac.nz/~pcowpert/ts/osvisit.dat")
x.ts <- ts(x, start = 1977, freq = 12)
z.ts <- log(x.ts)
plot(z.ts)
z.arima1 <- arima(z.ts, order = c(1,1,0)); z.arima1  ## method="CSS-ML"
#Call:
#  arima(x = z.ts, order = c(1, 1, 0))
#Coefficients:
#  ar1
#0.0250
#s.e.  0.0668
#sigma^2 estimated as 0.05002:  log likelihood = 17.86,  aic = -31.73
z.arima1.res <- z.arima1$res
sd(z.arima1.res)
#[1] 0.2235809
acf(z.arima1.res)  ## z.arima1.res have strong autocorrelation on lag 0.5 (year), 1.0, 1.5, ...
z.arima2 <- arima(z.ts, order = c(1,1,0), seas = list(order = c(0,1,0), 12)); z.arima2  ## method="CSS-ML"
#Call:
#  arima(x = z.ts, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), 12))
#Coefficients:
#  ar1
#-0.4209
#s.e.   0.0629
#sigma^2 estimated as 0.005265:  log likelihood = 258.84,  aic = -513.69
z.arima2.res <- z.arima2$res
sd(z.arima2.res)
#[1] 0.07065806
acf(z.arima2.res)  ## z.arima2.res have strong autocorrelation on lag 0.16 (year), 1.0, ...

get.best.arima1 <- function(x.ts, maxord = c(1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(q in 0:maxord[2])
    for (P in 0:maxord[3]) for(Q in 0:maxord[4])
    {
      if ( (p+q+P+Q==2) + (p+q==2) ) 
      {
        fit <- arima(x.ts, order = c(p,1,q), seas = list(order = c(P,1,Q), frequency(x.ts)), method = "CSS")
        fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)  ## consistent AIC
        if(fit.aic < best.aic)
        {
          best.aic <- fit.aic
          best.fit <- fit
          best.model <- c(p,1,q,P,1,Q) 
        }
      }
    }
    list(best.aic, best.fit, best.model)
}

z.best <- get.best.arima1(z.ts, maxord = rep(1,4))  ## method="CSS"
z.best
#[[1]]
#[1] -561.3234
#[[2]]
#Call:
#  arima(x = x.ts, order = c(p, 1, q), seasonal = list(order = c(P, 1, Q), frequency(x.ts)), 
#        method = "CSS")
#Coefficients:
#  ma1     sar1
#-0.6934  -0.2662
#s.e.   0.0663   0.0630
#sigma^2 estimated as 0.004052:  part log likelihood = 287.09
#[[3]]
#[1] 0 1 1 1 1 0
z.best.res <- z.best[[2]]$res
sd(z.best.res)
#[1] 0.06237859
acf(z.best.res)  ## looks like acf of white noise
acf(z.best.res^2)  ## looks like acf of white noise^2
# z.best[[2]]:  ( 1 - B )( 1 - B^{12} )( 1 + 0.2662 * B^{12} ) z_t = ( 1 - 0.6934 * B ) w_t

log.pred <- predict(z.best[[2]], n.ahead = 12)$pred
sigmasq <- mean(z.best.res^2)
x.pred <- exp( log.pred + 0.5*sigmasq )
x.pred.ts <- ts(x.pred, st = 1996, fr = 12)
ts.plot(x.ts, x.pred.ts, lty=1:2)

##5
www = "http://elena.aut.ac.nz/~pcowpert/ts/cbe.dat"
cbe = read.table(www, head=T)
choc.ts <- ts(cbe[,1], st=1958, fr=12)
plot(choc.ts)
plot(log(choc.ts))
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))  #### SARIMA
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q), frequency(x.ts)), 
                   method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)  ## consistent AIC
      if(fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q) 
      }
    }
  list(best.aic, best.fit, best.model)
}
best.arima.choc <- get.best.arima( log(choc.ts), maxord = c(2,2,2,2,2,2)); best.arima.choc
#[[1]]
#[1] -663.6902
#[[2]]
#Call:
#  arima(x = x.ts, order = c(p, d, q), seasonal = list(order = c(P, D, Q), frequency(x.ts)), 
#        method = "CSS")
#Coefficients:
#  ar1      ma1    sar1     sma1     sma2
#0.1070  -0.8322  0.9909  -0.6097  -0.1717
#s.e.  0.0584   0.0446  0.0049   0.0503   0.0505
#sigma^2 estimated as 0.009987:  part log likelihood = 349.3
#[[3]]
#[1] 1 1 1 1 0 2
best.fit.choc <- best.arima.choc[[2]]
choc.res <- resid(best.fit.choc)
acf( choc.res )
acf(choc.res^2)  ## looks like acf's of white noise

##6
www = "http://elena.aut.ac.nz/~pcowpert/ts/stockmarket.dat"
x = read.table(www, head=T)
ams <- x[,1]
ts.plot(ams)
ts.plot(diff(ams))
get.best.arima <- function(x.ts, maxord = c(1,1)) 
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(q in 0:maxord[2])
  {
    fit <- arima(x.ts, order = c(p,1,q), method = "CSS")
    fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)  ## consistent AIC
    if(fit.aic < best.aic)
    {
      best.aic <- fit.aic
      best.fit <- fit
      best.model <- c(p,1,q) 
    }
  }
  list(best.aic, best.fit, best.model)
}
best.arima.ams <- get.best.arima( ams, maxord = c(1,1)); best.arima.ams
#[[1]]
#[1] 18569.77
#[[2]]
#Call:
#  arima(x = x.ts, order = c(p, 1, q), method = "CSS")
#sigma^2 estimated as 22.21:  part log likelihood = -9284.89
#[[3]]
#[1] 0 1 0
best.fit.ams <- best.arima.ams[[2]]
ams.res <- resid(best.fit.ams)[-1]
acf(ams.res)  ## with some small seasonal significant values
acf(ams.res^2)  ## larger seasonal significant values

library(tseries)
get.best.garch <- function(x.ts, maxord = c(1,1)) 
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (q in 0:maxord[1]) for(p in 0:maxord[2])
  {
    if ( (q+p<=2) && (q+p>0) )
    {
      fit <- garch(x.ts, order = c(q,p), grad = "numerical", trace = FALSE)
      fit.aic <- fit$n.likeli
      if(fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(q,p) 
      }
    }
  }
  list(best.aic, best.fit, best.model)
}
best.garch.res <- get.best.garch( ams.res, maxord = c(1,2)); best.garch.res
#[[1]]
#[1] 5193.139
#[[2]]
#Call:
#  garch(x = x.ts, order = c(q, p), grad = "numerical", trace = FALSE)
#Coefficient(s):
#a0       a1       b1  
#0.20871  0.07225  0.91174 
#[[3]]
#[1] 1 1
best.g.res <- best.garch.res[[2]]
confint(best.g.res)
#2.5 %    97.5 %
#a0 0.16079147 0.25662001
#a1 0.06267217 0.08183235
#b1 0.89869631 0.92477941
## all significant
gres <- resid(best.g.res)[-1]
acf(gres)  ## seasonal significant values
acf(gres^2)

##7
stemp <- scan("http://elena.aut.ac.nz/~pcowpert/ts/stemp.dat")
stemp.ts <- ts(stemp, start = 1850, freq = 12)
plot(stemp.ts)
library(tseries)
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))  #### SARIMA
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q), frequency(x.ts)), 
                   method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)  ## consistent AIC
      if(fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q) 
      }
    }
  list(best.aic, best.fit, best.model)
}
stemp.best <- get.best.arima(stemp.ts, maxord = rep(2,6))  ## method="CSS"
stemp.best[[3]]
#[1] 1 1 2 2 0 1
stemp.arima <- arima(stemp.ts, order = c(1,1,2), seas = list(order = c(2,0,1), 12))  ## method="CSS-ML"(default)
stemp.arima 
t( confint(stemp.arima) )
stemp.arima <- arima(stemp.ts, order = c(1,1,2), seas = list(order = c(1,0,1), 12))
t( confint(stemp.arima) )
stemp.res <- resid(stemp.arima)
layout(1:2)
acf(stemp.res)
acf(stemp.res^2)
stemp.garch <- garch(stemp.res, trace = F)
t(confint(stemp.garch))
stemp.garch.res <- resid(stemp.garch)[-1]
acf(stemp.garch.res)
acf(stemp.garch.res^2)
plot(predict(stemp.garch))

layout(1)
stemp.pred <- predict(stemp.arima, n.ahead = 12)
stemp.recent <- window(stemp.ts, start = 1990)
ts.plot(stemp.recent, stemp.pred$pred, lty = 1:2)
upbd <- stemp.pred$pred + 1.96 * stemp.pred$se; upbd

length(predict(stemp.garch))
#[1] 3792
a0 <- stemp.garch$coef[1]
a1 <- stemp.garch$coef[2]
b1 <- stemp.garch$coef[3]
vep <- rep(0,12)
vep[1] <- sqrt( a0 + (a1 + b1) * predict(stemp.garch)[3792]^2 )
for (i in 2:12)
  vep[i] <- sqrt( a0 + (a1 + b1) * vep[i-1]^2 )
vep
pred <- stemp.pred$pred + vep * ( 2*rbinom(12,1,0.5)-1)
ts.plot(stemp.recent, pred, lty = 1:2)

############################################################
#### Chapter 9

##2
## Example: Simulation of AR(1) with positive coefficient, ACF, Spectrum
x <- w <- rnorm(1024)
for (t in 2:1024) x[t]<- 0.9 * x[t-1] + w[t]
layout(1:3)
plot(as.ts(x))
acf(x)
spectrum(x, span = 51, log = c("no"))

x <- w <- rexp(1024)
for (t in 2:1024) x[t]<- 0.9 * x[t-1] + w[t]
layout(1:3)
plot(as.ts(x))
acf(x)
spectrum(x, span = 51, log = c("no"))

## Example: Simulation of AR(1) with negative coefficient, ACF, Spectrum
x <- w <- rnorm(1024)
for (t in 2:1024) x[t]<- -0.9 * x[t-1] + w[t]
layout(1:3)
plot(as.ts(x))
acf(x)
spectrum(x, span = 51, log = c("no"))

x <- w <- rexp(1024)
for (t in 2:1024) x[t]<- -0.9 * x[t-1] + w[t]
layout(1:3)
plot(as.ts(x))
acf(x)
spectrum(x, span = 51, log = c("no"))

##5
www <- "http://elena.aut.ac.nz/~pcowpert/ts/wave.dat"
wavetank.dat <- read.table(www, header=T)
attach (wavetank.dat)
layout (1:4)
plot (as.ts(waveht))
acf (waveht)
spectrum (waveht)
spectrum( fft(waveht), span = 5 )

##6
t <- 0:7
s1 <- sin(pi*t/2)
s2 <- sin(pi*t*3/4)
s3 <- sin(pi*t*5/8)
fft(s1); fft(s2); fft(s3)

##7
t <- 0:31
s1 <- sin(pi*t*11/32)
fft(s1)
plot(t, fft(s1))
spectrum(s1)

##8
layout(1)
a <- 5
b <- 10
gamma <- function(w){
  a * w^(-5) * exp( -b * w^(-4) )
}
t <- (0:(pi*10))/10
plot(t, gamma(t), type="l")

############################################################
#### Chapter 10

##2
m <- 1; c <- 1; k <- 16.25; Delta <- 0.01  ## frequency = 1s / 0.01(s/Hz) = 100 Hz
a0 <- m / Delta^2 + c / Delta + k
a1 <- -2 * m / Delta^2 - c / Delta; a2 <- m / Delta^2
n <- 100000  ## The length of spectrum is n/2. The highest frequency is 0.5 * 100Hz = 50Hz.
y <- c(0, 0); x <- c(0, 0)
set.seed(1)
for (i in 3:n) {
  x[i] <- x[i-1] - 0.5 * x[i-2] + rnorm(1)
  y[i] <- (-a1 * y[i-1] - a2 * y[i-2] + x[i]) / a0
}
Sxx <- spectrum(x, span = 31)
Syy <- spectrum(y, span = 31)
Gemp <- sqrt( Syy$spec[1:5000] / Sxx$spec[1:5000] )
Freq <- Syy$freq[1:5000]
FreH <- Freq / Delta
plot(FreH, Gemp, type = "l")

nax <- 0; nay <- 1
for (i in 1:n) {
  x[i] <- x[i] + nax * rnorm(1)
  y[i] <- y[i] + nay * rnorm(1)
}
Sxx <- spectrum(x, span = 31)
Syy <- spectrum(y, span = 31)
Gemp <- sqrt( Syy$spec[1:5000] / Sxx$spec[1:5000] )
plot(FreH, Gemp, type = "l")

##3
## Delta^(-1) (sampling rate) = 1000/s
m <- 1; c <- 1; k <- 16.25; Delta <- 0.001  ## frequency = 1s / 0.001(s/Hz) = 1000 Hz
a0 <- m / Delta^2 + c / Delta + k
a1 <- -2 * m / Delta^2 - c / Delta; a2 <- m / Delta^2
n <- 100000  ## The length of spectrum is n/2. The highest frequency is 0.5 * 1000Hz = 500Hz.
y <- c(0, 0); x <- c(0, 0)
set.seed(1)
for (i in 3:n) {
  x[i] <- x[i-1] - 0.5 * x[i-2] + rnorm(1)
  y[i] <- (-a1 * y[i-1] - a2 * y[i-2] + x[i]) / a0
}
Sxx <- spectrum(x, span = 31)    ## Gemp is closer to Gth with a smaller span, for example: span = 2.
Syy <- spectrum(y, span = 31)
Freq <- Syy$freq[1:500]
FreH <- Freq / Delta
Omeg <- 2 * pi * Freq
OmegH <- 2 * pi * FreH
Gth <- sqrt( 1/( (k-m*OmegH^2)^2 + c^2*OmegH^2 ))
Gemp <- sqrt( Syy$spec[1:500] / Sxx$spec[1:500] )
Gar <- 1 / abs( a0 + a1 * exp(-Omeg*1i) + a2 * exp(-Omeg*2i) )
plot(FreH, Gth, xlab = "Frequency (Hz)", ylab = "Gain", type="l")
lines(FreH, Gemp, lty = "dashed")              ## Gemp is a little different from Gth. Gemp has a lower peak.
lines(FreH, Gar, lty = "dotted", col="red")    ## Gar is almost the same as Gth.

## Delta^(-1) (sampling rate) = 10000/s
m <- 1; c <- 1; k <- 16.25; Delta <- 0.0001  ## frequency = 1s / 0.0001(s/Hz) = 10000 Hz
a0 <- m / Delta^2 + c / Delta + k
a1 <- -2 * m / Delta^2 - c / Delta; a2 <- m / Delta^2
n <- 100000  ## The length of spectrum is n/2. The highest frequency is 0.5 * 1000Hz = 500Hz.
y <- c(0, 0); x <- c(0, 0)
set.seed(1)
for (i in 3:n) {
  x[i] <- x[i-1] - 0.5 * x[i-2] + rnorm(1)
  y[i] <- (-a1 * y[i-1] - a2 * y[i-2] + x[i]) / a0
}
Sxx <- spectrum(x, span = 31)    ## Gemp is closer to Gth with a smaller span, for example: span = 2.
Syy <- spectrum(y, span = 31)
Freq <- Syy$freq[1:50]
FreH <- Freq / Delta
Omeg <- 2 * pi * Freq
OmegH <- 2 * pi * FreH
Gth <- sqrt( 1/( (k-m*OmegH^2)^2 + c^2*OmegH^2 ))
Gemp <- sqrt( Syy$spec[1:50] / Sxx$spec[1:50] )
Gar <- 1 / abs( a0 + a1 * exp(-Omeg*1i) + a2 * exp(-Omeg*2i) )
plot(FreH, Gth, xlab = "Frequency (Hz)", ylab = "Gain", type="l")
lines(FreH, Gemp, lty = "dashed")              ## Gemp is far from Gth.
lines(FreH, Gar, lty = "dotted", col="red")    ## Gar is the same as Gth.

## using a centred difference approximation to the derivatives: dy_t/dt = ( y_t - y_{t-2} ) / (2*Delta)
m <- 1; c <- 1; k <- 16.25; Delta <- 0.01  ## frequency = 1s / 0.01(s/Hz) = 100 Hz
a0 <- m / Delta^2 / 4 + c / Delta / 2 + k
a1 <- -m / Delta^2 / 2 - c / Delta / 2
a2 <- m / Delta^2 / 4
n <- 100000  ## The length of spectrum is n/2. The highest frequency is 0.5 * 100Hz = 50Hz.
y <- rep(0,4); x <- rep(0,4)
for (i in 5:n) {
  x[i] <- x[i-1] - 0.5 * x[i-2] + rnorm(1)
  y[i] <- (-a1 * y[i-2] - a2 * y[i-4] + x[i]) / a0
}
Sxx <- spectrum(x, span = 31)
Syy <- spectrum(y, span = 31)
Freq <- Syy$freq[1:5000]
FreH <- Freq / Delta
Omeg <- 2 * pi * Freq
OmegH <- 2 * pi * FreH
Gth <- sqrt( 1/( (k-m*OmegH^2)^2 + c^2*OmegH^2 ))
Gemp <- sqrt( Syy$spec[1:5000] / Sxx$spec[1:5000] )
Gar <- 1 / abs( a0 + a1 * exp(-Omeg*2i) + a2 * exp(-Omeg*4i) )
plot(FreH, Gth, xlab = "Frequency (Hz)", ylab = "Gain", type="l")
lines(FreH, Gemp, lty = "dashed")              ## Gemp is not close to Gth. Gemp has a lower peak.
lines(FreH, Gar, lty = "dotted", col="red")    ## Gar is the same as Gemp.

## using a centred difference approximation to the derivatives: dy_t/dt = ( y_t - y_{t-3} ) / (3*Delta)
m <- 1; c <- 1; k <- 16.25; Delta <- 0.01  ## frequency = 1s / 0.01(s/Hz) = 100 Hz
a0 <- m / Delta^2 / 9 + c / Delta / 3 + k
a1 <- -2 * m / Delta^2 / 9 - c / Delta / 3
a2 <- m / Delta^2 / 9
n <- 100000  ## The length of spectrum is n/2. The highest frequency is 0.5 * 100Hz = 50Hz.
y <- rep(0,6); x <- rep(0,6)
for (i in 7:n) {
  x[i] <- x[i-1] - 0.5 * x[i-2] + rnorm(1)
  y[i] <- (-a1 * y[i-3] - a2 * y[i-6] + x[i]) / a0
}
Sxx <- spectrum(x, span = 31)
Syy <- spectrum(y, span = 31)
Freq <- Syy$freq[1:5000]
FreH <- Freq / Delta
Omeg <- 2 * pi * Freq
OmegH <- 2 * pi * FreH
Gth <- sqrt( 1/( (k-m*OmegH^2)^2 + c^2*OmegH^2 ))
Gemp <- sqrt( Syy$spec[1:5000] / Sxx$spec[1:5000] )
Gar <- 1 / abs( a0 + a1 * exp(-Omeg*3i) + a2 * exp(-Omeg*6i) )
plot(FreH, Gth, xlab = "Frequency (Hz)", ylab = "Gain", type="l")
lines(FreH, Gemp, lty = "dashed")              ## Gemp is not close to Gth. Gemp has an even more lower peak.
lines(FreH, Gar, lty = "dotted", col="red")    ## Gar is the same as Gemp.

############################################################
#### Chapter 11

##3
www <- "http://elena.aut.ac.nz/~pcowpert/ts/stockmarket.dat"
stm <- read.table(www, header = T)
ld <- stm$London
ny <- stm$NewYork
acf( diff(ld) )
acf( diff(ny) )
##The correlogram plots of the differenced ld and ny series indicate that 
##both series can be well approximated by random walk.
plot(ld, ny, pch = 4)
cor(ld, ny)
#[1] 0.9838358
library(tseries)
adf.test(ld)
#Augmented Dickey-Fuller Test
#data:  ld 
#Dickey-Fuller = -1.641, Lag order = 14, p-value = 0.7303
#alternative hypothesis: stationary
adf.test(ny)
#Augmented Dickey-Fuller Test
#data:  ny 
#Dickey-Fuller = 0.3074, Lag order = 14, p-value = 0.99
#alternative hypothesis: stationary 
#Warning message:
#   In adf.test(ny) : p-value greater than printed p-value
pp.test(ld)
#Phillips-Perron Unit Root Test
#data:  ld 
#Dickey-Fuller Z(alpha) = -6.5177, Truncation lag parameter = 9, p-value = 0.7462
#alternative hypothesis: stationary
pp.test(ny)
#Phillips-Perron Unit Root Test
#data:  ny 
#Dickey-Fuller Z(alpha) = 1.2237, Truncation lag parameter = 9, p-value = 0.99
#alternative hypothesis: stationary 
#Warning message:
#  In pp.test(ny) : p-value greater than printed p-value

##All tests above show that both series are non-stationary.

ldny.ar <- ar(cbind(ld,ny))
ldny.ar$order
#[1] 11

library(vars)
stm.var <- VAR(cbind(ld, ny), p = 1, type = "trend")
coef(stm.var)
#$ld
#Estimate  Std. Error     t value     Pr(>|t|)
#ld.l1  0.9946712361 0.001528040 650.9460028 0.0000000000
#ny.l1  0.0374811330 0.010255771   3.6546382 0.0002617824
#trend -0.0005893123 0.001067102  -0.5522548 0.5808133139
#$ny
#Estimate   Std. Error     t value  Pr(>|t|)
#ld.l1 -0.0002481118 0.0002673601  -0.9280061 0.3534761
#ny.l1  1.0013854132 0.0017944454 558.0473104 0.0000000
#trend  0.0001935077 0.0001867102   1.0364067 0.3000927

##The ny series influences the ld series the most, based on the 'Pr(>|t|)' of coefficients on each other. 

po.test(cbind(ld, ny))
#Phillips-Ouliaris Cointegration Test
#data:  cbind(ld, ny) 
#Phillips-Ouliaris demeaned = -33.085, Truncation lag parameter = 31, p-value = 0.01
#Warning message:
#  In po.test(cbind(ld, ny)) : p-value smaller than printed p-value

##The Phillips-Ouliaris test provides evidence that the series are cointegrated since the null hypothesis is rejected at the 1% level.
##The Phillips-Ouliaris test shows there is evidence that the series are cointegrated, 
##which justifies the use of a regression model.

ldny.lm <- lm(ld ~ ny)
summary(ldny.lm)
#Call:
#  lm(formula = ld ~ ny)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-413.55 -116.59   -9.10   90.88  577.65 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 629.86637    7.42018   84.89   <2e-16 ***
#  ny            4.82723    0.01571  307.18   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Residual standard error: 158 on 3126 degrees of freedom
#Multiple R-squared: 0.9679,  Adjusted R-squared: 0.9679 
#F-statistic: 9.436e+04 on 1 and 3126 DF,  p-value: < 2.2e-16

ldny.res <- resid(ldny.lm)
adf.test(ldny.res)
#Augmented Dickey-Fuller Test
#data:  ldny.res 
#Dickey-Fuller = -3.8238, Lag order = 14, p-value = 0.01779
#alternative hypothesis: stationary
pp.test(ldny.res)
#Phillips-Perron Unit Root Test
#data:  ldny.res 
#Dickey-Fuller Z(alpha) = -32.3813, Truncation lag parameter = 9, p-value = 0.01
#alternative hypothesis: stationary 
#Warning message:
#  In pp.test(ldny.res) : p-value smaller than printed p-value

##Both tests show that the residual series is stationary.

ldny.res.ar <- ar(ldny.res)
ldny.res.ar$order
#[1] 11


##4
library(vars)
data(Canada)
cnd.var <- VAR(Canada, p = 1, type = "trend")
acf(resid(cnd.var)[, 1])
acf(resid(cnd.var)[, 2])
acf(resid(cnd.var)[, 3])
acf(resid(cnd.var)[, 4])
cnd.var <- VAR(Canada, p = 2, type = "trend")
acf(resid(cnd.var)[, 1])
acf(resid(cnd.var)[, 2])
acf(resid(cnd.var)[, 3])
acf(resid(cnd.var)[, 4])
coef(cnd.var)

cnd.pred <- predict(cnd.var, n.ahead = 4)
cnd.pred
e.pred <- ts(cnd.pred$fcst$e[, 1], start = 2001, fr = 4)
prod.pred <- ts(cnd.pred$fcst$prod[, 1], start = 2001, fr = 4)
rw.pred <- ts(cnd.pred$fcst$rw[, 1], start = 2001, fr = 4)
U.pred <- ts(cnd.pred$fcst$U[, 1], start = 2001, fr = 4)
ts.plot(cbind(Canada[,1], e.pred), main = "e", lty = 1:2)
ts.plot(cbind(Canada[,2], prod.pred), main = "prod", lty = 1:2)
ts.plot(cbind(Canada[,3], rw.pred), main = "rw", lty = 1:2)
ts.plot(cbind(Canada[,4], U.pred), main = "U", lty = 1:2)

##5
www <- "http://elena.aut.ac.nz/~pcowpert/ts/cbe.dat"
CBE <- read.table(www, header = T)
elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
elec.log.arima <- arima(log(elec.ts), order = c(1,1,0), seas = list(order = c(1,1,1), frequency(elec.ts)), method = "CSS")
elec.log.arima.res <- elec.log.arima$res
acf(elec.log.arima.res)
acf(elec.log.arima.res^2)
choc.log.arima <- arima(log(choc.ts), order = c(1,1,0), seas = list(order = c(1,1,1), frequency(elec.ts)), method = "CSS")
choc.log.arima.res <- choc.log.arima$res
acf(choc.log.arima.res)
acf(choc.log.arima.res^2)
ccf(elec.log.arima.res, choc.log.arima.res)
predict(elec.log.arima, n.ahead = 12)
elec.pred <- exp( predict(elec.log.arima, n.ahead = 12)$pred ) * exp( sd(elec.log.arima.res)^2/2 )
ts.plot(cbind(elec.ts, elec.pred), lty = 1:2)
choc.pred <- exp( predict(choc.log.arima, n.ahead = 12)$pred ) * exp( sd(choc.log.arima.res)^2/2 )
ts.plot(cbind(choc.ts, choc.pred), lty = 1:2)

############################################################
#### Chapter 12

##1
library(sspir)
Plummet.dat <- 20 + 2*rnorm(20) + c(rep(0,10), rep(-10,10))
n <- length(Plummet.dat)
Plummet.mat <- matrix(Plummet.dat, nrow = n, ncol = 1)
m1 <- SS(y = Plummet.mat,
         Fmat = function(tt,x,phi) return( matrix(1) ),
         Gmat = function(tt,x,phi) return( matrix(1) ),
         Wmat = function(tt,x,phi) return( matrix(10)),
         Vmat = function(tt,x,phi) return( matrix(2) ),
         m0 = matrix(25), C0 = matrix(10))
plot(m1$y, ylab = "Closing price", main = "Simulated")
m1.f <- kfilter(m1)
m1.s <- smoother(m1.f)
lines(m1.f$m, lty = 2)
lines(m1.s$m, lty = 3)

m1 <- SS(y = Plummet.mat,
         Fmat = function(tt,x,phi) return( matrix(1) ),
         Gmat = function(tt,x,phi) return( matrix(1) ),
         Wmat = function(tt,x,phi) return( matrix(10)),
         Vmat = function(tt,x,phi) return( matrix(200) ),
         m0 = matrix(25), C0 = matrix(10))
plot(m1$y, ylab = "Closing price", main = "Simulated")
m1.f <- kfilter(m1)
m1.s <- smoother(m1.f)
lines(m1.f$m, lty = 2)
lines(m1.s$m, lty = 3)

##3
library(sspir)
www <- 'http://elena.aut.ac.nz/~pcowpert/ts/MorgStan.dat'
morg.dat <- read.table(www, header=T) ; attach(morg.dat)
n <- length(Price); n
pr.mat <- matrix( c(Price[1:10],0,Price[11:64]), nrow=5, ncol=13 )

#var.within <- rep(0,13)
#for (i in 1:13) var.within[i] <- var(pr.mat[,i])
#var.between <- var( t(pr.mat) %*% rep(1,5)/5 )
#ss.sum1 <- var.between*12*5 + sum(var.within)*4
#ss.sum2 <- var(c(pr.mat))*64  ##ss.sum1=ss.sum2 

var.within <- rep(0,13)
for (i in 1:13) var.within[i] <- var(pr.mat[,i])
var.within[3] <- var( pr.mat[(2:5),3] )
var.within.mean <- mean(var.within); var.within.mean
#[1] 5.342303
var.between <- var( t(pr.mat) %*% rep(1,5)/5 ); var.between
#[1,] 93.21573
morg.mat <- matrix(Price, nrow = n, ncol = 1)
m1 <- SS(y = morg.mat,
         Fmat = function(tt,x,phi) return( matrix(1) ),
         Gmat = function(tt,x,phi) return( matrix(1) ),
         Wmat = function(tt,x,phi) return( matrix(93.22)),
         Vmat = function(tt,x,phi) return( matrix(5.34) ),
         m0 = matrix(25), C0 = matrix(10))
plot(m1$y, xlab = "Closing price", ylab = "Time (trading days)", main = "Simulated", type = "l")
m1.f <- kfilter(m1)
m1.s <- smoother(m1.f)
lines(m1.f$m, lty = 2)
lines(m1.s$m, lty = 3)

week <- vector(len=64)
week[1:5] <- 1
week[6:10] <- 2
week[11:14] <- 3
k <- 10
wk <- 3
for (i in 4:13) {
  k <- k + 5
  wk <- wk + 1
  week[k:(k+4)] <- wk
}
anova(aov(Price ~ factor(week)))
#Analysis of Variance Table
#Response: Price
#Df Sum Sq Mean Sq F value    Pr(>F)    
#factor(week) 12 6318.2  526.52  96.856 < 2.2e-16 ***
# Residuals    51  277.2    5.44                      
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

##7
set.seed(1)
x1 <- c(1:30)
x1 <- x1/10 + 2
a <- 4
b <- 2
n <- length(x1)
y1 <- a + b * x1 + 0.1 * rnorm(n)
x0 <- rep(1, n)
xx <- cbind(x0, x1)
F <- matrix(xx, nrow = n,ncol=2)
y <- matrix(y1, nrow = n,ncol=1)
G <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
W <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
V <- matrix(1)
m0 <- matrix(c(5,1.5), nrow = 2, ncol = 1)
C0 <- matrix(c(.1,0,0,.1), nrow = 2, ncol = 2)
a <- 0;R <- 0;f <- 0;Q <- 0;e <- 0;A <- 0;m <- 0;C <- 0;tt <- 0;
Kfilt.m <- cbind(rep(0, n), rep(0, n))
m <- m0
C <- C0
for (tt in 1:n) {
  Fmat <- matrix(c(F[tt,1],F[tt,2]), nrow = 2, ncol = 1)
  a <- G %*% m  
  R <- G %*% C %*% t(G) + W 
  f <- t(Fmat) %*% a
  Q <- t(Fmat) %*% R %*% Fmat + V
  e <- y[tt]-f
  A <- R %*% Fmat %*% solve(Q)
  m <- a + A %*% e
  C <- R - A %*% Q %*% t(A)
  Kfilt.m[tt,1] <- m[1,1]
  Kfilt.m[tt,2] <- m[2,1]
}
par(mfcol=c(2,1))
plot(Kfilt.m[1:n, 1])
plot(Kfilt.m[1:n, 2])

## use function 'kfilter'
x.mat <- matrix(xx, nrow = n, ncol = 2)
y.mat <- matrix(y1, nrow = n, ncol = 1)
m1 <- SS(y = y.mat, x = x.mat,
         Fmat = function(tt,x,phi)
           return( matrix(c(x[tt,1], x[tt,2]), nrow = 2, ncol = 1)),
         Gmat = function(tt,x,phi) return (diag(2)),
         Wmat = function(tt, x, phi) return (diag(2)),
         Vmat = function(tt,x,phi) return (matrix(1)),
         m0 = matrix(c(5,1.5),nrow=1,ncol=2),C0=0.1*diag(2)
)
m1.f <- kfilter(m1)
plot(m1.f$m[,1])
plot(m1.f$m[,2])

############################################################
