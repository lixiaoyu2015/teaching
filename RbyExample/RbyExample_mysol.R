# `R by Example (Use R!)'
# By Jim Albert and Maria Rizzo
# Edition: 2012th
# Selected Exercises Solutions
# Author: Xiaoyu Li
# Created: 7/14/2014
# LinkedIn Profile: https://www.linkedin.com/in/xiaoyu-li-84435220/

############################################################
#### Chapter 2

##2.1
head(chickwts)
attach(chickwts)
boxplot(weight~feed)

##2.2
head(iris)
attach(chickwts)
by(data=iris[-5], INDICES=iris$Species, FUN=colMeans)

##2.3
?mtcars
head(mtcars)
mtcars.qtt <- mtcars[c("mpg","disp","hp","drat","wt","qsec")]
pairs(mtcars.qtt)

##2.4, 2.5, 2.12, 2.13, 2.14, 2.15
library(MASS) #load the package
head(mammals)
mammals$r <- mammals$brain / mammals$body
o = order(mammals$r)
sorted.r = mammals[o, ]
head(sorted.r)
tail(sorted.r)

plot(mammals$r, mammals$body)
plot(log(mammals$r), log(mammals$body))

b = order(mammals$brain)
sorted.brain = mammals[b, ]
head(sorted.brain)
tail(sorted.brain)

plot(mammals$body, mammals$brain, xlab="body", ylab="brain")
y = mammals[c("Cat", "Cow", "Human"), ]
polygon(y)
text(y, rownames(y), adj=c(1, .5))

d = dist(log(mammals))
h = hclust(d^2, method="ward")
plot(h)

d = dist(log(mammals))
h = hclust(d, method="complete")
plot(h)
g <- cutree(h,k=5)
g
table(g)

##2.6
LakeHuron
plot(LakeHuron)
abline(h = mean(LakeHuron))
lines(lowess(LakeHuron))
d <- diff(LakeHuron)
plot(d)
abline(h = mean(d))
lines(lowess(d))

##2.7
?randu
head(randu)
x <- runif(400)
y <- runif(400)
z <- runif(400)
xyz <- data.frame(x,y,z)
mean(xyz)
var(xyz)
diag(var(xyz))
cor(xyz)
cloud(z ~ x + y, data=xyz)
means = apply(xyz, MARGIN=1, FUN=mean)
hist(means)
hist(means, prob=TRUE)
plot(density(means))
truehist(means)
curve(dnorm(x, 1/2, sd=sqrt(1/36)), add=TRUE)
qqnorm(means)
qqline(means)

##2.10, 2.11
?faithful
hist(faithful$waiting, prob=TRUE)

lines(density(faithful$waiting))

############################################################
#### Chapter 3

##3.1
fast = scan(what="character")
Wendys McDonalds Subway Subway Subway Wendys
Wendys Subway Wendys Subway Subway Subway
Subway Subway Subway  ##'Ent'
table(fast)
prop.fast = table(fast) / length(fast)
plot(prop.fast)
barplot(prop.fast)

##3.2, 3.7
die1 <- sample(6, 1000, replace=TRUE)
die2 <- sample(6, 1000, replace=TRUE)
die.sum <- die1 + die2
table(die.sum)
prop.die.sum <- table(die.sum) / length(die.sum)
prob <- c(1:6, 5:1) / 36
cbind(prop.die.sum, prob)

max.rolls <- pmax(die1, die2)
t1 <- table(max.rolls, die.sum)
prop.table(t1, margin=1)

##3.3
k <- 0:4
p <- dbinom(k, size=4, prob=0.312)
probs = c(p[1:3], 1 - sum(p[1:3]))
hit = c(17,31,17,5)
chisq.test(hit, p=probs)
#p-value = 0.8068
k <- 0:5
p <- dbinom(k, size=5, prob=0.312)
probs = c(p[1:3], 1 - sum(p[1:3]))
hit = c(5,5,4,11)
chisq.test(hit, p=probs)
#p-value = 0.003922

##3.4, 3.5, 3.6
twn <- read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/twins.dat.txt", header=TRUE, sep=",", na.strings=".")
c.age <- cut(twn$AGE, breaks=c(0,30,40,50,100))
t1 <- table(c.age)
prop.table(t1)
barplot(prop.table(t1))
#mosaicplot(t1)

c.wage <- cut(twn$HRWAGEL, c(0, 7, 13, 20, 150))
t2 <- table(c.age, c.wage)
P <- prop.table(t2, margin=1)
barplot(t(P), ylim=c(0, 1.3), ylab="PROPORTION", legend.text=dimnames(P)$c.wage, args.legend=list(x = "top"))
barplot(t(P), beside=T, legend.text=dimnames(P)$c.wage, args.legend=list(x="top"), ylab="PROPORTION")

S = chisq.test(t2)
print(S)
#Pearson's Chi-squared test
#data:  t2
#X-squared = 24.7709, df = 9, p-value = 0.003235
S$residuals
mosaicplot(t2, shade=TRUE)

##3.8
pidigits <- read.table("http://www.itl.nist.gov/div898/strd/univ/data/PiDigits.dat", skip=60)
t1 <- table(pidigits)
t2 <- t1[-1]
t3 <- t2 / sum(t2)
barplot(t2)
probs <- rep(1/9, 9)
chisq.test(t3, p=probs)

############################################################
#### Chapter 4

##4.1, 4.2, 4.3
?cars
attach(cars)
plot(speed, dist, xlab = "Speed (mpg)", ylab = "Stopping Distance (ft)", col="red", pch=17)

fit.linear = lm(dist ~ speed, data=cars)
fit.quadratic = lm(dist ~ speed + I(speed^2), data=cars)
plot(speed, dist)
abline(fit.linear, lty = "dotted", lwd = 2)
abline(fit.quadratic, lty = "longdash", lwd = 2)
legend("topleft", legend=c("linear fit", "quadratic fit" ), lty=c(3,5), lwd=2)

plot(speed, fit.linear$residual)
abline(h=0, col = "blue", lwd = 3)
text(13, 42, "POS", col = "blue")
text(23, 43, "POS", col = "blue")
text(19, -29, "NEG", col = "red")
identify(speed, fit.linear$residual, n=2, labels=speed)
#[1] 17 31

##4.4
?mtcars
attach(mtcars)
par(mfrow=c(2,2))
plot(mpg, disp)
plot(mpg, wt)
plot(mpg, hp)
plot(mpg, drat)
par(mfrow=c(1,1))

##4.5
house=function(x, y, ...){
  lines(c(x - 1, x + 1, x + 1, x - 1, x - 1),
        c(y - 1, y - 1, y + 1, y + 1, y - 1), ...)
  lines(c(x - 1, x, x + 1), c(y + 1, y + 2, y + 1), ...)
  lines(c(x - 0.3, x + 0.3, x + 0.3, x - 0.3, x - 0.3),
        c(y - 1, y - 1, y + 0.4, y + 0.4, y - 1), ...)
}
plot.new()
plot.window(xlim=c(0, 10), ylim=c(0, 10))
house(1, 1)
house(4, 2)
house(6, 7)
house(2, 7, col="red", lwd=3)
house(8, 1, col=2, lty=2)
house(9, 4.5, col=3, lty=3)
house(9, 8, col=4, lty=4)
box()

##4.6
curve(dbeta(x, 2, 6), from=0, to=1)
curve(dbeta(x, 4, 4), from=0, to=1, add=TRUE)
curve(dbeta(x, 6, 2), from=0, to=1, add=TRUE)
title(expression(f(y)==frac(1,B(a,b))*y^{a-1}*(1-y)^{b-1}))
text(0.05, 2.5, "a=2, b=6")
text(0.5, 2.3, "a=4, b=4")
text(0.96, 2.5, "a=6, b=2")
curve(dbeta(x, 2, 6), from=0, to=1)
curve(dbeta(x, 4, 4), from=0, to=1, col=2, add=TRUE)
curve(dbeta(x, 6, 2), from=0, to=1, col=3, add=TRUE)
title(expression(f(y)==frac(1,B(a,b))*y^{a-1}*(1-y)^{b-1}))
legend("top", legend=c("a=2, b=6", "a=4, b=4", "a=6, b=2"), col=1:3, lwd=1)

##4.7, 4.8
library(lattice)
?faithful
faithful$length = ifelse(faithful$eruptions < 3.2, "short", "long")
bwplot(eruptions ~ length, data=faithful)
densityplot(~ waiting, group=length, data=faithful, auto.key=list(space="top"))

library(ggplot2)
p1 <- ggplot(faithful, aes(x = waiting, color = length))
p1 + geom_density()

p2 <- ggplot(faithful, aes(y = waiting, x = length))
p2 + geom_boxplot()

############################################################
#### Chapter 5

##5.1, 5.2, 5.3, 5.5, 5.6, 5.7
dat = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/college.txt", header=TRUE, sep="\t")
college = subset(dat, complete.cases(dat))
stripchart(college$Pct.20, method="stack", pch=19, xlab="percentages of small classes")
#identify(college$Pct.20, n=1, labels=college$School)   ##not work
b.output = boxplot(college$Pct.20, horizontal=TRUE)
b.output$stats
abline(v=45)

plot(college$Pct.20, college$Pct.50)
fit = line(college$Pct.20, college$Pct.50)
coef(fit)
#[1] 22.535088 -0.245614
abline(fit)
22.535088-0.245614*60
#[1] 7.798248
fit2 = lm(college$Pct.50 ~ college$Pct.20)
plot(college$Pct.20, fit2$residuals)
abline(h=0)
#identify(college$Pct.20, college$Pct.50, n=7, labels=college$School)   ##not work well

plot(college$Accept.rate, college$Top.10)
fit3 = line(college$Accept.rate, college$Top.10)
coef(fit3)
[1] 117.132530  -1.277108
abline(fit3)
fit4 = lm(college$Top.10 ~ college$Accept.rate)
plot(college$Accept.rate, fit4$residuals)
abline(h=0)

hist(college$Full.time)
hist( sqrt(college$Full.time) )
hist( log(college$Full.time) )

stripchart(college$Alumni.giving, method="stack", pch=19, xlab="percentage of alumni's financial contributions")
#identify(college$Alumni.giving, n=3, labels=college$Alumni.giving)   ##not work
roots = sqrt(college$Alumni.giving)
logs = log(college$Alumni.giving)
stripchart(roots, method="stack", pch=19)
stripchart(logs, method="stack", pch=19)  ##more symmetric
hist(roots)
hist(logs)

stripchart(Alumni.giving ~ Tier, method="stack", pch=19,
           xlab="Alumni Giving Percentage", ylab="Tier", 
           data=college)
stripchart(sqrt(Alumni.giving) ~ Tier, method="stack", pch=19, data=college)
stripchart(log(Alumni.giving) ~ Tier, method="stack", pch=19, data=college)
boxplot(sqrt(Alumni.giving) ~ Tier, data=college, horizontal=TRUE)
boxplot(log(Alumni.giving) ~ Tier, data=college, horizontal=TRUE)  
##The log transformation is more successful in making the spreads approximately the same between groups.

##5.4
xy <- matrix( c( c(1955, 2653), c(1956, 2918), c(1957, 3324), c(1959, 3640), c(1961, 4145), c(1963, 4780), c(1964, 5280), 
                 c(1965, 5921), c(1966, 6390), c(1967, 6912), c(1968, 7513), c(1969, 8005), c(1970, 8581) ),
              byrow=T, ncol=2)
xy <- data.frame(Year=xy[,1], Enrollment=xy[,2])
fit <- lm(xy$Enrollment ~ xy$Year)
plot(xy$Year, fit$residuals)
abline(h=0)
fit <- lm(log(xy$Enrollment) ~ xy$Year)
plot(xy$Year, fit$residuals)
abline(h=0)

############################################################
#### Chapter 6

##6.1
y <- 120; n <- 276
Test <- prop.test(y, n, p=0.375, alternative="two.sided", correct=FALSE)
Test
Test$conf.int
#[1] 0.3775897 0.4937660
Test.exact <- binom.test(y, n, p=0.375)
Test.exact
Test.exact$conf.int
#[1] 0.3754670 0.4955137

##6.2, 6.3
mrt <- read.csv("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/nyc-marathon.csv")
women.marathon <- subset(mrt, Gender=="female")
t.test(women.marathon$Age, mu=36.1, conf.int=TRUE)
wilcox.test(women.marathon$Age, mu=36.1, conf.int=TRUE)

men.marathon <- subset(mrt, Gender=="male")
t.test(men.marathon$Age, women.marathon$Age, alternative = c("greater"), conf.int=TRUE, conf.level=0.90)
wilcox.test(men.marathon$Age, women.marathon$Age, alternative = c("greater"), conf.int=TRUE, conf.level=0.90)

##6.4
meas = scan()
22 18 27 23 24 15 26 22 24 25 24 18
18 26 20 24 27 16 30 22 17 18 22 26
#Press "Enter"
t.test(meas, mu=26, conf.int=TRUE, conf.level=0.90)
hist(meas, prob=TRUE)
curve(dnorm(x, mean=mean(meas), sd=sd(meas)), add=TRUE)
mt <- table(meas)
#mosaicplot(mt)
mk <- c(15, 16, 17, 18, 20, 22, 23, 24, 25, 26, 27, 30)
mp <- pnorm(mk+1/2,mean=mean(meas),sd=sd(meas)) - pnorm(mk-1/2,mean=mean(meas),sd=sd(meas))
chisq.test(mt, p=mp/sum(mp))  ##p-value = 0.722
mp <- pnorm(mk+1/2,mean=26,sd=sd(meas)) - pnorm(mk-1/2,mean=26,sd=sd(meas))
chisq.test(mt, p=mp/sum(mp))  ##p-value = 2.788e-06

##6.5
bcs <- read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/buffalo.cleveland.snowfall.txt", header = TRUE)
head(bcs)
diff <- bcs$Buffalo - bcs$Cleveland
t.test(diff, mu=0, conf.int=TRUE)

##6.6
ei <- read.csv("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/Etruscan-Italian.csv")
head(ei)
t.test(x ~ group, data=ei, conf.int=TRUE)
wilcox.test(x ~ group, data=ei, conf.int=TRUE)

##6.7
winner <- c(185, 182, 182, 188, 188, 188, 185, 185, 177,
           182, 182, 193, 183, 179, 179, 175)
opponent <- c(175, 193, 185, 187, 188, 173, 180, 177, 183,
             185, 180, 180, 182, 178, 178, 173)
t.test(winner, opponent, paired=TRUE)

############################################################
#### Chapter 7

##7.1, 7.2, 7.3, 7.4
library(MASS)
head(mammals)
L1 <- lm(mammals$brain ~ mammals$body)
#Coefficients:
#  (Intercept)  mammals$body  
#91.0044        0.9665 
plot(L1, which=1, add.smooth=FALSE)
mammals[19, ]

plot(log(mammals$body), log(mammals$brain),
     xlab="log(body)", ylab="log(brain)")
L2 <- lm( log(mammals$brain) ~ log(mammals$body) ); L2
#Coefficients:
#  (Intercept)  log(mammals$body)  
#2.1348             0.7517
abline(L2)

plot(L2, which=1:2)
hist(L2$resid, prob=TRUE)
curve(dnorm(x, mean=0, sd=sd(L2$resid)), add=TRUE)

summary(L2)
res.var <- sd(L2$resid)^2
#[1] 0.4741428
1-res.var/sd(log(mammals$brain))^2
#[1] 0.9207837
cor(log(mammals$brain), log(mammals$body))^2
#[1] 0.9207837
summary(L2)$r.squared
#[1] 0.9770912

##7.5
dat <- scan()
.032	170
.034	290
.214	-130
.263	-70
.275	-185
.275	-220
.45	200
.5	290
.5	270
.63	200
.8	300
.9	-30
.9	650
.9	150
.9	500
1.0	920
1.1	450
1.1	500
1.4	500
1.7	960
2.0	500
2.0	850
2.0	800
2.0	1090
mat <- matrix(dat, ncol=2, byrow=TRUE)
hubble <- data.frame(distance=mat[,1], recession_velocity=mat[,2])
L <- lm(hubble$recession_velocity ~ 0 + hubble$distance); L$coef
#hubble$distance 
#423.9373

##7.7, 7.8
head(cars)
L1 <- lm(cars$dist ~ cars$speed); summary(L1)$r.squared
#[1] 0.6510794
L2 <- lm(cars$dist ~ 0 + cars$speed); summary(L2)$r.squared
#[1] 0.8962893

speed2 <- cars$speed^2
L3 <- lm(cars$dist ~ cars$speed + speed2); L3
plot(cars$speed, cars$dist)
curve(2.47014 + 0.91329 * x + 0.09996 * x^2, add = TRUE)

##7.9
head(trees)
M1 = lm( trees$Volume ~ trees$Girth + I(trees$Girth^2) ); M1
plot(trees$Girth, trees$Volume)
curve(10.7863 + -2.0921 * x + 0.2545 * x^2, add = TRUE)
plot(M1, which=1)

##7.10
lunatics = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/lunatics.txt", header=TRUE)
attach(lunatics)
RDIST = 1/DIST
M1 = lm(PHOME ~ RDIST); M1
#Coefficients:
#  (Intercept)        RDIST  
#73.93      -266.32 
plot(M1$fitted, M1$resid, xlab="fitted", ylab="residuals")
abline(h=0, lty=2)
detach(lunatics)
##with NANTUCKET and DUKES deleted:
lunatics2 <- lunatics[-c(13,14), ]
attach(lunatics2)
RDIST = 1/DIST
M2 = lm(PHOME ~ RDIST); M2
#Coefficients:
#  (Intercept)        RDIST  
#79.36      -305.52 
plot(M2$fitted, M2$resid, xlab="fitted", ylab="residuals")
abline(h=0, lty=2)
detach(lunatics2)

##7.11
twn = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/twins.dat.txt", header=TRUE,
                 sep=",", na.strings=".")
head(twn)
L <- lm(twn$DLHRWAGE ~ 0 + log(twn$HRWAGEL)); L
plot(log(twn$HRWAGEL), twn$DLHRWAGE)
abline(L)

############################################################
#### Chapter  8

##8.1
y1 = c(22, 26)
y2 = c(28, 24, 29)
y3 = c(29, 32, 28)
y4 = c(23, 24)
y = c(y1, y2, y3, y4)
Model = c(rep("A", 2), rep("B", 3), rep("C", 3), rep("D", 2))
mileages = data.frame(y, Model)
str(mileages)
mileages
oneway.test(mileages$y ~ mileages$Model)
#At alpha = 0.05 significance, the four models (A, B, C, D) have the same gas mileage.

##8.2
head(PlantGrowth)
str(PlantGrowth)
L = lm(PlantGrowth$weight ~ PlantGrowth$group)
anova(L)
M = aov(PlantGrowth$weight ~ PlantGrowth$group)
TukeyHSD(M)

##8.3, 8.4, 8.7
head(iris)
attach(iris)
boxplot(Sepal.Length ~ Species, ylab = "Sepal.Length")
stripchart(Sepal.Length ~ Species, vertical=TRUE)
meansd = function(x) c(mean=mean(x), sd=sd(x))
by(Sepal.Length, Species, FUN=meansd)
L <- lm(Sepal.Length ~ Species)
anova(L)
summary(L)

plot(L, which=1:2)

n = table(Species) #sample sizes
a = length(n) #number of levels
N = sum(n) #total sample size
SS.total = (N - 1) * var(Sepal.Length)
vars = by(Sepal.Length, Species, var) #within-sample variances
SSE = sum(vars * (n - 1))  
SST = SS.total - SSE
print(c(a - 1, N - a, N - 1)) #degrees of freedom
print(c(SST, SSE, SS.total)) #SST, SSE, SS.total
MST = SST / (a - 1)
MSE = SSE / (N - a)
statistic = MST / MSE
p.value = pf(statistic, df1=a-1, df2=N-a, lower.tail=FALSE)
print(as.data.frame(list(MST=MST, MSE=MSE, F=statistic, p=p.value)))
detach(iris)

##8.5
times = read.csv("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/PATIENT.csv")
head(times)
times1 = stack(times)
names(times1)
names(times1) = c("time", "organ")
times1 = na.omit(times1)
L1 <- lm(times1$time ~ times1$organ)
plot(L1, which=1:2)
L2 <- lm(log(times1$time) ~ times1$organ)
plot(L2, which=1:2)
anova(L2)
M2 <- aov(log(times1$time) ~ times1$organ)
TukeyHSD(M2)

##8.6
wru = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/wasterunup.txt", header=TRUE, na.strings="*")
wru
wru1 = stack(wru)
names(wru1)
names(wru1) = c("waste", "plant")
wru1 = na.omit(wru1)
boxplot(wru1$waste ~ wru1$plant, ylab = "waste")
L <- lm(wru1$waste ~ wru1$plant)
anova(L)
plot(L$fit, L$res)
abline(h=0)
qqnorm(L$res)
qqline(L$res)

############################################################
#### Chapter 9

##9.1
rounding = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/rounding.txt", header=TRUE)
str(rounding)
names(rounding)[3] = "player"
rounding$player <- as.factor(rounding$player)
L <- aov(times ~ method + player, data=rounding)
plot(L, which=1:2)

##9.2
str(morley)
morley$Expt <- factor(morley$Expt)
morley$Run <- factor(morley$Run)
boxplot(Speed ~ Expt, xlab="Expt", ylab="Speed", data=morley)
L <- aov(Speed ~ Expt + Run, data=morley)
anova(L)  ##summary(L)
plot(L, which=1:2)
model.tables(L, cterms="Expt")
CIs = TukeyHSD(L, which=1); CIs
plot(CIs, las=1)
with(data=morley, expr={interaction.plot(Run, Expt, response=Speed)})
pairwise.t.test(morley$Speed, morley$Expt)  ##significantly difference in Expt 1&4, 1&5.

##9.3, 9.4
poison = read.csv("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/poison.csv")
L1 = aov(Time ~ Poison * Treatment, data = poison)
anova(L1)
plot(L1, which=1:2)

L2 = aov(1/Time ~ Poison * Treatment, data = poison)
anova(L2)
plot(L2, which=1:2)
with(data=poison, expr={
  interaction.plot(Poison, Treatment, response=1/Time)
  interaction.plot(Treatment, Poison, response=1/Time)
})
model.tables(L2, type="means")
TukeyHSD(L2, which=c("Poison", "Treatment"))
plot(TukeyHSD(L2, which=c("Poison", "Treatment")))
pairwise.t.test(1/poison$Time, poison$Poison)
pairwise.t.test(1/poison$Time, poison$Treatment)

############################################################
#### Chapter 10

##10.1
str(PlantGrowth)
boxplot(weight ~ group, xlab="group", ylab="weight", data=PlantGrowth)
rand.oneway = function(response, group, R=199) {
  test = oneway.test(response ~ group)
  observed.F <- test$statistic
  stats = replicate(R, {
    random.labels = sample(group)
    oneway.test(response ~ random.labels)$statistic})
  p = sum(stats >= observed.F) / (R+1)
  test$method = "Randomization test for equal means"
  test$p.value = p
  test
}
rand.oneway(response=PlantGrowth$weight, group=PlantGrowth$group, R=999)
L = lm(PlantGrowth$weight ~ PlantGrowth$group)
anova(L)

##10.2
flicker = read.table(file="http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/flicker.txt",
                     header=TRUE)
rand.oneway = function(response, group, R=199) {
  test = oneway.test(response ~ group)
  observed.F <- test$statistic
  stats = replicate(R, {
    random.labels = sample(group)
    oneway.test(response ~ random.labels)$statistic})
  p = sum(stats >= observed.F) / (R+1)
  test$method = "Randomization test for equal means"
  test$p.value = p
  test
}
rand.oneway(response=flicker$Flicker, group=flicker$Colour, R=999)
L = lm(flicker$Flicker ~ flicker$Colour)
anova(L)

##10.4
head(airquality)
airquality$Month <- factor(airquality$Month)
L = lm(airquality$Ozone ~ airquality$Month)
plot(L, which=1:2)
anova(L)
library(coin)
oneway_test(Ozone ~ Month, data=airquality, distribution=approximate(B=999))
detach(package:coin)

##10.5, 10.6, 10.7
web = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/webhits.txt", header=TRUE)
str(web)
web$Season <- character(length = 35)
web$Season[web$Week>=1 & web$Week<=9] <- c("spring")
web$Season[web$Week>=10 & web$Week<=22] <- c("summer")
web$Season[web$Week>=23 & web$Week<=35] <- c("fall")
rand.oneway = function(response, group, R=199) {
  test = oneway.test(response ~ group)
  observed.F <- test$statistic
  stats = replicate(R, {
    random.labels = sample(group)
    oneway.test(response ~ random.labels)$statistic})
  p = sum(stats >= observed.F) / (R+1)
  test$method = "Randomization test for equal means"
  test$p.value = p
  test
}
rand.oneway(response=web$Hits, group=web$Season, R=999)
L = lm(web$Hits ~ web$Season)
anova(L)

rand.correlation = function(x, y, R=199) {
  ranks.x = rank(x)
  ranks.y = rank(y)
  observed.r = cor(ranks.x, ranks.y)
  stats = replicate(R, {
    random.ranks = sample(ranks.y)
    cor(ranks.x, random.ranks)
  })
  p.value = sum(stats >= observed.r) / (R + 1)
  hist(stats, prob = TRUE)
  abline(v = observed.r)
  list(observed.r = observed.r, p.value = p.value)
}
rand.correlation(web$Week, web$Hits, R=1000)
#$observed.r
#[1] 0.3217489
#$p.value
#[1] 0.02397602

rand.correlation = function(x, y, R=199) {
  ranks.x = rank(x)
  ranks.y = rank(y)
  observed.r = cor(ranks.x, ranks.y)
  stats = replicate(R, {
    random.ranks = sample(ranks.y)
    cor(ranks.x, random.ranks, method = c("spearman"))
  })
  p.value = sum(stats >= observed.r) / (R + 1)
  list(observed.r = observed.r, p.value = p.value)
}
rand.correlation(web$Week, web$Hits, R=1000)
#$observed.r
#[1] 0.3217489
#$p.value
#[1] 0.02497502

##10.8
rand.correlation = function(x, y, R=199) {
  observed.r = cor(x, y)
  stats = replicate(R, {
    random.y = sample(y)
    cor(x, random.y)
  })
  p.value = sum(stats >= observed.r) / (R + 1)
  hist(stats, prob = TRUE)
  abline(v = observed.r)
  list(observed.r = observed.r, p.value = p.value)
}
x <- rnorm(50)
y <- rnorm(50)
rand.correlation(x, y, R=1000)
#$observed.r
#[1] 0.07270599
#$p.value
#[1] 0.3266733

############################################################
#### Chapter 11

##11.1
simu = function(n=20){
  bet=sample(c(5, -5), size=n, replace=TRUE, prob=c(18/38, 20/38))
  sum(bet)
}
F = replicate(100, simu())
sum(F>0)/100
#[1] 0.33
1-pbinom(10, 20, 18/38)
#[1] 0.3223419
cumu.pos = function(n=20){
  winnings=sample(c(5, -5), size=n, replace=TRUE, prob=c(18/38, 20/38))
  cum.win = cumsum(winnings)
  sum(cum.win>0)
}
S = replicate(500, cumu.pos())
table(S)
plot(table(S))

##11.2
scramble.hats = function(n=20){
  hats = c(rep(1,n/2), rep(2,n/2))
  mixed.hats = sample(hats)
  sum(hats == mixed.hats)
}
scramble.hats()
matches = replicate(1000, scramble.hats())
plot(table(matches))
sum(matches>=10)/1000
#[1] 0.674
mean(matches)
#[1] 9.942

#11.3
collector=function(N=365, n){
  samp.births = sample(1:N, size=n, replace=TRUE)
  length(unique(samp.births))
}
collector(n=30)
F = replicate(1000, collector(n=30))
plot(table(F))
sum(F<30)/1000
#[1] 0.711
perm = function(n, m){
  if (n>=m & m>0) {
    prod = 1
    for (i in n:(n-m+1)) prod = prod * i
    prod
  }
  else print(0)
}
p.exact = 1 - perm(n=365, m=30) / 365^30
#[1] 0.7063162
pbirthday(n=30)
#[1] 0.7063162

##11.4
switches = function(y) sum(abs(diff(y)))
dat = read.table("http://personal.bgsu.edu/~mrizzo/Rx/Rx-data/utley2006.txt", header=TRUE, sep="\t")
utley = as.numeric(dat$H > 0)
switches(utley)
#[1] 60
random.switches = function(y){
  mixed.up.y = sample(y)
  switches(mixed.up.y)
}
L = replicate(10000, random.switches(utley))
hist(L)
abline(v=60, lwd=3)
text(58, 1500, "Utley")

##11.5
collector = function(N=50, n){
  samp.states = sample(1:N, size=n, replace=TRUE)
  length(unique(samp.states))
}
F = replicate(1000, collector(n=100))
plot(table(F))
sum(F>=45)/1000
#[1] 0.271
mean(F)
#[1] 43.215
cost = function(N=50, n){
  samp.states = sample(1:N, size=n, replace=TRUE)
  m = length(unique(samp.states))
  2 * (50 - m)
}
M = replicate(1000, collector(n=100))
plot(table(M))
mean(M)
#[1] 43.377

##11.6
mosteller = function(N=10, n=2){
  students = 1:N
  m = matrix(0, nrow=N, ncol=N)
  for (j in 1:N) {
    s = sample(students[-j], size=n)
    m[j, s] = 1
  }
  sum(colSums(m)==0)
}
F = replicate(100, mosteller())
table(F)
#F
#0  1  2  3 
#26 40 26  8 
prop.table(table(F))
#F
#0    1    2    3 
#0.26 0.40 0.26 0.08

############################################################
#### Chapter 12

##12.1, 12.2
mu0 = 6
tau0 = sqrt(0.25)
curve( dnorm(x, mean=mu0, sd=tau0), from=0.0, to=12.0 )
qt = seq(0, 1, 1/4)
qnorm(qt, mean=mu0, sd=tau0)
#[1]     -Inf 5.662755 6.000000 6.337245      Inf
1 - pnorm(7, mean=mu0, sd=tau0)
#[1] 0.02275013

n = 24
y.bar = 7.688
sigma.sq = 2.0
tau1.sq = 1 / ( 1/tau0^2 + n/sigma.sq ); tau1.sq
#[1] 0.0625
mu1 = tau1.sq * ( mu0/tau0^2 + n*y.bar/sigma.sq ); mu1
#[1] 7.266
tau1 = sqrt(tau1.sq)
curve( dnorm(x, mean=mu1, sd=tau1), from=0.0, to=12.0, col=1 )  ##black: posterior
curve( dnorm(x, mean=mu0, sd=tau0), from=0.0, to=12.0, add=TRUE, col=2 )  ##red: prior
CI.post = c( qnorm(0.05, mean=mu1, sd=tau1), qnorm(0.95, mean=mu1, sd=tau1) ); CI.post
#[1] 6.854787 7.677213
1 - pnorm(7, mean=mu1, sd=tau1)
#[1] 0.8563357

##12.3, 12.4, 12.5, 12.6
spacings <- scan()
0 2 0 4 1 0 2 0 1 0 0 1 1 3 1 0 0 0 1
6 0 9 0 4 1 9 1 0 3 4 5 5 1 0 2 4 0 4
0 3 2 1 0 1 3 7 0 3 1 2 14 4 0 1 6 1 10
1 2 0 1 0 4 5 0 7 3 1 2 1 2 1 2 2 4 3
3 1 1 2 1 2 7 0 3 1 2 2 2 2 0 3 4 1 1
0 0 1 1 1 11 2 2 1 3 1 0 1 2 1 1 1 0 0
2 0 10 1 2 2 1 1 3 1 1 0 0 1 0 1 0 1 1
0 1 0 0 0 2 1 4 5 5 0 0 0 0 2 0 8 5 2
11 8 0 7 1 3 1
n = length(spacings); n
#[1] 159
s = sum(spacings); s
#[1] 336
curve( x^n*(1-x)^s, from=0.2, to=0.5 )
p.hat = n/(s+n); p.hat  ##MLE of p
#[1] 0.3212121

a0 = 44
b0 = 102
a1 = a0 + n
b1 = b0 + s
curve( dbeta(x, shape1=a1, shape2=b1), from=0.0, to=1.0, col=1 )   ##black: posterior
curve( dbeta(x, shape1=a0, shape2=b0), from=0.0, to=1.0, add=TRUE, col=2 )  ##red: prior
qbeta(0.5, shape1=a1, shape2=b1)
#[1] 0.3165019
CI.post = c( qbeta(0.025, shape1=a1, shape2=b1), qbeta(0.975, shape1=a1, shape2=b1) ); CI.post
#[1] 0.2812654 0.3532033

sim.post = rbeta(1000, shape1=a1, shape2=b1)
hist(sim.post)
sme = mean(sim.post); sme
#[1] 0.3166424
ssd = sd(sim.post); ssd
#[1] 0.01818837
CI.sim = quantile(sim.post,c(0.025, 0.975)); CI.sim
#     2.5%     97.5% 
#0.2810239 0.3523739

betalogpost = function(p, a, b) dbeta(p, a, b, log=TRUE)
metrop.hasting.rw = function(logpost, current, C, iter, ...){
  S = rep(0, iter); n.accept = 0
  for(j in 1:iter){
    candidate = runif(1, min=current - C, max=current + C)
    prob = exp(logpost(candidate, ...) - logpost(current, ...))
    accept = ifelse(runif(1) < prob, "yes", "no")
    current = ifelse(accept == "yes", candidate, current)
    S[j] = current; n.accept = n.accept + (accept == "yes")
  }
  list(S=S, accept.rate=n.accept / iter)
}
sim1 = metrop.hasting.rw(betalogpost, 0.2, 0.1, 1000, a1, b1)
ts.plot(sim1$S)
sim1$accept.rate
#[1] 0.286
#Generally, acceptance rates between 20 and 40 percent are believed to produce acceptable simulated samples 
#in the random walk M-H algorithm.
mean(sim1$S)
#[1] 0.3183207
a1/(a1+b1)  ##(n+44)/(n+s+44+102)
#[1] 0.3166927
sim2 = metrop.hasting.rw(betalogpost, 0.2, 0.01, 1000, a1, b1)
ts.plot(sim2$S)
sim2$accept.rate
#[1] 0.875
sim3 = metrop.hasting.rw(betalogpost, 0.2, 0.30, 1000, a1, b1)
ts.plot(sim3$S)
sim3$accept.rate
#[1] 0.113

############################################################
#### Chapter 13

##13.1
time = function(n=5, mu=20, sd=4){
  sim=rnorm(n, mean=mu, sd=sd)
  sum(sim) 
}
time()
#[1] 94.40572
time.sim = function(N=1000){
  sim=replicate(N, time())
  sim.mean=mean(sim)
  sim.mean.sd=sd(sim)/sqrt(N)
  c(time.mean=sim.mean, time.mean.sd=sim.mean.sd)
}
time.sim()
#   time.mean time.mean.sd 
#99.8176478   0.2851638
late = function(n=5, mu=20, sd=4){
  sim=rnorm(n, mean=mu, sd=sd)
  sum(sim>30)/n
}
late()
#[1] 0
late.sim = function(N=1000){
  sim=replicate(N, late())
  sim.mean=mean(sim)
  sim.mean.sd=sd(sim)/sqrt(N)
  c(late.mean=sim.mean, late.mean.sd=sim.mean.sd)
}
late.sim()
#late.mean late.mean.sd 
#0.007600000  0.001209836 
long = function(n=5, mu=20, sd=4){
  sim=rnorm(n, mean=mu, sd=sd)
  max(sim) 
}
long()
#[1] 27.20005
long.sim = function(N=1000){
  sim=replicate(N, long())
  sim.mean=mean(sim)
  sim.mean.sd=sd(sim)/sqrt(N)
  c(long.mean=sim.mean, long.mean.sd=sim.mean.sd)
}
long.sim()
#   long.mean long.mean.sd 
#24.5418348    0.0806558 

##13.2
quan = function(n=20){
  sim=rnorm(n)
  Q1=quantile(sim, 0.25)
  M=quantile(sim, 0.50)
  Q3=quantile(sim, 0.75)
  M/(Q3-Q1)
}
quan()
#50% 
#-0.06890326  
quan.sim = function(N=1000){
  sim=replicate(N, quan())
  c(sim=sim)
}
s = quan.sim()
s1 = quantile(s, 0.05)
s2 = quantile(s, 0.95)
c(s1, s2)
#        5%        95% 
#-0.3562751  0.3867103
Q1 = 37.8; M = 51.3; Q3 = 58.2
mu1 = s2*(Q3-Q1)-M; mu2 = s1*(Q3-Q1)-M
c(mu1, mu2)
#-43.37359 -58.56801

##13.3
varbias = function(n=5, sd=4){
  sim=rnorm(n, mean=0, sd=sd)
  sd(sim)^2-sd^2 
}
varbias()
#[1] -6.697928
varbias.sim = function(N=1000){
  sim=replicate(N, varbias())
  sim.mean=mean(sim)
  sim.mean.sd=sd(sim)/sqrt(N)
  c(varbias.mean=sim.mean, varbias.mean.sd=sim.mean.sd)
}
varbias.sim()
#   varbias.mean varbias.mean.sd 
#-0.3374563       0.3541504
sc = function(sid, c, n=5){
  sim=rnorm(n, mean=0, sd=sid)
  sd(sim)^2 * (n-1) / c
}
sc.sim = function(siid, ci, N=1000){
  sim=replicate(N, sc(sid=siid, c=ci))
  mse=mean( (sim-siid^2)^2 )
  mse
}
sca = rep(0, 4)
for (i in 1:4) {
  j=2*i+1
  sca[i]=sc.sim(siid=1, ci=j)
}
sca
#[1] 1.0665590 0.3271243 0.3578582 0.4095096
##We choose c=5.

##13.4
plusfour = function(y ,n, prob){
  p = (y+2) / (n+4)
  z = qnorm(1 - (1 - prob) / 2)
  lb = p - z * sqrt(p * (1 - p) / n)
  ub = p + z * sqrt(p * (1 - p) / n)
  cbind(lb, ub)
}
mc.coverage = function(p, n, prob, iter=10000){
  y = rbinom(iter, n, p)
  c.interval = plusfour(y, n, prob)
  mean((c.interval[ ,1] < p) & (p < c.interval[ ,2]))
}
many.mc.coverage = function(p.vector, n, prob)
  sapply(p.vector, mc.coverage, n, prob)

curve(many.mc.coverage(x, 100, 0.90), from=0.001, to=0.999,
      xlab="p", ylab="Coverage Probability",
      main=paste("n=", 100, ", prob=", 0.90),
      ylim=c(0.7, 1))
abline(h=.9)

##13.5
log.dcauchy = function(y, a.vector) {
  n=length(a)
  s=0
  for (j in 1:n) s=s+dcauchy(y, location=a[j], log=TRUE)
  s
}
metrop.hasting.rw = function(logf, current, C, iter, ...){
  S = rep(0, iter); n.accept = 0
  for(j in 1:iter){
    candidate = rnorm(1, mean=current, sd=C)
    prob = exp(logf(candidate, ...) - logf(current, ...))
    accept = ifelse(runif(1) < prob, "yes", "no")
    current = ifelse(accept == "yes", candidate, current)
    S[j] = current; n.accept = n.accept + (accept == "yes")
  }
  list(S=S, accept.rate=n.accept / iter)
}
a = c(1,2,2,6,7,8)
metrop.hasting.rw(log.dcauchy, 1, 1, 10000, a)$accept.rate
#[1] 0.6726
metrop.hasting.rw(log.dcauchy, 1, 2, 10000, a)$accept.rate
#[1] 0.5367
metrop.hasting.rw(log.dcauchy, 1, 3, 10000, a)$accept.rate
#[1] 0.4529
metrop.hasting.rw(log.dcauchy, 1, 4, 10000, a)$accept.rate
#[1] 0.3838
mcmc.sample = metrop.hasting.rw(log.dcauchy, 1, 4, 10000, a)
plot(density(mcmc.sample$S))
s=mcmc.sample$S
mean((s>6)&(s<8))
#[1] 0.2274

##13.6
#f(x) ~ Gamma(shape=a, rate=b)
#f(y|x) ~ Poisson(x)
random.coin.gibbs = function(m=1000){
  S = matrix(0, m, 2)
  dimnames(S)[[2]] = c("x", "y")
  for(j in 1:m){
    x = rgamma(1, shape=1, rate=1)
    y = rpois(1, lambda=x)
    S[j, ] = c(x, y)
  }
  return(S)
}
sim.values = random.coin.gibbs()
plot(sim.values)
y = sim.values[ ,"y"]
table(y)
plot(table(y))
mean(y)
#[1] 0.997

############################################################