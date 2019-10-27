#the begining
library(DMwR)
library(VIM)
library(mice)
#导入数据
sample = read.csv("sample.csv",na.strings = "")
#了解数据和其缺失情况
head(sample)
sum(complete.cases(sample))
mean(!complete.cases(sample))
dev.new()
md.pattern(sample)
sample=data.frame(sample)
#删去数据缺失超过20%的行
sample=sample[-manyNAs(sample),]
#求X
sample=sample[2:253,]
attach(sample)
a=weight
b=height
a=as.numeric(as.character(a))
b=as.numeric(as.character(b))/100
BMI = a/b^2

X2= FPG
X3= sbp
X4= dbp
X5= TG
X6= sample$HDL.C
#将数据变换为数值型的
X1= BMI
X2 = as.numeric(as.character(X2))
X3 = as.numeric(as.character(X3))
X4 = as.numeric(as.character(X4))
X5 = as.numeric(as.character(X5))
X6 = as.numeric(as.character(X6))
X = data.frame(X1,X2,X3,X4,X5,X6)

#了解数据缺失信息
dim(X)
mean(is.na(X))
dev.new()
md.pattern(X)
#对X进行补全
X[is.na(X$X1),"X1"] <- mean(X$X1,na.rm = T)
sum(is.na(X))
summary(X)
#查找数据中的异常值
dev.new()
par(mfrow = c(2,3))
plot(X$X1)
text(X$X1,type="1:252")
plot(X$X2)
text(X$X2,type="1:252")
plot(X$X3)
text(X$X3,type="1:252")
plot(X$X4)
text(X$X4,type="1:252")
plot(X$X5)
text(X$X5,type="1:252")
plot(X$X6)
text(X$X6,type="1:252")
X=X[X$X1<500,]
#(1)X个变量间的相关性
library(PerformanceAnalytics)
symnum(cor(X,use="complete.obs"))
dev.new()
chart.Correlation(X)
#散布图矩阵
library(ggplot2)
library(GGally)
dev.new()
ggpairs(X, columns=1:6, aes(color="")) +  ggtitle("散布图矩阵")
cor(X)
#(2)分析患病比例有没有性别差异，与吸烟喝酒是否有关
#性别
man = X[sample$gender=="男",]
woman = X[sample$gender == "女",]
manclassfy = function(x){
#over weitht
ow = x[1]
ow[ow>=25] = 1
ow[ow!=1] = 0
#high blood sugar
highbloodsugar = x[2]
highbloodsugar[highbloodsugar>=6.1] =1
highbloodsugar[highbloodsugar!=1]=0
#hypertension
hypertension1 = x[3]
hypertension2 = x[4]
hypertension1[hypertension1>=140]=1
hypertension1[hypertension1!=1]=0
hypertension2[hypertension2>=90]=1
hypertension2[hypertension2!=1]=0
hypertension = hypertension1|hypertension2
#fasting blood
fastingblood1 = x[5]
fastingblood2 = x[6]
fastingblood1[fastingblood1>=1.7]=1
fastingblood1[fastingblood1!=1] =0
fastingblood2[fastingblood2<0.9]=1
fastingblood2[fastingblood2!=1]=0

fastingblood = fastingblood1|fastingblood2

conclusion = cbind(ow,highbloodsugar,hypertension,fastingblood)
conclusion =apply(conclusion,1,function(x) sum(x))
conclusion[conclusion<3]=0
conclusion[conclusion!=0]=1
return(conclusion)
}
womanclassfy = function(x){
  #over weitht
  ow = x[1]
  ow[ow>=25] = 1
  ow[ow!=1] = 0
  #high blood sugar
  highbloodsugar = x[2]
  highbloodsugar[highbloodsugar>=6.1] =1
  highbloodsugar[highbloodsugar!=1]=0
  #hypertension
  hypertension1 = x[3]
  hypertension2 = x[4]
  hypertension1[hypertension1>=140]=1
  hypertension1[hypertension1!=1]=0
  hypertension2[hypertension2>=90]=1
  hypertension2[hypertension2!=1]=0
  hypertension = hypertension1|hypertension2
  #fasting blood
  fastingblood1 = x[5]
  fastingblood2 = x[6]
  fastingblood1[fastingblood1>=1.7]=1
  fastingblood1[fastingblood1!=1] =0
  fastingblood2[fastingblood2<1.0]=1
  fastingblood2[fastingblood2!=1]=0
  
  fastingblood = fastingblood1|fastingblood2
  
  conclusion = cbind(ow,highbloodsugar,hypertension,fastingblood)
  conclusion =apply(conclusion,1,function(x) sum(x))
  conclusion[conclusion<3]=0
  conclusion[conclusion!=0]=1
  return(conclusion)
}
manrate = mean(manclassfy(man),na.rm = TRUE)
womanrate = mean(womanclassfy(woman),na.rm =TRUE)
manrate
womanrate

#吸烟
smoke = sample[sample$smoke == "是",]
Xa= X[sample$smoke == "是",]
mansmoke = Xa[smoke$gender == "男",]
womansmoke = Xa[smoke$gender == "女",]
output1 = c(manclassfy(mansmoke),womanclassfy(womansmoke))
smokerate = mean(output1,na.rm = TRUE)

nosmoke = sample[sample$smoke =="否",]
Xb=X[sample$smoke =="否",]
mannosmoke = Xb[nosmoke$gender == "男",]
womannosmoke = Xb[nosmoke$gender == "女",]
output2 =c(manclassfy(mannosmoke),womanclassfy(womannosmoke))
nosmokerate = mean(output2,na.rm = TRUE)
smokerate
nosmokerate
#喝酒
drunk = sample[sample$drunk == "是",]
Xc=X[sample$drunk == "是",]
mandrunk = Xc[drunk$gender=="男",]
womandrunk = Xc[drunk$gender=="女",]
output3 = c(manclassfy(mandrunk),womanclassfy(womandrunk))
drunkrate = mean(output3,na.rm = TRUE)

nodrunk = sample[sample$drunk == "无",]
Xd=X[sample$drunk == "无",]
mannodrunk = Xd[drunk$gender=="男",]
womannodrunk = Xd[drunk$gender=="女",]
output4 = c(manclassfy(mannodrunk),womanclassfy(womannodrunk))
nodrunkrate = mean(output4,na.rm = TRUE)
drunkrate
nodrunkrate
#(3)分年龄(小于等于30， 30~50, 50~70, 70以上)
#分析X中的各个指标是否有年龄上的差异？
age=as.numeric(as.character(sample$age))
Xe=X[complete.cases(age),]
samplea = sample[complete.cases(age),]
age=age[complete.cases(age)]
thirtyless = Xe[age<=30,]
thirytofifty = Xe[age>30&age<=50,]
fiftytoseventy = Xe[age>50&age<=70,]
seventymore = Xe[age>70,]
max(age)
summary(thirtyless)
summary(thirytofifty)
summary(fiftytoseventy)
summary(seventymore)
library(ggplot2)
dev.new()
par(mfrow = c(2,3))
plot(age,Xe$X1)
axis(1,c(0,30,50,70,80))
plot(age,Xe$X2)
axis(1,c(0,30,50,70,80))
plot(age,Xe$X3)
axis(1,c(0,30,50,70,80))
plot(age,Xe$X4)
axis(1,c(0,30,50,70,80))
plot(age,Xe$X5)
axis(1,c(0,30,50,70,80))
plot(age,Xe$X6)
axis(1,c(0,30,50,70,80))
#(4)计算X样本均值、样本离差阵、样本协方差和样本相关阵。
#求X均值
sapply(X,mean)
#求X协方差阵
sapply(X, sd)
dev.new()
cov(X)
#求X样本离差阵
n=6
(n-1)*cov(X)
#求X样本相关阵
cor(X)
#(5)分析X2, X3是否服从正态分布?
sum(is.na(X2))
sum(is.na(X3))
norm_test = function(X){
dev.new()
par(mfrow=c(2,1))
#Q-Q图法
qqnorm(X,main="qq图")
qqline(X)

#概率密度曲线比较法
hist(X,freq=F,main="直方图和密度估计曲线")
lines(density(X),col="blue") #密度估计曲线
x<-seq(min(X),max(X),0.0001)
lines(x,dnorm(x,mean(X),sd(X)),col="red") 
norm_test(X2)
norm_test(X3)
