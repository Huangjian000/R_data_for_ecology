# data()
# iris
# install.packages('ade4')
# library(ade4)
# data('doubs')
# 
# cor()
# pcor()
# Hmis()

myData <- iris[,1:4]
head(myData)

#计算pearson相关系数
cor(x = myData[,"Sepal.Length"],y=myData[,"Sepal.Width"],
    use = "everything",method="pearson")

#计算显著性检验
cor.test(x = myData[,"Sepal.Length"],y=myData[,"Sepal.Width"])

res <- cor.test(x = myData[,"Sepal.Length"],y=myData[,"Sepal.Width"])

res$p.value  #提取P值
res$conf.int  #置信区间
library(corrplot)  #加载绘图包
res_cor <- cor(myData)
corrplot::corrplot(res_cor)

#对整个矩阵单个变量进行计算并检验
cor(myData)
install.packages("psych")
library(psych)  #需要使用psych包中的corr.test,所有需要加载
corr.test(myData,use="complete",method="pearson",adjust="none")
res <- corr.test(myData,use="complete",method="pearson",adjust="none")
res$r
res$ci


myData <- read.csv('../0309work_dir/data/sample.csv',header=TRUE)
myData

x1 <- myData$food
x1
x2 <- myData$eggs
x2
x3 <- myData$workers
x3
y1 <- myData$foragers
y1
y2 <- myData$distance
y2
test <- data.frame(x1,x2,x3,y1,y2)  
test
test <- scale(test)  #standard
test
pairs(test)  #san dot plot 
cor.test(x1,x1)
cor.test(x1,x2)
cor.test(x1,x3)
cor.test(x1,y1)
cor.test(x1,y2)
cor.test(x2,y1)
cor.test(x2,y2)
cor.test(x3,y1)
cor.test(x3,y2)
cor.test(y1,y2)

sc_test <- cor(test)
corrplot::corrplot(sc_test)

ca <- cancor(test[,1:3],test[,4:5])  #no center
ca
ca <- cancor(test[,1:3],test[,4:5],ycenter=TRUE)  #no center
ca

U <- as.matrix(test[,1:3])%*% ca$xcoef

V <- as.matrix(test[,4:5])%*% ca$ycoef

par(mfrow=c(1,2))
plot(U[,1],V[,1],xlab = "U1",ylab = "V1")
plot(U[,2],V[,2],xlab = "U2",ylab = "V2")

corcoef.test <- cor.test(r=ca$cor,n=7,p=3,q=2)

cor.test(r=ca$cor,n=7,p=3,q=2)  #？


#
age <- c(20,22,24,26,28,30,32,34,36,38,40,42)
collection <- c(8.4,9.5,11.8,10.4,13.3,14.8,13.2,14.7,16.4,16.5,18.9,18.5)

plot(age ~ collection)

reg <- lm(age ~collection)
abline(reg,col="red")
summary(reg)

boxplot(day ~ )




