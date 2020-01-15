#ml hw4 
#1.a)
priceData<-read.csv("file:///C:/Users/hp/Downloads/textBookSales.csv",header = TRUE)
head(priceData)
plot(priceData$price,priceData$sales,col='white',main='scatterplot',
     xlab = 'price of the book',ylab='sales of the book')
points(priceData[priceData$edition=='hardcover','price'],priceData[priceData$edition=='hardcover','sales'],
       col='blue',pch=20,cex=1)
points(priceData[priceData$edition=='paperback','price'],priceData[priceData$edition=='paperback','sales'],
       col='orange',pch=20,cex=1)
#1.b)
M1<-lm(sales~ price, data=priceData[priceData$edition=='hardcover',])
M2<-lm(sales~ price, data=priceData[priceData$edition=='paperback',])

summary(M1)
summary(M2)
#1.c)
print('variation in the sales:')

print("for paperback 53.66%" )

print("for hardcover 31.19%" )

abline(a=M1$coefficients[1],b=M1$coefficients[2],col='blue')
abline(a=M2$coefficients[1],b=M2$coefficients[2],col='red')

#1.d)
print('there is negative relationship between price and sales for both the types of editions.')
print(M1$coefficients)
print(M2$coefficients)

#1.f)
matrix1<-matrix(c(priceData$price,priceData$sales),ncol = 2)
library(EMCluster)
em1<-init.EM(matrix1,nclass = 2)
summary(em1)
em<-emcluster(matrix1,emobj = em1)
c<-assign.class(matrix1,em)
priceData<-cbind(priceData,c$class)
plot(priceData$price,priceData$sales,col='white',main='scatterplot',
     xlab = 'price of the book',ylab='sales of the book')
points(priceData[c$class==1,'price'],priceData[c$class==1,'sales'],col='red',pch=20)
points(priceData[c$class==2,'price'],priceData[c$class==2,'sales'],col='blue',pch=20)
original<-c()
for(i in priceData$edition){
  if(i=='paperback'){
    original<-c(original,1)
  }else{
    original<-c(original,2)
  }
}
priceData<-cbind(priceData,original)
x<-print(table(priceData$original,priceData$`c$class`))

print('misclassification rate:')
print(x[1,2]+x[2,1]/sum(x))
print(x[1,1]+x[2,2]/sum(x))


M3<-lm(sales~price,data = priceData[priceData$`c$class`==1,])
M4<-lm(sales~price,data = priceData[priceData$`c$class`==2,])
summary(M3)
summary(M4)
abline(a=M3$coefficients[1],b=M3$coefficients[2],col="black")
abline(a=M4$coefficients[1],b=M4$coefficients[2],col="green")
print(paste('intercept and slope of hardcover:',M1$coefficients[1],M1$coefficients[2]))
print(paste('intercept and slope of paperback:',M2$coefficients[1],M2$coefficients[2]))
print(paste('intercept and slope of c$class==1:',M3$coefficients[1],M3$coefficients[2]))
print(paste('intercept and slope of c$class==2:',M4$coefficients[1],M4$coefficients[2]))








