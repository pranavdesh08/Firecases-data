#Generating for red.
#created a matrix of n*m order and stored it in data.
set.seed(15)
data<-matrix(rnorm(2*300,mean = 0, sd=1),2,300)

#named rows as X1 & X2.
X1<-data[c(1),]
X2<-data[c(2),]
X1_mean<-mean(X1)
X2_mean<-mean(X2)
#Created a given matrix of2*2 order.
sigma_r<-matrix(c(149,-47,-47,209),nrow = 2,ncol = 2)

#Updating data value by multiplying the sigmar
data<-sigma_r%*%data
data
dim(data)
#Adding vector to the matrix.
mu_r<-c(680,700)
data<-data+mu_r
View(data)
##################################
#Generating for blue.
##################################
data_1<-matrix(rnorm(2*300,mean = 0, sd=1),2,300)
X1<-data_1[c(1),]
X2<-data_1[c(2),]
sigma_b<-matrix(c(200,15,15,250),nrow = 2,ncol=2)
data_1<-sigma_b%*%data_1
mu_b<-c(10,550)
data_1<-data_1+mu_b
View(data_1)
#Creating data frame.
simData<- data.frame(a=data[1,],b=data[2,],c=rep('red',300))
simData<-rbind(simData,data.frame(a=data_1[1,],b=data_1[2,],c=rep('blue',300)))
#plot
plot(simData[,'a'],simData[,'b'],col='white',main = 'scatter plot',
     xlab='a',ylab = 'b',
     xlim = c(floor(min(as.numeric(simData$a))),
              ceiling(max(as.numeric(simData$a)))),
     ylim = c(floor(min(as.numeric(simData$b))),
              ceiling(max(as.numeric(simData$b)))))
points(simData[simData[,'c']=='red','a'],simData[simData[,'c']=='red','b'],col='red',pch=16)
points(simData[simData[,'c']=='blue','a'],simData[simData[,'c']=='blue','b'],col='blue',pch=16)

# points for test purpose.
#red test points.
test_r<-matrix(rnorm(2*200,mean=0,sd=1),2,200)
sigma_tr<-matrix(c(149,-47,-47,209),nrow = 2,ncol=2)
test_r<-sigma_tr%*%test_r
mu_tr<-c(680,700)
test_r<-mu_tr+test_r

#blue test points.
test_b<-matrix(rnorm(2*200,mean = 0, sd=1),2,200)
sigma_tb<-matrix(c(200,15,15,250),nrow = 2,ncol=2)
test_b<-sigma_tb%*%test_b
mu_tb<-c(10,550)
test_b<-mu_tb+test_b

#Creating data frame for test points.
testDf<-data.frame(a=test_r[1,],b=test_r[2,],c=rep('red',200))
testDf<-rbind(testDf,data.frame(a=test_b[1,],b=test_b[2,],c=rep('blue',200)))

#creating a grid.
newA2<-seq(mA2<-floor(min(simData$a)),MA2<-ceiling(max(simData$a)),length.out = 200)
newA3<-seq(mA3<-floor(min(simData$b)),MA3<-ceiling(max(simData$b)), length.out = 200)

newPts0<-data.frame(a=kronecker(newA2, rep(1,length(newA3))), 
                    b=rep(newA3,length(newA2)))

#Implementing bayes decision
deter_r<-det(sigma_r) 
deter_b<-det(sigma_b)
sigma_r1<-sigma_r%*%sigma_r
sigma_b1<-sigma_b%*%sigma_b
prob_all <- matrix(ncol = 2, nrow = 0)
newPts0$c<-NA


for(i in c(1:40000)){
  x=c(newPts0[i,1],newPts0[i,2])
  
  #bay_rule_r=(((-1/2)*((t(x-mu_r))%*%(1/sigma_r)%*%(x-mu_r))) -
  #            ((1/2)*(log(2*(pi))) -(1/2)%*%(log(deter_r))))
  bay_rule_r=log(1/2*pi*(deter_r))+(-1/2 * t(x-mu_r)%*% solve(sigma_r1)%*%(x-mu_r))
  
  #bay_rule_b=(((-0.5)*((t(x-mu_b))*(1/sigma_b)*(x-mu_b))) -
  #           ((d/2)*(log(2*(pi))) -(d/2)*(log(det(sigma_b)))))
  
  bay_rule_b=log(1/2*pi*(deter_b))+(-1/2 * t(x-mu_b)%*% solve(sigma_b1)%*%(x-mu_b))
  #bay_rule__r = -log(1/(determinant_r*2*pi)) + (-1/2 * (x - mean_r) %*% solve(cov_r) %*% t(data_point-mean_r))
  #bay_rule_b = -log(1/(determinant_b*2*pi)) + (-1/2 * (data_point - mean_b) %*% solve(cov_b) %*% t(data_point-mean_b))
  
  
  
  
  
  if (bay_rule_r>bay_rule_b){
    bay_rule_r='red'
    newPts0[,3]=bay_rule_r
    prob_all = rbind(prob_all, c(1,0))
  }
  else{
    bay_rule_b='blue'
    newPts0[,3]=bay_rule_b
    prob_all = rbind(prob_all, c(0,1))
  }
}



z=matrix(prob_all,nrow=length(newA2),ncol = length(newA3),byrow = T) 

contour(newA2,newA3,z,levels=c(0.5),col="maroon", drawlabels=FALSE,lwd=2)

points(newPts0[newPts0$c =='red',"a"],newPts0[newPts0$c=='red',"b"],pch='.',cex=2,col='orange')
points(newPts0[newPts0$c =='blue',"a"],newPts0[newPts0$c=='blue',"b"],pch='.',cex=2,col="cornflowerblue")

points(simData[simData$c=="red","a"],
       simData[simData$c=="red", "b"], col="red",pch=18)


points(simData[simData$c=="blue","a"],
       simData[simData$c=="blue", "b"], col="blue",pch=20)

#############################################################
testDf$d<-NA
for(i in c(1:400)){
  x=c(testDf[i,1],testDf[i,2])
  bay_rule_r=log(1/2*pi*(deter_r))+(-1/2 *t((x) - mu_r)%*% solve(sigma_r1)%*%(x-mu_r))
  bay_rule_b=log(1/2*pi*(deter_b))+(-1/2 *t((x) - mu_b)%*% solve(sigma_b1)%*%(x-mu_b))
  

 if(bay_rule_r>bay_rule_b){
   testDf[i,4]<-'red'
 }
 if(bay_rule_r<bay_rule_b){
   testDf[i,4]<-'blue'
   
 }  
 
}

C= table(testDf$c, testDf$d)
print(C)
print(paste("Error rate:  ", (C[1,2]+C[2,1])/sum(C)))

#############################################################k-nn##################
library(kknn)
cv1=train.kknn(c~a+b, ks=seq(1,600,by=10),data=simData,kernel="rectangular")
summary(cv1)
k=cv1$best.parameters[[2]]
print(k)
#################################

bestknnModel <- kknn(c~a+b,train=simData,test=testDf, k=k,kernel="rectangular")

nj<-predict(bestknnModel, data=testDf)

confusion_knn= table(testDf$c, nj)
print(confusion_knn)
print(paste("Error rate:  ", (confusion_knn[1,2]+confusion_knn[2,1])/sum(confusion_knn)))

bestknnModel1_grid<-kknn(c~a+b, k=k,train=simData,test=newPts0,kernel="rectangular")

pj<-predict(bestknnModel1_grid,data=newPts0)
pj
newPts0<-cbind(newPts0,data.frame(Y1=pj))


z1=matrix(bestknnModel1_grid$prob,nrow=length(newA2),
         ncol = length(newA3),byrow = T)

contour(newA2,newA3,z,levels=c(0.5),col="maroon",drawlabels=FALSE,lwd=2)
contour(newA2,newA3,z1,levels=c(0.5),col="green",drawlabels=FALSE,lwd=2,add=TRUE)


points(newPts0[newPts0$Y1 =='red',"a"],newPts0[newPts0$Y1=='red',"b"],
       pch='.',cex=2,col="orange")
points(newPts0[newPts0$Y1 =='blue',"a"],newPts0[newPts0$Y1=='blue',"b"],
       pch='.',cex=2,col="yellow")
points(testDf[testDf$d=='red','a'],testDf[testDf$d=='red','b'],col='red',pch=15)
points(testDf[testDf$d=='blue','a'],testDf[testDf$d=='blue','b'],col='blue',pch=15)


################################################k-means#####################################

dat<-read.csv("in2.csv",header=TRUE)
mod<-kmeans(cbind(dat$X1, dat$X2), 4)
dat<- cbind(dat, cluster=mod$cluster)
plot(dat$X1,dat$X2, col="blue",xlab="X1", ylab="X2",pch=20)
readline("Original data")
dev.new()
plot(dat$X1,dat$X2, col="white",xlab="X1", ylab="X2", main="clustered data")
points(dat[dat$cluster==1,"X1"],dat[dat$cluster==1,"X2"], col="cornflowerblue",
       pch=20)
points(dat[dat$cluster==2,"X1"],dat[dat$cluster==2,"X2"], col="orange",pch=20)
points(dat[dat$cluster==3,"X1"],dat[dat$cluster==3,"X2"], col="green",pch=20)
points(dat[dat$cluster==4,"X1"],dat[dat$cluster==4,"X2"], col="red",pch=20)
readline("clustered data")
# Draw the original clusters:
dev.new()
plot(dat$X1,dat$X2, col="white",xlab="X1", ylab="X2", main="original clusters")
n=nrow(dat)
points(dat[1:(n/4),"X1"],dat[1:(n/4),"X2"], col="cornflowerblue",pch=20)
points(dat[(n/4+1):(n/2),"X1"],dat[(n/4+1):(n/2),"X2"], col="orange",pch=20)
points(dat[(n/2+1):(3*n/4),"X1"],dat[(n/2+1):(3*n/4),"X2"], col="green",pch=20)
points(dat[(3*n/4+1):n,"X1"],dat[(3*n/4+1):n,"X2"], col="red",pch=20)
print("Original clusters")
















