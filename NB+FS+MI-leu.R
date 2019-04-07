
library(golubEsets)
data(Golub_Merge)
dim(Golub_Merge)
levels(Golub_Merge$ALL.AML)
library(e1071)


x <- t(exprs(Golub_Merge))
y <- as.vector(pData(Golub_Merge))
z=cbind(y[,2],x[,])
#computes the MIM (mutual information matrix)
dat1<-discretize(z[,1:2001])
z1=cbind(z[,1],z[,2002:4002])
z2=cbind(z[,1],z[,4003:6003])
z3=cbind(z[,1],z[,6004:7130])
dat2<-discretize(z1[,1:2002])
dat3<-discretize(z2[,1:2002])
dat4<-discretize(z3[,1:1128])
I1 <- mutinformation(dat1,method= "emp")
I2 <- mutinformation(dat2,method= "emp")
I3 <- mutinformation(dat3,method= "emp")
I4 <- mutinformation(dat4,method= "emp")
I5 <- c(I1[1,],I2[1,2:2002],I3[1,2:2002],I4[1,2:1128])
I5=as.matrix(I5)
I5=t(I5)
r=I5[1,]
as=order(-r)
as=as.matrix(as)
asw=r[order(-r)]
asw=as.matrix(asw)
tw=asw[2:11,]
tw=as.matrix(tw)
tw=t(tw)
fin=cbind(z[,as[2:11]])
fin=as.matrix(fin)
fin=t(fin)

f1=tw[1,1] %*% fin[,1]
f2=tw[1,2] %*% fin[,2]
f3=tw[1,3] %*% fin[,3]
f4=tw[1,4] %*% fin[,4]
f5=tw[1,5] %*% fin[,5]
f6=tw[1,6] %*% fin[,6]
f7=tw[1,7] %*% fin[,7]
f8=tw[1,8] %*% fin[,8]
f9=tw[1,9] %*% fin[,9]
f10=tw[1,10] %*% fin[,10]
final=rbind(z[,1],f1,f2,f3,f4,f5,f6,f7,f8,f9,f10)
na.omit(final)
final=as.matrix(final)
final=t(final)



#classifier cv & loo

final1=rbind(final[1:45,],final[61:72,])
final1=as.matrix(final1)
classifier<-naiveBayes(final1, as.factor(final1[,1])) 
p=predict(classifier, final[46:60,])
table(final[46:60,1],p)
recall_accuracy(final[46:60,1],p)


