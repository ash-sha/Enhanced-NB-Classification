library(DLBCL)
data(exprLym)
dim(exprLym)
levels(exprLym$Subgroup)
library(e1071)

x <- t(exprs(exprLym))
y <- as.vector(pData(exprLym))
z=cbind(y[,1],x[,])


#computes the MIM (mutual information matrix)
dat1<-discretize(z[,1:1001])
z1=cbind(z[,1],z[,1002:2002])
z2=cbind(z[,1],z[,2003:3584])
dat2<-discretize(z1[,1:1002])
dat3<-discretize(z2[,1:1583])
I1 <- mutinformation(dat1,method= "emp")
I2 <- mutinformation(dat2,method= "emp")
I3 <- mutinformation(dat3,method= "emp")

I4 <- c(I1[1,],I2[1,2:1002],I3[1,2:1583])
I4=as.matrix(I4)
I4=t(I4)
r=I4[1,]
as=order(-r)
as=as.matrix(as)
asw=r[order(-r)]
asw=as.matrix(asw)
tw=asw[2:11,]
tw=as.matrix(tw)
tw=t(tw)
fin=cbind(z[,as[2:11]])
fin=as.matrix(fin)

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

final1=rbind(final[1:44,],final[89:194,])
final1=as.matrix(final1)
classifier<-naiveBayes(final[1:150,], as.factor(final[1:150,1])) 
p=predict(classifier, final[151:194,])
table(final[151:194,1],p)
recall_accuracy(final[151:194,1],p)


