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
tw=asw[1:11,]
tw=as.matrix(tw)
final=cbind(z[,as[1:11]])
final=as.matrix(final)
final=t(final)


#classifier cv & loo

final1=rbind(final[1:132,],final[177:194,])
final1=as.matrix(final1)
classifier<-naiveBayes(final1, as.factor(final1[,1])) 
p=predict(classifier, final[133:176,])
table(final[133:176,1],p)
recall_accuracy(final[133:176,1],p)


