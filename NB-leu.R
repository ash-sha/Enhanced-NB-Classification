
library(golubEsets)
data(Golub_Merge)
dim(Golub_Merge)
levels(Golub_Merge$ALL.AML)
library(e1071)

#Loo
x <- t(exprs(Golub_Merge))
y <- as.vector(pData(Golub_Merge))
z=cbind(y[,2],x[,])


a1=z[,1]
a=z[,40]
b=z[,854]
c=z[,280]
d=z[,375]
e=z[,377]
f=z[,805]
g=z[,701]
h=z[,1118]
i=z[,184]
j=z[,224]
k=z[,217]
l=z[,166]
m=z[,372]
n=z[,918]
o=z[,346]
p=z[,214]
q=z[,256]
r=z[,802]
s=z[,282]
t=z[,572]
final=cbind(a1,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
final=as.matrix(final)

#cv5
final1=rbind(final[1:45,],final[61:72,])
final1=as.matrix(final1)
classifier<-naiveBayes(final1, as.factor(final1[,1])) 
p=predict(classifier, final[45:60,])
table(final[45:60,1],p)
recall_accuracy(final[45:60,1],p)

