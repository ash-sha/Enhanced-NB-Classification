#AlonDS
#NB
library(HiDimDA)
data(AlonDS)
dim(AlonDS)
levels(AlonDS$grouping)
head(AlonDS)

#----
a1=AlonDS[,1]
a=AlonDS[,1672]
b=AlonDS[,250]
c=AlonDS[,494]
d=AlonDS[,766]
e=AlonDS[,1773]
f=AlonDS[,626]
g=AlonDS[,1043]
h=AlonDS[,1424]
i=AlonDS[,514]
j=AlonDS[,1772]
final1=cbind(a,b,c,d,e,f,g,h,i,j,a1)
final1=as.matrix(final)

#---
  final1=cbind(tw,a1) 

#----

final=rbind(AlonDS[1:12,],AlonDS[25:62,])
final=as.matrix(final)
classifier<-naiveBayes(final, as.factor(final[,1])) 
p=predict(classifier, AlonDS[13:24,])
table(AlonDS[13:24,1],p)
recall_accuracy(AlonDS[13:24,1],p)



dat<-discretize(AlonDS[1:2001])
#computes the MIM (mutual information matrix)
I <- mutinformation(dat,method= "emp")
r=I[1,]
as=order(-r)
as=as.matrix(as)
asw=r[order(-r)]
asw=as.matrix(asw)
tw=asw[1:11,]
tw=as.matrix(tw)
t=t(tw)
fin=cbind(AlonDS[,as[1:11]])
fin=as.matrix(fin)
f249 = t[1,1]%*% fin[,1]
f1042 = t[1,2]%*% fin[,2]
f258 = t[1,3]%*% fin[,3]
f399 = t[1,4]%*% fin[,4]
f493 = t[1,5]%*% fin[,5]
f513 = t[1,6]%*% fin[,6]
f1771 = t[1,7]%*% fin[,7]
f377 = t[1,8]%*% fin[,8]
f1772 = t[1,9]%*% fin[,9]
f780 = t[1,10]%*% fin[,10]
final=as.matrix(final)
final = rbind(AlonDS[,1],f249,f1042,f258,f399,f493,f513,f1771,f377,f1771,f780)
final=t(final)


#finding the best
n = cbind(AlonDS[,1],fin[,1:20])
setwd("/Users/aswath/RProjects/exc")
getwd()
write.csv(n[,], file = "segg.CSV",row.names=FALSE)

new=read.csv("segg.csv")
fit = lm(X~.,data=new)
summary(fit)

