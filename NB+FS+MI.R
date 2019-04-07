#------------------------------------------------AlonDS------------------------------------------------------------

library(HiDimDA)
data(AlonDS)
dim(AlonDS)
levels(AlonDS$grouping)
head(AlonDS)

#computes the MIM (mutual information matrix)
dat<-discretize(AlonDS[1:2001])
I <- mutinformation(dat,method= "emp")
r=I[1,]
as=order(-r)
as=as.matrix(as)
asw=r[order(-r)]
asw=as.matrix(asw)
tw=asw[2:11,]
tw=as.matrix(tw)
t=t(tw)
fin=cbind(AlonDS[,as[2:11]])
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

final = rbind(AlonDS[,1],f249,f1042,f258,f399,f493,f513,f1771,f377,f1771,f780)
final=as.matrix(final)
final=t(final)


#classifier
final=rbind(final[49:62,],final[1:36,])
final=as.matrix(final)
classifier<-naiveBayes(final, as.factor(final[,1])) 
p=predict(classifier, final[37:48,])
table(final[37:48,1],p)
recall_accuracy(final[37:48,1],p)
