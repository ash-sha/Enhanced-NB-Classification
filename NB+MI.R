#-------------------------------------------------AlonDS-------------------------------------------------------------
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
tw=asw[1:11,]
tw=as.matrix(tw)
final=cbind(AlonDS[,as[1:11]])
final=as.matrix(final)
final=t(final)


#classifier cv & loo
final=rbind(final[1:24,],final[37:62,])
final=as.matrix(final)
classifier<-naiveBayes(final, as.factor(final[,1])) 
p=predict(classifier, final[25:36,])
table(final[25:36,1],p)
recall_accuracy(final[25:36,1],p)


