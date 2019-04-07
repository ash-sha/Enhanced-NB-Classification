#---------------------------------------------AlonDS----------------------------------------------------------------

library(HiDimDA)
data(AlonDS)
dim(AlonDS)
levels(AlonDS$grouping)
head(AlonDS)
library(e1071)

#Loo
classifier<-naiveBayes(AlonDS[,2:2000], AlonDS[,1]) 
xtab=table(predict(classifier, AlonDS[,2:2000]), AlonDS[,1])
library(caret) 
confusionMatrix(xtab)

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
final1=cbind(a1,a,b,c,d,e,f,g,h,i,j)
final1=as.matrix(final1)

#cv5
final1=rbind(final1[13:62,])
final1=as.matrix(final1)
classifier<-naiveBayes(final1[1:50,], as.factor(final1[1:50,1])) 
p=predict(classifier, final1[51:62,])
table(final1[51:62,1],p)
recall_accuracy(final1[51:62,1],p)





