
library(DLBCL)
data(exprLym)
dim(exprLym)
levels(exprLym$Subgroup)
library(e1071)

#Loo
x <- t(exprs(exprLym))
y <- as.vector(pData(exprLym))
z=cbind(y[,1],x[,])




#cv5

final1=rbind(z[1:44,],z[89:194,])
final1=as.matrix(final1)
classifier<-naiveBayes(final1, as.factor(final1[,1])) 
p=predict(classifier, z[45:88,])
table(z[45:88,1],p)
recall_accuracy(z[45:88,1],p)





