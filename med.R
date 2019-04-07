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
classifier<-naiveBayes(final1[1:50,], as.factor(final1[1:50,1])) 
p=predict(classifier, final1[51:62,])
table(final1[51:62,1],p)
recall_accuracy(final1[51:62,1],p)



dat<-discretize(AlonDS[1:2001])
#computes the MIM (mutual information matrix)
I <- mutinformation(dat,method= "emp")
r=I[1,]
as=order(-r)
as
asw=r[order(-r)]
asw=as.matrix(asw)
tw=asw[1:11,]
tw
final=cbind(AlonDS[,as[1:11]])




#maxent
matrix = create_matrix(AlonDS)
matrix
container = create_container(matrix, as.numeric(as.factor(AlonDS[,1])),
                              trainSize=1:50, testSize=51:62,virgin = FALSE)
container

models = train_models(container, algorithms=c("MAXENT"))
models

results = classify_models(container, models)
results

N=5
cross_validate(container,N,"MAXENT")

#-------------------------------------------------GolubEset-------------------------------------------------------------------


#NB
library(golubEsets)
data(Golub_Merge)
dim(Golub_Merge)
levels(Golub_Merge$ALL.AML)
head(Golub_Merge)
View(Golub_Merge)



classifier<-naiveBayes(Golub_Merge[1:50,], Golub_Merge$ALL.AML) 
xtab=table(predict(classifier, Golub_Merge[51:72,]), Golub_Merge$ALL.AML)
library(caret) 
confusionMatrix(xtab)



#maxent

library('golubEsets')  

data(Golub_Merge)
x <- t(exprs(Golub_Merge))
y <- as.vector(pData(Golub_Merge))

matrix = create_matrix(x)
matrix
container = create_container(matrix, as.numeric(as.factor(Golub_Merge$ALL.AML)),
                             trainSize=1:60, testSize=61:72,virgin = FALSE)
container

models = train_models(container, algorithms=c("MAXENT"))
models

results = classify_models(container, models)
results
N=5
cross_validate(container,N,"MAXENT")




#-------------------------------------------------DLBCL-------------------------------------------------------------------

#NB
library(DLBCL)
data(exprLym)
dim(exprLym)
levels(exprLym$Subgroup)
head(exprLym)
View(exprLym)

final=rbind(exprLym[1:12,],exprLym[25:62,])
final=as.matrix(final)
classifier<-naiveBayes(exprLym[1:194,], exprLym$Subgroup) 
xtab=table(predict(classifier, exprLym[1:194,]), exprLym$Subgroup)
library(caret) 
confusionMatrix(xtab)


dat2<-discretize(exprLym[1:1000])
dat3<-discretize(exprLym[1001:2000])
dat4<-discretize(exprLym[2001:3583])

#computes the MIM (mutual information matrix)
I2 <- mutinformation(dat2,method= "emp")
I3 <- mutinformation(dat3,method= "emp")
I4 <- mutinformation(dat4,method= "emp")


I5 <- c(I2[1001,],I3[1001,],I4[1584,])
I5=as.matrix(I5)
r2=I5[,]
as2=order(-r2)
as2=as.matrix(as2)
#asw2=r2[order(-r2)]
#asw2=as.matrix(asw2[4:3598,])
#tw2=asw2[,1:10]
#tw2=as.matrix(tw2)
#t2=t(tw2)
fin2=exprLym[,as2[1:20]]
fin2=as.matrix(fin2)
f249 = t2[1,1]%*% fin2[,1]
f1042 = t2[1,2]%*% fin2[,2]
f258 = t2[1,3]%*% fin2[,3]
f399 = t2[1,4]%*% fin2[,4]
f493 = t2[1,5]%*% fin2[,5]
f513 = t2[1,6]%*% fin2[,6]
f1771 = t2[1,7]%*% fin2[,7]
f377 = t2[1,8]%*% fin2[,8]
f1772 = t2[1,9]%*% fin2[,9]
f780 = t2[1,10]%*% fin2[,10]
finalll=as.matrix(finallll)
finalll = rbind(exprLym$Subgroup,f249,f1042,f258,f399,f493,f513,f1771,f377,f1771,f780)
finalll=t(finallll)

n2 = cbind(exprLym$Subgroup,fin2[194,1:2])
setwd("/Users/aswath/RProjects/exc")
getwd()
write.csv(n2[,], file = "segg2.CSV",row.names=FALSE)

new2=read.csv("segg2.csv")
fit2 = lm(V1~.,data=new2)
summary(fit2)


#maxent

library('golubEsets')  

data(exprLym)
x <- t(exprs(exprLym))
y <- as.vector(pData(exprLym)$Subgroup)

matrix = create_matrix(x)
matrix
container = create_container(matrix, as.numeric(as.factor(exprLym$Subgroup)),
                             trainSize=1:145, testSize=146:194,virgin = FALSE)
container

models = train_models(container, algorithms=c("MAXENT"))
models

results = classify_models(container, models)
results
N=5
cross_validate(container,N,"MAXENT")






