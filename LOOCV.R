set.seed(123)
library(caret)
folds <- createFolds(AlonDS$grouping)
train1 <- AlonDS[-folds$Fold01,]
test1 <- AlonDS[folds$Fold01,]
train2 <- AlonDS[-folds$Fold02,]
test2 <- AlonDS[folds$Fold02,]
train3 <- AlonDS[-folds$Fold03,]
test3 <- AlonDS[folds$Fold03,]
train4 <- AlonDS[-folds$Fold04,]
test4 <- AlonDS[folds$Fold04,]
train5 <- AlonDS[-folds$Fold05,]
test5 <- AlonDS[folds$Fold05,]
train6 <- AlonDS[-folds$Fold06,]
test6 <- AlonDS[folds$Fold06,]
train7 <- AlonDS[-folds$Fold07,]
test7 <- AlonDS[folds$Fold07,]
train8 <- AlonDS[-folds$Fold08,]
test8 <- AlonDS[folds$Fold08,]
train9 <- AlonDS[-folds$Fold09,]
test9 <- AlonDS[folds$Fold09,]
train10 <- AlonDS[-folds$Fold10,]
test10 <- AlonDS[folds$Fold10,]
for(i in 1:9) 
{ 
  assign(paste0("train_",i), AlonDS[paste0("-folds$Fold0",i),])
}
train_10 <- AlonDS[-folds$Fold10,]



#---

library(caret)
x1 <- t(exprs(exprLym))
y1 <- as.vector(pData(exprLym))
z1=cbind(y[,1],x[,])
make.unique(c("X1", "X2"))
make.unique(c(make.unique(c("X", "Y")), "a.2", "a"))

model <- train(x = z1[1:194,2:3584], 
               y = factor(z1[1:194,1]), 
               method = 'glmnet', 
               metric='ROC', 
               trControl = trainControl(method = 'LOOCV', # repeated cross validation
                                        number = 10, # nr of partitions
                                        repeats = 10, # nr of repeats
                                        classProbs = T, 
                                        summaryFunction = twoClassSummary))


#----
library(caret)
library(doParallel)
library(data.table)

cl <- makeCluster(detectCores() - 1) # I'm using 3 cores.
registerDoParallel(cl)

data(AlonDS)
AlonDS <- AlonDS[AlonDS$grouping != 'colon',] # to get two categories
TrainData <- as.data.table(AlonDS[,2:2001]) # My data is a data.table.
TrainClasses <- as.factor(as.character(AlonDS[,1])) # to reset the levels to the two remaining flower types.

ctrl <- trainControl(method = 'LOOCV',
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     allowParallel = TRUE)
model.fit <- train(x = TrainData,
                   y = TrainClasses,
                   method = 'nb',
                   metric = 'ROC',
                   tuneLength = 3,
                   trControl = ctrl)


#--
# LOOCV (leave one out cross validation)
library(boot)
require(boot)
?cv.glm
cv.glm(AlonDS, glmfit, cost, K=n)
str(AlonDS)
Alon$grouping <- as.numeric(Alon$grouping)
model1 <- glm(grou ~ horsepower, data=AlonDS)
summary(model1)
loocv1 <- cv.glm(data=Auto, glmfit=model1) 
loocv1$delta
model2 <- glm(mpg ~ poly(horsepower, 2), data=Auto) 
model3 <- glm(mpg ~ poly(horsepower, 3), data=Auto) 
model4 <- glm(mpg ~ poly(horsepower, 4), data=Auto) 
model5 <- glm(mpg ~ poly(horsepower, 5), data=Auto) 
model6 <- glm(mpg ~ poly(horsepower, 6), data=Auto) 
Auto$model1 <- predict(model1)
Auto$model2 <- predict(model2)
Auto$model3 <- predict(model3) 
Auto$model4 <- predict(model4) 
Auto$model5 <- predict(model5)
Auto$model6 <- predict(model6)

#---
#data split
library(caret)
library(klaR)
# load the iris dataset
data(AlonDS)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(AlonDS$grouping, p=split, list=FALSE)
data_train <- final[ trainIndex,]
data_test <- final[-trainIndex,]
# train a naive bayes model
model <- NaiveBayes(grouping~., data=data_train)
# make predictions
x_test <- data_test[,2:11]
y_test <- data_test[,1]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)

#loocv
library(caret)
data(AlonDS)
data(exprLym)
data(Golub_Merge)
x <- t(exprs(exprLym))
y <- as.vector(pData(exprLym))
z=cbind(y[,1],x[,])
x1 <- t(exprs(Golub_Merge))
y1 <- as.vector(pData(Golub_Merge))
z1=cbind(y1[,2],x1[,])

train_control <- trainControl(method="LOOCV")
model <- train(grouping~., data=final, trControl=train_control, method="nb")

print(model)







install.packages("twitteR")
api_key <- "b2afVUp4PNsYFNe1wEXso2aeO" 
api_secret <- "L1zdn8WcYOCaCjfiMXrIdkQkpKHLQgWLj36IpLLWhRGeBD852D" 
token <- "2749482174-SNrfb90408RF8ZJAlF5JbVng0LQd0t6GY86A7ei"  
token_secret <- "5jpwWLv79V7wrQ4LdbJyyBGZHU3WPBXcl3WW4IoYd0UzF" 
setup_twitter_oauth(api_key, api_secret, token, token_secret)
tweets <- searchTwitter("community garden OR #communitygarden OR farmers market OR #farmersmarket", n = 200, lang = "en")
tweets.df <-twListToDF(tweets)
write.csv(tweets.df, "Users\aswath\RProjects\exc\tweets.csv")
