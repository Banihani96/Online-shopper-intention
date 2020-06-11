library(caret)
library(klaR)

shop <- read.csv("C:/Users/GTS/Downloads/online_shoppers_intention.csv")

#list types for each attribute
sapply(shop, class)
head(shop)


shop[11:15] <- NULL
shop[12] <- NULL
anyNA(shop)
#pre-processing
shop[,12] <- as.factor(shop[,12])
shop$VisitorType<-as.integer(shop$VisitorType)
#shop$Administrative <- as.numeric(shop$Administrative)
#shop$Informational <- as.numeric(shop$Informational)
#shop$ProductRelated <- as.numeric(shop$ProductRelated)
summary(shop)



boxplot(shop)
outliers <- boxplot(shop$ProductRelated_Duration, plot=FALSE)$out
shop[which(shop$ProductRelated_Duration %in% outliers),]
shop<- shop[-which(shop$ProductRelated_Duration %in% outliers),]






#Summarize class distribution
percentage <- prop.table(table(shop$Revenue)) * 100
cbind(freq=table(shop$Revenue), percentage=percentage)



#split
set.seed(3033)
intrain <- createDataPartition(y = shop$Revenue, p= 0.75, list = FALSE)
training <- shop[intrain,]
testing <- shop[-intrain,]

percentagetrain <- prop.table(table(training$Revenue)) * 100
cbind(freq=table(training$Revenue), percentage=percentagetrain)

percentagetest <- prop.table(table(testing$Revenue)) * 100
cbind(freq=table(testing$Revenue), percentage=percentagetest)



#Bagging 
library(caret)
bagging.control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
system.time(modle.treebag <- train(Revenue~., data=training, method="treebag", metric=metric, 
                       trControl=bagging.control,preProc=c("center","scale")))
print(modle.treebag)

system.time(treeteest <- predict(modle.treebag,newdata = testing))
confusionMatrix(data = treeteest,testing$Revenue)

pre.tree <- predict(modle.treebag,newdata = testing,type = "prob")
prediction.tree <- prediction(pre.tree[,2], testing$Revenue)
#pre vs recall
perform.tree <- performance(prediction.tree,"prec","rec")
plot(perform.tree,colorize = T,main = "Bagged CART")
#accuracy
acc.trr <- performance(prediction.tree,"acc")
plot(acc.trr,main = "Bagged CART")
#roc
pre.roc <- performance(prediction.tree,"tpr","fpr")
plot(pre.roc,colorize=T,main = "ROC Curves for Bagged CART")
abline(a= 0,b=1)

pre.auc <- performance(prediction.tree,measure="auc")

auc <- slot(pre.auc,"y.values")[[1]]
legend(.4,.25,auc,title = "AUC")
auc



set.seed(seed)
system.time(model.rf <- train(Revenue~., data=training, method="rf", metric=metric, 
                  trControl=bagging.control,preProc=c("center","scale")))
print(model.rf)
system.time(baggingtest <- predict(model.rf,newdata = testing))
confusionMatrix(data = baggingtest,testing$Revenue)

pred.rf <- predict(model.rf,newdata = testing,type = "prob")
pred.rf <- prediction(as.numeric( pred.rf[,2]), testing$Revenue)
perform.rf <- performance(pred.rf,"prec","rec")
plot(perform.rf,colorize = T,main = "Random Forest")
#acc
acc.rf <- performance(pred.rf,"acc")
plot(acc.rf,main = "Random Forest")
#roc
pre.rf <- performance(pred.rf,"tpr","fpr")
plot(pre.rf,colorize=T,main = "ROC Curves for Random Forest")
abline(a= 0,b=1)
pre1.auc <- performance(pred.rf,measure="auc")

auc <- slot(pre1.auc,"y.values")[[1]]
legend(.4,.25,auc,title = "AUC")
auc


# combine the models
bagging_results <- resamples(list(treebag=modle.treebag, rf=model.rf))
summary(bagging_results)
dotplot(bagging_results)
bwplot(bagging_results)





  # Example of Stacking algorithms
# create submodels
library(caretEnsemble)
stack.control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        savePredictions=TRUE, classProbs=TRUE,preProc=c("center","scale"))
algorithmList <- c( 'rpart', 'knn', 'nb')
set.seed(seed)
system.time (models <- caretList(Revenue~., data=training, trControl=stack.control, methodList=algorithmList))
results <- resamples(models)
summary(results)
dotplot(results)
bwplot(results)

#coolelation
modelCor(results)
splom(results)


system.time(model.rpart <- predict(models$rpart,newdata = testing))
confusionMatrix(data = model.rpart,testing$Revenue)

pred.rpart <- predict(models$rpart,newdata = testing,type = "prob")
pred.rpart <- prediction(as.numeric( pred.rpart[,2]), testing$Revenue)
perform.rpart <- performance(pred.rpart,"prec","rec")
plot(perform.rpart,colorize = T,main = "Stacking CART")

acc.rpart <- performance(pred.rpart,"acc")
plot(acc.rpart,main = "Stacking CART")

#roc
pre.rpart <- performance(pred.rpart,"tpr","fpr")
plot(pre.rpart,colorize=T,main = "ROC Curves for Stacking CART ")
abline(a= 0,b=1)
pre3.auc <- performance(pred.rpart,measure="auc")

auc <- slot(pre3.auc,"y.values")[[1]]
legend(.4,.25,auc,title = "AUC")
auc



system.time(model.knn <- predict(models$knn,newdata = testing))
confusionMatrix(data = model.knn,testing$Revenue)

pred.knn <- predict(models$knn,newdata = testing,type = "prob")
pred.knn <- prediction(as.numeric( pred.knn[,2]), testing$Revenue)
perform.knn <- performance(pred.knn,"prec","rec")
plot(perform.knn,colorize = T,main = "KNN")

acc.knn <- performance(pred.knn,"acc")
plot(acc.knn,main = "KNN")
#roc
pre.knn <- performance(pred.knn,"tpr","fpr")
plot(pre.knn,colorize=T,main = "ROC Curves for KNN ")
abline(a= 0,b=1)
pre4.auc <- performance(pred.knn,measure="auc")

auc <- slot(pre4.auc,"y.values")[[1]]
legend(.4,.25,auc,title = "AUC")
auc


system.time(model.nb <- predict(models$nb,newdata = testing))
confusionMatrix(data = model.nb,testing$Revenue)

pred.nb <- predict(models$nb,newdata = testing,type = "prob")
pred.nb <- prediction(as.numeric( pred.nb[,2]), testing$Revenue)
perform.nb <- performance(pred.nb,"prec","rec")
plot(perform.rf,colorize = T,main = "Naïve Bayes ")
acc.nb <- performance(pred.nb,"acc")
plot(acc.nb,main = "Naïve Bayes")
#roc
pre.nb <- performance(pred.nb,"tpr","fpr")
plot(pre.nb,colorize=T,main = "ROC Curves for Naïve Bayes")
abline(a= 0,b=1)
pre5.auc <- performance(pred.nb,measure="auc")

auc <- slot(pre5.auc,"y.values")[[1]]
legend(.4,.25,auc,title = "AUC")
auc

library(caretEnsemble)
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.nb <- caretStack(models, method="knn", metric="Accuracy", trControl=stackControl)
print(stack.nb)

set.seed(seed)
system.time(stack.rf <- caretStack(models, method="rpart", metric="Accuracy", trControl=stackControl))
print(stack.rf)

system.time(models.rf <- predict(stack.rf,newdata = testing))
confusionMatrix(data = models.rf,testing$Revenue)

pre.rf <- predict(stack.rf,newdata = testing,type = "prob")
pre <- prediction(as.numeric( pre.rf), testing$Revenue)
pre2 <- performance(pre,"prec","rec")
plot(pre2,colorize = T)
acc.rf <- performance(pre,"acc")
plot(acc.rf)
#roc
r.pre <- performance(pre,"tpr","fpr")
plot(r.pre,colorize=T,main = "ROC Curves for Naïve Bayes")
abline(a= 0,b=1)
pre6.auc <- performance(pre,measure="auc")

auc <- slot(pre6.auc,"y.values")[[1]]
legend(.1,.95,auc,title = "AUC")
auc




library(ROCR)
#sol 1
lda.model <- predict(stack.rf,newdata = testing,type= "raw" )
head(lda.model)
lda.pre <- prediction(as.numeric(lda.model),as.numeric( testing$Revenue))
evl1 <- performance(lda.pre,"acc")
plot(evl1)

evl2 <- performance(pre,"tpr","fpr")
plot(evl2)

evl3 <- performance(pre,"sens","spec")
plot(evl3)


max <- which.max(slot(evl1,"y.values")[[1]])
max
acc <- slot(evl1,"y.values")[[1]][max]
cut <- slot(evl1,"x.values")[[1]][max]
print(c(Accuuracy=acc,cutoff=cut))

roc <- performance(lda.pre,"tpr","fpr")
plot(roc,colorize = T)

#sol2 
library(ROCR)
pre.rf <- predict(stack.rf,newdata = testing,type = "prob")
pre <- prediction(as.numeric( pre.rf), testing$Revenue)
pre1 <- performance(pre,"acc")
plot(pre1)
abline(h=0.89)
#pre vs recall
pre2 <- performance(pre,"prec","rec")
plot(pre2,colorize = T)

#AUC
pre3 <- performance(pre,"tpr","fpr")
plot(pre3)
plot(pre3,colorize=T,main = "ROC Curves",
     ylab = "sensivity",
     xlab = "specifity") 
abline(a= 0,b=1)

pre4 <- performance(pre,measure="auc")
pre4

auc <- pre4@y.values[[1]]
legend(.5,.25,auc,title = "AUC")
auc
acc1 <- slot(pre4,"y.values")[[1]]
acc1












