###random forest

if(!require(randomForest))
{install.packages("randomForest")}
if(!require(Metrics))
{install.packages("Metrics")}
if(!require(party))
{install.packages("party")}
library(party)
if(!require(partykit))
{install.packages("partykit")}
library(partykit)
if(!require(rpart))
{install.packages("rpart")}
library(rpart)
library(randomForest)
library(caret)
if(!require(gam))
{install.packages("gam")}
library(gam)
library(Metrics)


set.seed(200)
setwd("~/Actuaria Anahuac/7mo semestre/Matematicas actuariales")
datarf<-read.csv("insurancerandomforest.csv")
datarf<-datarf[,-8]
trainindex<-createDataPartition(datarf$charges,p=.75,list=FALSE)
trainrf<-datarf[trainindex,]
testrf<-datarf[-trainindex,]


#############################regression trees

#tuning para cp
modelort <- train(charges~., data=trainrf,method = "rpart", tuneLength = 10)
modelort
plot(modelort)
#modelo definitivo de arbol de decision
regressiontree<-rpart(charges~.,data=trainrf,control = rpart.control(cp = 0.002547338))
regressiontree

summary(regressiontree)

prediccionrt<-predict(regressiontree,newdata=testrf)
#plotear actuales vs predecidos
plot(prediccionrt,testrf$charges)

#plot del arbol 
arbolrt<-as.party(regressiontree)
plot(arbolrt)
rpart.plot::rpart.plot(regressiontree)

#metricas RT
RMSERT<-RMSE(predict(regressiontree,newdata=trainrf),trainrf$charges)
MAERT<-mean(abs(predict(regressiontree,newdata=trainrf)-trainrf$charges))
R2RT<-R2(predict(regressiontree,newdata=trainrf),trainrf$charges)

RMSERT
MAERT
R2RT

###########################RANDOM FOREST####################3
#tuning para mtry
modelorf1<-train(charges~., data=trainrf,method="rf",importance=TRUE,ntree=50)
modelorf1
plot(modelorf1)
#modelo definitivo de randomforest
modelorf2<-randomForest(charges~., data=trainrf,importance=TRUE,ntree=500,mtry=5)
modelorf2
plot(modelorf2)

prediccionrf<-predict(modelorf2,testrf)

#plotear actuales vs predecidos
plot(prediccionrf,testrf$charges)
#metricas RF
RMSERF<-RMSE(predict(modelorf2,trainrf),trainrf$charges)
MAERF<-mean(abs(predict(modelorf2,trainrf)-trainrf$charges))
R2RF<-R2(predict(modelorf2,trainrf),trainrf$charges)

RMSERF
MAERF
R2RF

comparacion<-cbind(c(RMSERT,MAERT,R2RT),c(RMSERF,MAERF,R2RF))
colnames(comparacion)<-c("Árbol de regresión","Random Forest")
rownames(comparacion)<-c("RMSE","MAE","R2")
View(comparacion)
View(datarf)
