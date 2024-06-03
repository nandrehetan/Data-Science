cp<- read.csv("water_potability.csv")

#NA values
str(cp)
summary(cp)

#impute missing values using knn
library(DMwR2)
cp<-knnImputation(cp, k = 10, scale = TRUE, meth = "weighAvg",distData = NULL)

#remove outliers using boxplot
boxplot(cp$ph,cp$Hardness,cp$Solids,cp$Chloramines,cp$Sulfate,cp$Conductivity,cp$Organic_carbon,cp$Trihalomethanes,cp$Turbidity)
for (i in 2:10) {
  cp<-cp[!cp$ph %in% boxplot.stats(cp$ph)$out,]
  cp<-cp[!cp$Hardness %in% boxplot.stats(cp$Hardness)$out,]
  cp<-cp[!cp$Solids %in% boxplot.stats(cp$Solids)$out,]
  cp<-cp[!cp$Chloramines %in% boxplot.stats(cp$Chloramines)$out,]
  cp<-cp[!cp$Sulfate %in% boxplot.stats(cp$Sulfate)$out,]
  cp<-cp[!cp$Conductivity %in% boxplot.stats(cp$Conductivity)$out,]
  cp<-cp[!cp$Organic_carbon %in% boxplot.stats(cp$Organic_carbon)$out,]
  cp<-cp[!cp$Trihalomethanes %in% boxplot.stats(cp$Trihalomethanes)$out,]
  cp<-cp[!cp$Turbidity %in% boxplot.stats(cp$Turbidity)$out,]
}
boxplot(cp)

#class imbalance
table(cp$Potability)
prop.table(table(cp$Potability))
library(ROSE)
library(smotefamily)
over=ovun.sample(Potability~.,data=cp,method="over")
cp=over$data
table(cp$Potability)
prop.table(table(cp$Potability))

cp$Potability=as.factor(cp$Potability)

#create training and testing data partitions
library(caret)
set.seed(9999)
train <- createDataPartition(cp[,"Potability"],p=0.8,list=FALSE)
trn <- cp[train,]
tst <- cp[-train,]

#Algorithms applying
ctrl<-trainControl(method = "cv",number = 10)

#Decision Trees
dec<-train(Potability~.,data = trn,method="rpart",trControl=ctrl,tuneLength=30)
pred1<-predict(dec,tst)
confusionMatrix(table(tst[,"Potability"],pred1))

#Random forest
rand<-train(Potability~.,data = trn,method="rf",trControl=ctrl,tuneLength=30)
pred2<-predict(rand,tst)
confusionMatrix(table(tst[,"Potability"],pred2))

library(doParallel)
#Xgb linear
#varImp(XgbLinear)
registerDoParallel(cores=4)
set.seed(1)
xgbLinear<-train(Potability~.,data=trn,method="xgbLinear",trControl=ctrl)
xgbLinear
pred3<-predict(xgbLinear,tst)
confusionMatrix(table(tst[,"Potability"],pred3))

#XGboast tree
#VarImp(xgbTree)
registerDoParallel(cores=4)
set.seed(1)
xgbTree<-train(Potability~.,data=trn,method="xgbTree",trControl=ctrl)
xgbTree
pred4<-predict(xgbTree,tst)
confusionMatrix(table(tst[,"Potability"],pred4))

