stevens = read.csv('stevens.csv')

str(stevens)

library(caTools)

set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, split == TRUE)
Test = subset(stevens, split == FALSE)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
StevensTree = rpart(Reverse~Circuit + Issue+ Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", control = rpart.control(minbucket = 25))
prp(StevensTree)

PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
install.packages('ROCR')
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
#PredictROC has 2 columns, the probability of true and false. the second column is yes
ROCRpred = prediction(PredictROC[,2],Test$Reverse)
perf= performance(ROCRpred, "tpr" ,"fpr")
plot(perf)


install.packages("randomForest")
library(randomForest)

StevenForest = randomForest(Reverse ~Circuit + Issue+ Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize= 25, ntree=200)
#nodesize = minbucket, ntree = how many tree should we build
# Random forest needs to make sure the outcome will be a factor. 
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevenForest = randomForest(Reverse ~Circuit + Issue+ Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize= 25, ntree=200)
PredictForest = predict(StevenForest,newdata = Test)
table(Test$Reverse, PredictForest)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
fitControl =  trainControl(method ="cv", number =10) ##number = how many sets
cartGrid = expand.grid(.cp=(1:50)*0.01)
train(Reverse ~ Circuit + Issue+ Petitioner + Respondent + LowerCourt + Unconst, data =Train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
##Accuracy was used to select the optimal model using  the
##largest value.
##The final value used for the model was cp = 0.17.  The minbucket = 17

StevensTreeCV = rpart(Reverse ~ Circuit + Issue+ Petitioner + Respondent + LowerCourt + Unconst, method = "class", data =Train, control = rpart.control(cp=0.17))
## what is cp 
PredictCV = predict(StevensTreeCV, newdata =Test , type ="class")
table(Test$Reverse,PredictCV)