library(caret)
library(randomForest)
library(clue)
library(rpart)
library(e1071)
library(ROCR)

#Load data
award1 = read.csv('G:/GWU/DAT6310 Mining/Project/Winner.csv', header = F)
award0 = read.csv('G:/GWU/DAT6310 Mining/Project/Notwinner.csv', header = F)

#Combine winner data and non-winner data
award = rbind(award0, award1)

#Assign winner (dummy) variable
award[1:665,501] = 0
award[666:1362,501] = 1
names(award)[501] = 'Winner'
#Change data type to factor
award$Winner = as.factor(award$Winner)

#Create training (70%) and testing (30%) datasets
Train_Size = floor(0.70 * nrow(award))
set.seed(65535)
train_ind = sample(seq_len(nrow(award)), size = Train_Size)
train = award[train_ind, ]
test = award[-train_ind, ]

#SVM nu-classification, Radial Basis Kernel
svm.fit = svm(Winner~., data=train, nu=0.2,
              type = "nu-classification", kernel="radial")
#SVM predict
pred_temp = predict(svm.fit,test)
#SVM result
confusionMatrix(pred_temp, test$Winner)$table


#Logistics Regression
LR = glm(Winner~., data = train, family = binomial())
#Logistics Regression predict
LRpred = predict(LR, test)
#Logistics Regression result
LRpred_class = rep(0, length(LRpred))
for(i in 1:length(LRpred))
{
  if(LRpred[i]>0)
  {
    LRpred_class[i] = 1
  }
}
confusionMatrix(LRpred_class, test$Winner)$table

#Random Forest
set.seed(65535)
RF = randomForest(Winner~., data=train)
#Random Forest predict
RFpred = predict(RF, test)
#Ramdom Forest result
confusionMatrix(RFpred, test$Winner)$table

#Kmean
set.seed(65535)
kmean <- kmeans(train[,1:500], centers=2, nstart=20)
#Kmean predict
kmeanpred = cl_predict(kmean, test[,1:500])
kmeanpred[kmeanpred==1] <- 0
kmeanpred[kmeanpred==2] <- 1
kmeanpred = as.factor(kmeanpred)

#Kmean result
confusionMatrix(kmeanpred, test$Winner)$table

