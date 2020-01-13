
# Clear environment

rm(list = ls())

# Setting the random number generator seed so that our results are reproducible


set.seed(30)

# Set local directory:

setwd("C:/Users/aksha/Desktop/DM_p1")

#Load Data from Excel
library(readxl)
cd <- read_excel("census-adult.xlsx", na="?")

#str(cd)
#table(cd$Income)
#head(cd)

#Data Manipulation

cd[cd == " ?"] <- NA

# Reomoving data of the factors that has new level(due to small sample size)
cd[cd == "Poland"] <- NA
cd[cd == "Married-AF-spouse"] <- NA

#cd

#Remove entries with NA
cd1 <- na.omit(cd)
#cd1


library(data.table)
set.seed(30)

cds<- data.table(cd1)

#Creating sample of 2K
cds<-cds[sample(.N, 2000)]

#Spliting data into 60-40 ratio
library(dplyr)
train<-sample_frac(cds, 0.6)
sid<-as.numeric(rownames(train)) 
# because rownames() returns character
test<-cds[-sid,]


#build model
library(rpart)
library(rpart.plot)
rpart_model <- rpart(Income~., data=train, method = "class",parms = list(split = 'information'))
printcp(rpart_model)

#Ploting the model
rpart.plot(rpart_model,nn=TRUE)

#Prediction
pred <- predict(object=rpart_model,test,type="class")
pred

#Confusion Matrix
conf_matrix <- table(pred,test$Income)
conf_matrix

library(caret)
confusionMatrix(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

precision <- conf_matrix[1,1]/sum(conf_matrix[1,1:2])
precision

recall <- conf_matrix[1,1]/sum(conf_matrix[1:2,1])
recall

f_score <- 2 * (precision * recall) /(precision + recall)
f_score

#----------------------------------------------------------------------------------#
#build model after withholding one column
library(rpart)
library(rpart.plot)
rpart_model1 <- rpart(Income~ Age+Work.Class+Education+Education.Number+Maritial.Status+Occupation+Relationship+Race+Sex+Capital.Gain+Capital.Loss+Hours.per.week+Native.Country+Income, data=train, method = "class",parms = list(split = 'information'))
printcp(rpart_model1)

#Ploting the new model
rpart.plot(rpart_model1,nn=TRUE)

#Prediction
pred1 <- predict(object=rpart_model1,test,type="class")
pred1
conf_matrix1 <- table(pred1,test$Income)
conf_matrix1

library(caret)
confusionMatrix(conf_matrix1)

accuracy1 <- sum(diag(conf_matrix1)) / sum(conf_matrix1)
accuracy1

precision1 <- conf_matrix1[1,1]/sum(conf_matrix1[1,1:2])
precision1

recall1 <- conf_matrix1[1,1]/sum(conf_matrix1[1:2,1])
recall1

f_score1 <- 2 * (precision1 * recall1) /(precision1 + recall1)
f_score1
#------------GINI INDEX-----------------------------------
library(rpart)
library(rpart.plot)
rpart_gini <- rpart(Income~., data=train, method = "class",parms = list(split = 'gini'))
printcp(rpart_gini)
#Ploting the model
rpart.plot(rpart_gini,uniform=FALSE)
#Prediction
pred <- predict(object=rpart_gini,test,type="class")
pred

#Confusion Matrix
conf_matrix <- table(pred,test$Income)
conf_matrix

library(caret)
confusionMatrix(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

precision <- conf_matrix[1,1]/sum(conf_matrix[1,1:2])
precision

recall <- conf_matrix[1,1]/sum(conf_matrix[1:2,1])
recall

f_score <- 2 * (precision * recall) /(precision + recall)
f_score
#-----------------Naive Bayes---------------------------------

library(e1071)
nb_model = naiveBayes(as.factor(Income) ~., data=train)
nb_model
modelPred <- predict(nb_model, train)
cMatrix <- table(modelPred, train$Income)
library(caret)
plot(cMatrix)
confusionMatrix(cMatrix)