library(gvlma)
library(olsrr)
library(ggplot2)
library(zoo)
library(randomForest)
library(dplyr)
library(ggthemes)
library(scales)
library(mice)
library(Amelia)
library(olsrr)
library(ISLR) 
library(rstudioapi) 
library(ggpubr)
library(corrplot)
library(caret)
library(Rsampling)
library(datasets)
library(NLP)
library(tm)
library(class)
library(gmodels)
library(mltools)
library(data.table)
library(tidyverse)
library(mlbench)
library(gmodels)
library(class)
library(randomForest)
install.packages("ROCR")
library(ROCR)
library(pROC)
#########################################set working directory#############################################
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
###########################################################################################################
#Banking Data Set for Marketing Campaigns https://www.kaggle.com/prakharrathi25/banking-dataset-marketing-targets

#First we load the Loans Dataset of a Dummy Bank
bankingdata <- read.csv("train.csv", stringsAsFactors = TRUE, header= TRUE, na.strings=c("?"), sep = ";")
banking_df <- as.data.frame(bankingdata)
str(banking_df)
head(banking_df)

# identify missing data
missmap(banking_df, main = "Banking Missing values vs observed")
#summary of the entire dataset
summary(banking_df)

#histogram of the loan data
hist(banking_df$duration, main="Banking Dataset Marketing Targets", xlab= "Duration since last contact", col="orange")
hist(banking_df$age, main="Banking Dataset Marketing Targets", xlab= "Customer Age Distribution", col="orange")

############################################
#Random Forrest start
############################################
### Convert characterr as a factor ,Ensure that Class is categorical
banking_df <- banking_df %>% 
  mutate_if(is.character, as.factor)
str(banking_df)
dim(banking_df)
###############################################

# Make dependent variable as a factor (categorical)
#our target dependent variable is "y" with yes and no values
banking_df$y = as.factor(banking_df$y)
class(banking_df$y)

#get 75% of data randomly [test set split]
index <- sample(1:nrow(banking_df), nrow(banking_df) * .75, replace=FALSE)
#split in test / train
banking_df_train <- banking_df[index, ]
banking_df_test <- banking_df[-index, ] #-index = complementary set to index
y_outcome <- banking_df_train$y

set.seed(71)
rf <-randomForest(y~.,data=banking_df_train, ntree=500) 
print(rf)
#Note : If a dependent variable is a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
#The number of variables selected at each split is denoted by mtry in randomforest function.

#Find the optimal mtry value
mtry <- tuneRF(banking_df_train[-17],banking_df_train$y, ntreeTry=500, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, doBest=TRUE)
print(mtry)

#####Build model again using best mtry value which is 6 in our case
set.seed(71)
rf <-randomForest(y~.,data=banking_df_train, mtry=6, importance=TRUE, ntree=500)
print(rf)

#Evaluation, Prediction and Calculate Performance Metrics

#importance of the variables in order
importance(rf)
varImpPlot(rf)

# Plot the RF Model
plot(rf)

# Partial Plot the RF model and its dependence on age
partialPlot(x=rf, pred.data=banking_df_train, x.var=age, which.class = "yes")

#Prediction
pred1=predict(rf,type = "prob")
head(pred1)

#create a performance prediction
perf = prediction(pred1[,2], banking_df_train$y)
perf

# 1. Area under curve
auc = performance(perf, measure="auc")
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
plot(pred3,col=rainbow(10))

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Bank Data Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


# 4 Confusion Matrix 

#predict
pred4 = predict(rf, banking_df_train)

# a confusion matrix shows "train" banking data accuracy is 100%
confusionMatrix(pred4,y_outcome)

table(y_outcome, pred4)


#5 Classification Error
rf$confusion


#5 Accuracy
cm = table(pred4, y_outcome)
cm

accuracy = (sum(diag(cm)))/sum(cm)
accuracy*100

#### Test 

pred2 <- data.frame(pred1)
head(pred2)
pred2_roc <- roc(pred4, pred2$yes)

plot(pred_test_roc)
