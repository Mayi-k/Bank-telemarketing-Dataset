### Libraries -------

library(ggplot2)
library(caret) # Accuracy
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(caTools)
library(descr)
library(rlang)
library(data.table)
library(dplyr)
library(readr)

library(data.table)

#------------------------------------------------------------------------------------------------------------------------------
### Read the data set 

library(readxl)
bank_bank <- read.csv("C:/Users/maryo.LAPTOP-M4A2D3FV/Downloads/bank_bank.csv")
View(bank_bank)


### Convert characterr as a factor ,Ensure that Class is categorical
library(dplyr)
bankfactors <- bank_bank %>% 
  mutate_if(is.character, as.factor)
str(bankfactors)

### Data analisis
dim(bankfactors)
str((bankfactors))
summary.data.frame((bankfactors))
class((bankfactors))

### Visualizations Pairs & GGpairs 
pairs(bankfactors, main = "Exploratory analisys of the bank Marketing data set", col = 4) 

### Correctlation between the variables .... select columms dir be=
install.packages("GGally")
library(GGally)

ggpairs(bankfactors) ### all data set 
ggpairs(bankfactors,
        columns = c(3,4,5,17)!"hr"


####------------------------------------Random Forest --------------------------------------------
### Using the Variable Y to determine id the client with susbribe or nor to the product

# Load the randomForest package
library(randomForest)

#Data Partition sample(2, nrow(data), replace=TRUE, prob= c(0.7,0.3))

set.seed(123)
train <- sample(nrow(bankfactors), 0.7*nrow(bankfactors), replace = FALSE)
TrainSet <- bankfactors[train,]
ValidSet <- bankfactors[-train,]

str(TrainSet )
str(ValidSet )

# Implement the random forest algorithm
fit.rf <- randomForest(Y~.,data=bankfactors)###use data set without split 
rf_train<- randomForest(Y~.,data = TrainSet, importance = TRUE, noutlier = 0, na.action = na.omit)

# Examine the results
fit.rf###use data set without split 
rf_train


#Prediction and confusion Matrix Trainset 
library(caret)
library(rlang)
library(ggplot2)
library(laticce)

p1_rf<- predict(rf_train, TrainSet)

confusionMatrix(p1_rf, TrainSet$Y)
table(TrainSet$Y)

#Prediction and confusion  Valid set Trainset 
p2_rf<- predict(rf_train, ValidSet)
confusionMatrix(p2_rf, ValidSet$Y)

table(ValidSet$Y)

# Let's look at variable importance
importance(rf_train)

# Or via this nice plot
varImpPlot(rf_train, main = "Variables Importance")

##Random Forest Attributtes--------------##
attributes(rf_train)
rf_train$confusion
rf_train$err.rate

rf_train$importance
rf_train$type


#Random Forest ERROR
plot(rf_train, main = " Error Random Forest")


##Variable importance# Note! Remember MeanDecreaseGini represents the mean decrease in node impurity 
# (and not the mean decrease in accuracy)
varImpPlot(rf_train)

##VarUsed
varUsed(rf_train)

#NUMBER OF NODES OF THE TREE

hist(treesize(rf_train),main = "NUMBER OF NODES OF THE TREE", col = "Blue")### Random forest is used 320 trees 
#gettree- get the specific tree is this cases #250
getTree(rf_train,250,labelVar = TRUE)
getTree(rf_train,230,labelVar = TRUE)



#Partial Plot
partialPlot(rf_train,TrainSet,Y)
plot(rf_train$importance)



