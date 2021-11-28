#### Read the data set ------------

(bank_full)
head(bank_full)
str(bank_full)
data.frame(bank_full)
str()
as.factor("marital$bank_full")
levels(married, single, divorce))

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

library(data.table)
temp <- tempfile()

dim(data)
names (data)
summary.data.frame(data)
str(data)

download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip', temp)
data  <- data.table(read.table(unzip(zipfile = temp, 
                                     files = 'bank-full.csv', 
                                     junkpaths = TRUE), header = TRUE, sep = ',', stringsAsFactors = FALSE))


download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip', temp)
bankfull  <- read.csv(unzip(zipfile = temp, 
                                     files = 'bank-full.csv', 
                                     junkpaths = TRUE), header = TRUE, sep = ',', stringsAsFactors = FALSE)


bankfull
summary.data.frame(bankfull)
dim (bankfull)
str(bankfull)
bankfulldata<-as.factor(bank_full)
dim(bankfulldata)


### Convert as a factor 
library(dplyr)
bankfullfactors <- bankfull %>% 
  mutate_if(is.character, as.factor)
str(bankfullfactors)
summa
library(readr)
bank_full22 <- read_csv("C:/Users/maryo.LAPTOP-M4A2D3FV/Desktop/R-project/bank-full.csv", stringsAsFactors = false)

-- Column specification ----------------------------------------------------------
  cols(
    `age;"job";"marital";"education";"default";"balance";"housing";"loan";"contact";"day";"month";"duration";"campaign";"pdays";"previous";"poutcome";"y"` = col_factor()
  )

class(bank_full)
dim(bank_full)
summary.data.frame((bank_full))
summary(bank_full)

bank_full22 <-read_table("C:/Users/maryo.LAPTOP-M4A2D3FV/Desktop/R-project/bank-full.csv")

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
pairs(bankfactors, main = "Exploratory analisys of the bank Marketing data set", col = 4) 
#pairs(bankfactors, # Data
                         pch = 19, # Pch symbol
                         col = 4,  # Color
                         main = "Exploratory analisys of the bank data set",    # Title
                         gap = 0,           # Subplots distance
                         row1attop = FALSE, # Diagonal direction
                         labels = colnames(df), # Labels
                         cex.labels = 0.8,  # Size of diagonal texts
                         font.labels = 1)   # Font style of diagonal texts)
### Correctlation between the variables .... select columms dir be=
install.packages("GGally")
library(GGally)

ggpairs(bankfactors)  


########## Classification Trees #############

# Fit a classification tree (same data)
library(rpart)
fit.r <- rpart(y~.,data= bankfactors)
pred.r <- predict(fit.r,newdata=wine,type="class")
summary(pred.r)



#Ensure that Class is categorical
### Convert as a factor 
library(dplyr)
bff <- bank_full %>% 
  mutate_if(is.character, as.factor)
str(bff)
summary(bff)


class(loan$bank_full)

loan$bank_full<-as.factor(Loan$bank_full)

# Check the data
cols(bank_full)
