######## SVM Number Recogniser ######
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5. Hyperparameter tuning and cross validation
# 6. Valdiating the model after cross validation on test data

##############################################

# 1. Business Understanding: 
# The objective is to correctly classify the handwritten digits written in an image 
# based on the pixel values given as features

##############################################


# 2. Data Understanding: 

# Number of Instances: 60,000 in training data 
#                      however only 15% of the sample is taken to build the model
# Number of Attributes: 785

#############################################

#3. Data Preparation: 

#Loading Neccessary libraries

library(ggplot2)
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(gridExtra)


#Loading Data

Data_train <- read.csv("mnist_train.csv",header = FALSE)
Data_test <- read.csv("mnist_test.csv", header = FALSE)

View(Data_train)
View(Data_test)

#Understanding Dimensions
dim(Data_train)

#Structure of the dataset
str(Data_train)

#printing first few rows
head(Data_train)

#Exploring the data
summary(Data_train)

#checking missing value in both train and test data
sapply(Data_train, function(x) sum(is.na(x)))
sapply(Data_test, function(x) sum(is.na(x)))

#Making our target class to factor in both test and train dataset 
Data_train$V1 <-factor(Data_train$V1)
Data_test$V1  <-factor(Data_test$V1) 


###################################################

## Seed the data and use 15% of the sample train data from the dataset.

set.seed(1)
train.indices = sample(1:nrow(Data_train), 0.15*nrow(Data_train))
train = Data_train[train.indices, ]
View(train)

test.indices = sample(1:nrow(Data_test), 0.15*nrow(Data_test))
test = Data_test[test.indices, ]
View(test)
###################################################

# 4. Model Building 

#  4.1 Linear kernel

#Using Linear Kernel
Model_linear <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$V1)

#printed the model to find the default value of C
print(Model_linear)

# Accuracy - 0.9307


#  4.2 RBF Kernel

#Using RBF Kernel
Model_RBF <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$V1)

#printed the model to find the default value of Sigma
print(Model_RBF)

# Accuracy - 0.9613

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 


trainControl <- trainControl(method="cv", number=3)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
# Making grid of  C values. 

set.seed(7)
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

# Performing 3 -fold cross validation
fit.svm <- train(V1 ~., data=train, method="svmRadial", metric=metric, tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm)

# Plotting model results
plot(fit.svm) 


################################################

# Valdiating the model after cross validation on test data

Eval_Lineartest <- predict(fit.svm,test)
confusionMatrix(Eval_Lineartest,test$V1)

#####################################################3