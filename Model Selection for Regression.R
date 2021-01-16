##### Model Selection #####

# 1. Multiple Linear Regression Model
# 2. Decison Tree Regression Model
# 3. Random Forest Regression Model 

# Importing th0e (Home Price) dataset
setwd("C:/Users/Admin/Downloads")
library(readxl)
dataset = read_excel("Home price.xlsx")
head(dataset)
summary(dataset)

### Multiple Linear Regression Model ###

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Price, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
model1 = lm(formula = Price ~ .,
               data = training_set)

model1
# Multiple Linear regression model summary
summary(model1)

# Predicting the Test set results
y_pred_1 = predict(model1, newdata = test_set)

###   Regression Tree   ###

# Fitting Decision Tree Regression to the df
# install.packages('rpart')
library(rpart)
library(rpart.plot)

model2 = rpart(formula = Price ~ .,
                  data = dataset, method = "anova")
model2

# Plotting the tree
rpart.plot(model2)

# Prediction
y_pred_2=predict(model2,dataset[,-1])
y_pred_2

## Create new dataset for predict and Actual values
newdf=data.frame(dataset$Price,y_pred_2)
head(newdf)
dim(newdf)

## Model accuracy
Actual=dataset$Price
rss2 <- sum((y_pred_2 - Actual) ^ 2)  ## residual sum of squares
tss2 <- sum((Actual - mean(Actual)) ^ 2)  ## total sum of squares
rsq2 <- 1 - rss2/tss2
rsq2

###   Random Forest Regression Model   #####

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)

model3 = randomForest(x=dataset[,-1],y=dataset$Price,ntree = 1000)
model3

# Predicting a new result with Random Forest Regression
y_pred_3 = predict(model3, dataset[,-1])


## Model accuracy
Actual=dataset$Price
rss3 <- sum((y_pred_3 - Actual) ^ 2)  ## residual sum of squares
tss3 <- sum((Actual - mean(Actual)) ^ 2)  ## total sum of squares
rsq3 <- 1 - rss3/tss3
rsq3

## Comparision of all three model predictied values with original test set values.
DT_Prediction=y_pred_2
RF_Prediction=y_pred_3
compare=cbind(Actual,DT_Prediction,RF_Prediction)
head(compare,20)

