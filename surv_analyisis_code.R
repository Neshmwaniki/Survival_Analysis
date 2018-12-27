
#### Surv Analysis

pkgs<-c("dplyr","lubridate","ggplot2","readxl","tidyr","tidyverse", "xlsx", "Hmisc", "mice", "dummies", "caret", "glmnet","MASS", "haven", "survival", "survminer")

miss_pkgs <- pkgs[!pkgs %in% installed.packages()[,1]] # vector of missing packages

# install the missing packages
if(length(miss_pkgs)>0){
  install.packages(miss_pkgs)
}

# Loading all the packages

invisible(lapply(pkgs,library,character.only=TRUE))


## Import data

obt <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/I&M-DS/Obituaries_Dataset.csv")
var.types <- sapply(obt, class)

character.vars <- names(var.types)[var.types %in% c("character")]

factor.vars <- names(var.types)[var.types %in% c("factor")]

cat.vars <- names(var.types)[var.types %in% c("character","factor")]

num.vars <- names(var.types)[var.types %in% c("numeric")]


# scheming top rows
head(obt[,1:18])
glimpse(obt)


#parsing dates
obt$an_date <- lubridate::mdy(obt$Announcement)

obt$dt_date <- lubridate::mdy(obt$Death)

obt$br_date <- lubridate::mdy(obt$Burial)




#compute death to announce and death to burial

obt$death2Announce <- obt$an_date - obt$dt_date

obt$death2Burial <- obt$br_date - obt$dt_date

obt$death2Burial[is.na(obt$death2Burial)] <- 0
obt$death2Announce[is.na(obt$Announcement)] <- 0

obt$death2Burial <- as.numeric(obt$death2Burial, units = "days")
obt$death2Announce <- as.numeric(obt$death2Announce, units = "days")


# Create a survival object

obt$status <- ifelse(obt$Death == "", 0, 1)

obt$status <- as.numeric(obt$status)



glimpse(obt)

### Subset for survival curve on Gender



obt2 <- obt %>% filter(death2Burial >= 0 & death2Burial < 365)
obt2 <- dplyr::select(obt2,Gender,death2Burial,status)


glimpse(obt2)

str(obt2)


# Fit survival data using the Kaplan-Meier method
obt_surv_object <- Surv(time = obt2$death2Burial, event = obt2$status)

obt_surv_object


fit1 <- survfit(obt_surv_object ~Gender, data = obt2)
summary(fit1)


ggsurvplot(fit1, data = obt2,pval = T)


###### Cleaning continued

str(obt)

histogram(obt$Age)


mean(obt$Age, na.rm = T)

sd(obt$Age, na.rm = T)

median(obt$Age, na.rm = T)

quantile(obt$Age, na.rm = T)
 

### Filter of negative durations in death to burial and to announcement
obt_df <- obt %>% filter(death2Burial >= 0 & death2Burial < 365) %>% filter(death2Announce >= 0 & death2Announce < 365)

### Preparing Age

obt_df$agegroup[obt_df$Age <= 20] <- "AgeGroup1"
obt_df$agegroup[obt_df$Age  >20 & obt_df$Age <= 39] <- "AgeGroup2"
obt_df$agegroup[obt_df$Age  >39 & obt_df$Age <= 59] <- "AgeGroup3"
obt_df$agegroup[obt_df$Age  >59 & obt_df$Age <= 79] <- "AgeGroup4"
obt_df$agegroup[obt_df$Age  >79 & obt_df$Age <= 99] <- "AgeGroup5"
obt_df$agegroup[obt_df$Age  >99 & obt_df$Age <= 119] <- "AgeGroup6"


# Get levels and add "None"
levels <- levels(obt_df$agegroup)
levels[length(levels)+1] <- "None"

# refactor AgeGroup to include "None" as a factor level
# and replace NA with "None"
obt_df$agegroup <- factor(obt_df$agegroup, levels = levels)

obt_df$agegroup[is.na(obt_df$agegroup)] <- "None"


obt_df$agegroup <- as.factor(obt_df$agegroup)

str(obt_df)

### Dropping empty levels

obt_df <- droplevels.data.frame(obt_df)
str(obt_df)

library(plyr)

obt_df$Color <- mapvalues(obt_df$Color, from = c("Yes","No"), to = c("yes","no"))

obt_df$Married <- mapvalues(obt_df$Married, from = c("Yes","No"), to = c("yes","no"))

obt_df$Burial_Week <- mapvalues(obt_df$Burial_Week, from = c("Weeekday","weekday","Weekday","Weekeday","weekend","Weekend"), to = c("weekday","weekday","weekday","weekday","weekend","weekend"))

obt_df$Residence <- mapvalues(obt_df$Residence, from = c("Yes","No"), to = c("yes","no"))

obt_df$Cause_of_Death <- mapvalues(obt_df$Cause_of_Death, from = c("accident","Accident","illness","Illness","ilness"), to = c("accident","accident","illness","illness","illness"))


## maintaining numerics
obt_df$Word_Count <- as.numeric(obt_df$Word_Count)
obt_df$No_of_Relatives <- as.numeric(obt_df$No_of_Relatives)
obt_df$Distance_Death <- as.numeric(obt_df$Distance_Death)
obt_df$Distance_Morgue <- as.numeric(obt_df$Distance_Morgue)


#### Dropping unnecessary variables for analysis

var.drop <- c("Name","Announcement","Death","Burial","Death_to_Announce","Death_to_Burial","Announce_to_Burial","Repetition","Age","an_date","dt_date","br_date")

obt3 <- obt_df

obt3[,var.drop] <- NULL

str(obt3)

# Checking for missingness
colSums(is.na(obt3))

obt_mis <- obt3

dim(obt_mis)


# Return the column names containing missing observations
list_na <- colnames(obt_mis)[ apply(obt_mis, 2, anyNA) ]
list_na

## maintaining numerics

# obt_mis$Age <- as.numeric(obt_mis$Age)
obt_mis$Size <- as.numeric(obt_mis$Size)
obt_mis$No_of_Children <- as.numeric(obt_mis$No_of_Children)
obt_mis$Significant_Children <- as.numeric(obt_mis$Significant_Children)
obt_mis$Significant_Relatives <- as.numeric(obt_mis$Significant_Relatives)
# obt_mis$death2Announce <- as.numeric(obt_mis$death2Announce)


obt_mis_impute_mean <-  data.frame(sapply(obt_mis,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))

## Preparing Fundraising Column
df <- obt_mis_impute_mean[!(is.na(obt_mis_impute_mean$Fundraising) | obt_mis_impute_mean$Fundraising==""|obt_mis_impute_mean$Fundraising==1), ]

df$Fundraising <- mapvalues(df$Fundraising, from = c("2","3"), to = c("Yes","No"))

df$Fundraising <- as.factor(df$Fundraising)

### Train Test Split

set.seed(121)
trainRowNumbers <- createDataPartition(df$Fundraising,p=0.8,list = F)

train_df <- df[trainRowNumbers,]
test_df <- df[-trainRowNumbers,]

# Store X and Y for later use.
x <- train_df[,-11]
y <- train_df$Fundraising


# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.

dummies_model <- dummyVars(Fundraising~.,data = train_df)   

# Create the dummy variables using predict.
train_obt_complete3 <- predict(dummies_model, newdata = train_df)

train_obt_complete3 <- data.frame(train_obt_complete3)

str(train_obt_complete3)

# Preprocess for range
preProcess_range_model <- preProcess(train_obt_complete3, method = "range")

train_obt_complete4 <- predict(preProcess_range_model,newdata = train_obt_complete3)

# Append the Y variable

train_obt_complete4$Fundraising <- y

apply(train_obt_complete4[,1:10],2,FUN = function(x){c("min" = min(x),"max" = max(x))})

## Visualizing important vars
featurePlot(x = train_obt_complete4[,1:33],
            y = train_obt_complete4$Fundraising,
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = .7)),
            scales = list(x=list(relation = "free"),
                          y=list(relation = "free")))




## Feature selection using recursive feature elimination
set.seed(121)

subsets <- c(1:10)
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv",repeats = 5, verbose = F)
lmProfile <- rfe(x=train_obt_complete4[,1:33], y = train_obt_complete4$Fundraising, sizes = subsets, rfeControl = ctrl)

lmProfile

# The top 5 variables (out of 33):
#   Distance_Death, Cause_of_Death, County_Death, No_of_Relatives, Size

# mars
set.seed(121)

model_mars = train(Fundraising ~., data = train_obt_complete4,method = 'earth')
fitted <- predict(model_mars)

model_mars

plot(model_mars, main="Model Accuracies with MARS")


varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")

# Define the training control
fitControl <- trainControl(method = "cv",number = 5,savePredictions = "final",classProbs = T,summaryFunction = twoClassSummary)

# Step 1: Tune hyper parameters by setting tuneLength
set.seed(100)

model_mars2 = train(Fundraising ~.,data = train_obt_complete4, method = "earth", tuneLength = 5, metric = "ROC",trControl = fitControl)

model_mars2

# Using Hyper parameter tuning
marsGrid <- expand.grid(nprune = c(2,4,6,8,10), degree = c(1,2,3))

#Tune hyper parameters by setting tuneGrid

set.seed(123)

model.mars3 = train(Fundraising ~.,data = train_obt_complete4, method = "earth", metric = "ROC", tuneGrid = marsGrid,trControl = fitControl)

model.mars3

# random forest

set.seed(124)
model_rf= train(Fundraising ~.,data = train_obt_complete4, tuneLength=5,trControl = fitControl, method = 'rf')

model_rf

# Adaboost
set.seed(125)
model_adaboost= train(Fundraising ~.,data = train_obt_complete4, tuneLength=2,trControl = fitControl, method = 'adaboost')

model_adaboost

# xgBoost Dart
set.seed(126)
model_xgbdart = train(Fundraising~., data = train_obt_complete4, tuneLength = 5, method = "xgbDART",trControl = fitControl,verbose = F)
model_xgbdart

# SVM
set.seed(127)
model_svm = train(Fundraising ~., data = train_obt_complete4, method = "svmRadial", tuneLength = 15, trControl = fitControl)
model_svm


# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST = model_adaboost,RF = model_rf, XGBDART = model_xgbdart, MARS = model.mars3, SVM = model_svm))

summary(models_compare)

# Drawing box plots to compare models

scales <- list(x = list(relation = "free"), y = list(relation = "free"))

bwplot(models_compare,scales = scales)  

#### Preparing test data

testData2 <- predict(dummies_model, test_df)

# Transform the features to range between 0 and 1
testData3 <- predict(preProcess_range_model, testData2)

# Predict on testData (Using model with highest ROC)

predicted <- predict(model_rf, testData3)
# predictedd <- predict(model_xgbdart, testData3)
# predicteddd <- predict(model_adaboost, testData3)
head(predicted)

head(test_df$Fundraising)

table(test_df$Fundraising)
table(predicted)

# Compute the confusion matrix

confusionMatrix(table(predicted, test_df$Fundraising)) 
# confusionMatrix(table(predictedd, test_df$Fundraising)) 
# confusionMatrix(table(predicteddd, test_df$Fundraising)) 

### Combining predictions from multiple models

library(caretEnsemble)
# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3,savePredictions = T,classProbs = T)

algorithmList <- c("rf","adaboost","earth","xgbDART","svmRadial")

set.seed(131)
models <- caretList(Fundraising~.,data = train_obt_complete4, trControl = trainControl,methodList = algorithmList)

results <- resamples(models)
summary(results)

# Box plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results,scales = scales)  

# Create the trainControl
set.seed(111)
stackControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3,savePredictions = T, classProbs = T)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models,method = "glm",metric = "Accuracy",trControl = stackControl)
print(stack.glm)

# Predict on testData
stack_predicted <- predict(stack.glm, newdata = testData3)

confusionMatrix(table(stack_predicted,test_df$Fundraising))
