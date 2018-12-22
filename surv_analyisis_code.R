
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
 


### Preparing Age


obt$agegroup[obt$Age <= 20] <- "AgeGroup1"
obt$agegroup[obt$Age  >20 & obt$Age <= 39] <- "AgeGroup2"
obt$agegroup[obt$Age  >39 & obt$Age <= 59] <- "AgeGroup3"
obt$agegroup[obt$Age  >59 & obt$Age <= 79] <- "AgeGroup4"
obt$agegroup[obt$Age  >79 & obt$Age <= 99] <- "AgeGroup5"
obt$agegroup[obt$Age  >99 & obt$Age <= 119] <- "AgeGroup6"

obt$agegroup <- as.factor(obt$agegroup)

str(obt)


### Dropping empty levels

obt <- droplevels.data.frame(obt)
str(obt)

library(plyr)

obt$Color <- mapvalues(obt$Color, from = c("Yes","No"), to = c("yes","no"))

obt$Married <- mapvalues(obt$Married, from = c("Yes","No"), to = c("yes","no"))

obt$Burial_Week <- mapvalues(obt$Burial_Week, from = c("Weeekday","weekday","Weekday","Weekeday","weekend","Weekend"), to = c("weekday","weekday","weekday","weekday","weekend","weekend"))

obt$Residence <- mapvalues(obt$Residence, from = c("Yes","No"), to = c("yes","no"))

obt$Cause_of_Death <- mapvalues(obt$Cause_of_Death, from = c("accident","Accident","illness","Illness","ilness"), to = c("accident","accident","illness","illness","illness"))


## maintaining numerics
obt$Word_Count <- as.numeric(obt$Word_Count)
obt$No_of_Relatives <- as.numeric(obt$No_of_Relatives)
obt$Distance_Death <- as.numeric(obt$Distance_Death)
obt$Distance_Morgue <- as.numeric(obt$Distance_Morgue)

#### Dropping unnecessary variables for analysis

var.drop <- c("Name","Announcement","Death","Burial","Death_to_Announce","Death_to_Burial","Announce_to_Burial","Repetition","an_date","dt_date","br_date","agegroup")

obt3 <- obt

obt3[,var.drop] <- NULL

# Checking for missingness
colSums(is.na(obt3))

obt_mis <- obt3

dim(obt3)


# Return the column names containing missing observations
list_na <- colnames(obt_mis)[ apply(obt_mis, 2, anyNA) ]
list_na

## maintaining numerics

obt_mis$Age <- as.numeric(obt_mis$Age)
obt_mis$No_of_Children <- as.numeric(obt_mis$No_of_Children)
obt_mis$Significant_Children <- as.numeric(obt_mis$Significant_Children)
obt_mis$Significant_Relatives <- as.numeric(obt_mis$Significant_Relatives)
obt_mis$death2Announce <- as.numeric(obt_mis$death2Announce)




obt_mis_impute_mean <-  data.frame(sapply(obt_mis,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))


df <- obt_mis_impute_mean[!(is.na(obt_mis_impute_mean$Fundraising) | obt_mis_impute_mean$Fundraising==""|obt_mis_impute_mean$Fundraising==1), ]

df$Fundraising <- mapvalues(df$Fundraising, from = c("2","3"), to = c("no","yes"))


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



# mars
set.seed(121)

model_mars = train(Fundraising ~., data = train_obt_complete4,method = 'earth')
fitted <- predict(model_mars)

model_mars

plot(model_mars, main="Model Accuracies with MARS")


varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")





# random forest
model_rf= train(Fundraising ~.,data = train_obt_complete4, method = 'rf')

model_rf



#### Preparing test data

testData2 <- predict(dummies_model, test_df)

# Transform the features to range between 0 and 1
testData3 <- predict(preProcess_range_model, testData2)


# Predict on testData
predicted <- predict(model_rf, testData3)
head(predicted)


predicted <- data.frame(predicted)

table(test_df$Fundraising)
table(predicted)

# Compute the confusion matrix


confusionMatrix(table(predicted, test_df$Fundraising)) 


