library(Information)
library(ggplot2)
library(plyr)
library(MASS)
library(caret)
library(caTools)
library(car)
library(caret)
library(e1071)
library(ROCR)
#Set Working Directory
setwd("H:/Upgrad_DA/New -ML/Kaggle")

# Load the dataset
train_data_titanic <- read.csv('H:/Upgrad_DA/New -ML/Kaggle/train.csv',header = T,
                         stringsAsFactors = T,na.strings=c(""))

test_data_titanic <- read.csv('H:/Upgrad_DA/New -ML/Kaggle/test.csv',header = T,
                              stringsAsFactors = T,na.strings=c(""))


#--------------------------------------------------------------------------------------------------------------
#Data Exploration
#1. Check total number of missing values
sum(is.na(train_data_titanic))
#177

#How Many Missing values in Each column/variable
lapply(train_data_titanic,function(x) sum(is.na(x)))
#train_data_titanic <- train_data_titanic[,-c(1,4,11)]
#Age has 177 missing value
#Percentage of missing values 
(177/891)*100
#19%

#Checking for duplication
titanic_no_dups <- train_data_titanic[!duplicated(train_data_titanic),]
train_data_titanic <- titanic_no_dups


#----------------------------------------------------------------------------------------------------------
#                                           Exploratory data analysis
#----------------------------------------------------------------------------------------------------------
#Finding the Survival Rate:
survival_rate <- count(train_data_titanic$Survived==1)[2,2]/(count(train_data_titanic$Survived==1)[2,2] +
                                                         count(train_data_titanic$Survived==0)[2,2])
survival_rate                                                       
# 0.3838384
#----------------------------------------------------------------------------------------------------------
plot_survival_rate <- function(cat_var, var_name)
{
  a <- aggregate(Survived ~ cat_var, train_data_titanic , mean)
  count <- data.frame(table(cat_var))
  print(count)
  count <- count[,-1]
  agg_sur_rate <- cbind(a, count)
  colnames(agg_sur_rate) <- c(var_name, "Survival_Rate"," Survival_Rate_count")
  agg_sur_rate[, 2] <- format(round(agg_sur_rate[, 2], 2))
  #print(agg_sur_rate)
  
    ggplot(agg_sur_rate, aes(agg_sur_rate[, 1], count, label = round(survival_rate,2))) +
                          geom_bar(stat = 'identity',color='black',fill='blue') +
                            theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                              geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}
#============================================================================================================
#Data Preparation & EDA
#============================================================================================================
#---------------------------------------1. Passenger Id & Cabin & Fare------------------------------------------------
#This column can be dropped as it is not useful
train_data_titanic <- train_data_titanic[,-c(1,11)]
#-----------------------------------------------------------------------------------------------------------
#---------------2. P-clas:Description pclass	- Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd-------------------
str(train_data_titanic$Pclass)
unique(train_data_titanic$Pclass)

# Converting it into factor variable
train_data_titanic$Pclass <- as.factor(train_data_titanic$Pclass)
str(train_data_titanic$Pclass)
#Factor with 3 levels

#Plotting the survival rate for this 
plot_survival_rate(train_data_titanic$Pclass , "Socio-economic status of the people on the ship")
#Passengers Travelling in class 3 were mostly survived : 0.38*490 = 186 people 
#-----------------------------------------------------------------------------------------------------------
#-------------------3. Name---------------------------------------------------------------------------------
# For now dropping name column as it doesn't seem to have any significance----------------------------------
#=============For Train Data============================================================================
str(train_data_titanic$Name)
train_data_titanic$Name <- as.character(train_data_titanic$Name)

train_data_titanic$Title <- sapply(train_data_titanic$Name, 
                                   function(x) {strsplit(x, split='[,.]')[[1]][2]})
train_data_titanic$Title <- trimws(train_data_titanic$Title)

table(train_data_titanic$Title)

train_data_titanic$Title[train_data_titanic$Title %in% c('Capt','Col','Don', 'Major','Sir')] <- 'Sir'

train_data_titanic$Title[train_data_titanic$Title %in% c('Dona' ,'Lady','Jonkheer','the Countess', 
                                                         'Rev')] <- 'Lady'

train_data_titanic$Title[train_data_titanic$Title %in% c('Miss','Ms','Mlle', 'Mme')] <- 'Miss'
#---------------------------Surname------------------------------------------------------------------
train_data_titanic$Surname <- sapply(train_data_titanic$Name, 
                                   function(x) {strsplit(x, split='[,.]')[[1]][1]})

train_data_titanic$Surname <- trimws(train_data_titanic$Surname)
table(train_data_titanic$Surname)


#=======================================================================================================
#-----------For Test Data-------------------------------------------------------------------------------
str(test_data_titanic$Name)
test_data_titanic$Name <- as.character(test_data_titanic$Name)

test_data_titanic$Title <- sapply(test_data_titanic$Name, 
                                   function(x) {strsplit(x, split='[,.]')[[1]][2]})
test_data_titanic$Title <- trimws(test_data_titanic$Title)

table(test_data_titanic$Title)

test_data_titanic$Title[test_data_titanic$Title %in% c('Capt','Col','Don', 'Major','Sir')] <- 'Sir'

test_data_titanic$Title[test_data_titanic$Title %in% c( 'Dona', 'Lady','Jonkheer','the Countess', 
                                                         'Rev')] <- 'Lady'

test_data_titanic$Title[test_data_titanic$Title %in% c('Miss','Ms','Mlle', 'Mme')] <- 'Miss'
table(test_data_titanic$Title)

test_data_titanic$Surname <- sapply(test_data_titanic$Name, 
                                     function(x) {strsplit(x, split='[,.]')[[1]][1]})

test_data_titanic$Surname <- trimws(test_data_titanic$Surname)

#-----------------------------------------------------------------------------------------------------------
#--------------------4. Gender------------------------------------------------------------------------------
#Any missing value in this?
sum(is.na(train_data_titanic$Sex))
#0
#Let's see the survival rate based on gender
plot_survival_rate(train_data_titanic$Sex , "Gender of the people on the ship")

# Out of approx 570 males 38% survived  i.e 216, this tells us 216 males survived and males ratio is more than
#females on the ship
#-----------------------------------------------------------------------------------------------------------
#---------We will try to perform feature transformation on Parch.Sibsp
#          and convert it to family based on ticket id 
#----------------------------------------------------------------------------------------------------------

for(i in 1:nrow(train_data_titanic)){

  train_data_titanic$FamilySize[i] <- (train_data_titanic$SibSp[i] + train_data_titanic$Parch[i] + 1)

}

for(i in 1:nrow(test_data_titanic)){
  
  test_data_titanic$FamilySize[i] <- (test_data_titanic$SibSp[i] + test_data_titanic$Parch[i] + 1)
  
}
#=====================================================================================================
#==============================Creating a new variable=================================================
#This variable will uniquely identify every person
train_data_titanic$Surname_Fsize <- paste(train_data_titanic$Title , train_data_titanic$Surname , 
                             train_data_titanic$FamilySize, sep = '')
test_data_titanic$Surname_Fsize <- paste(test_data_titanic$Title , test_data_titanic$Surname , 
                                         test_data_titanic$FamilySize, sep = '')
#=======================================================================================================
#After Feature Transformation we can subset the transformed variables into
#new dataset and can use it for our modelling purpose
titanic_new <- train_data_titanic[,-c(3,13,12,11,6,7,8)]
#=======================================================================================================
#--------------------Age-----------------------------------------------------------------------------

#--------------Age without na values-----------------------------------------------------------------
age_titanic_wo_na <- subset(titanic_new , !is.na(titanic_new$Age))
str(age_titanic_wo_na)
#-----------plotting normal curve------------------------------------------------

plot(density(age_titanic_wo_na$Age))
#almost a normal curve
#-------------------------Missing Value treatment------------------------------------------------------------
# Replace with Mean
mean(age_titanic_wo_na$Age)
#29.69912

titanic_new[which(is.na(titanic_new$Age)),]$Age <- mean(age_titanic_wo_na$Age)
#------Plotting Age variable after imputing with mean---------------------------------------------------
plot(density(titanic_new$Age))

ggplot(titanic_new ,  aes(Age)) + geom_density(stat = 'density', position = 'identity')
#------------------------------------------------------------------------------------------------------
#------Outlier Treatment for Age----------------------------------------------------------------------
boxplot(titanic_new$Age)
boxplot.stats(titanic_new$Age)
sort(boxplot.stats(titanic_new$Age)$out)
quantile(titanic_new$Age , seq(0,1,0.01))
#-----------Applying Lower and Upper cap--------------------------------------------------------------
titanic_new[which(titanic_new$Age <= 2),]$Age <- 3.00
titanic_new[which(titanic_new$Age >= 55),]$Age <- 54.00

sort(boxplot.stats(titanic_new$Age)$out)
#------------Verifying the distribution of age--------------------------------------------------------
ggplot(titanic_new ,  aes(Age)) +geom_histogram(bins = 2 ,binwidth = 3, aes(y = ..density..),
                                                       color="black" , fill='wheat') +
                                        geom_density(color = 'black')

#------------------------------------------------------------------------------------------------------
#--------Binning Age variable--------------------------------------------------------------------------
titanic_new$binning.Age <- as.factor((cut(titanic_new$Age,
                                               breaks = c(0,16,25,40,54))))

#======================================================================================================
#------------------------Fare-------------------------------------------------------------------------
sum(is.na(titanic_new$Fare))
#-No Missing Value-------------------------------------------------------------------------------------
#-------Checking Outlier------------------------------------
boxplot(titanic_new$Fare)
sort(boxplot.stats(titanic_new$Fare)$out)
quantile(titanic_new$Fare , seq(0, 1, 0.01))
plot(density(titanic_new$Fare))

titanic_new$Fare[which(titanic_new$Fare >= 66)] <- 65.48000
#titanic_new$Fare[which(titanic_new$Fare >=83.1583 & titanic_new$Fare <= 89.1042)] <- 112.07915
titanic_new$Fare[which(titanic_new$Fare <= 0)] <- 1
#--------Binning Fare variable--------------------------------------------------------------------------
ggplot(titanic_new ,  aes(binning.Fare)) +geom_bar()

titanic_new$binning.Fare<- as.factor((cut(titanic_new$Fare,
                                          breaks = c(0,15,25,35,45,55,66))))
#======================================================================================================
#------------------Embarked-----------------------------------------------------------------------------------
sum(is.na(titanic_new$Embarked))
#2 Variables, hence removing these 2 variables
titanic_new <- subset(titanic_new , !is.na(titanic_new$Embarked))
#==================Multivariate Analysis==============================================================

#=====================================================================================================
#Model Creation
#Dummy creation
library(dummies)
titanic_mod <- titanic_new[,-c(4,5)]
str(titanic_mod)
titanic_mod <- dummy.data.frame(titanic_mod[,-5])
titanic_mod$Survived <- as.factor(ifelse(titanic_mod$Survived == 1, "yes", "no"))
#titanic_mod$Surname_Fsize <- titanic_new$Surname_Fsize
#-----------------------------------------------------------------------
set.seed(100)
split_indices <- sample.split(titanic_mod$Survived, SplitRatio = 0.70)

train <- titanic_mod[split_indices,]
test <- titanic_mod[!split_indices, ]
#--------------------------------
logmodel_t <- glm(Survived ~ . , data= train ,family = "binomial")
summary(logmodel_t)

l.tit <- stepAIC(logmodel_t, data = train)
summary(l.tit)
vif(l.tit)
#========================================================================================================
mod_1 <- glm(formula = Survived ~ Pclass1 + Pclass2 + Sexfemale + 
        `binning.Age(0,16]`  + `binning.Age(25,40]` + 
        `binning.Fare(25,35]`, family = "binomial", data = train)

summary(mod_1)
#========================================================================================================
mod_2 <- glm(formula = Survived ~ Pclass1 + Pclass2 + Sexfemale + 
               `binning.Age(0,16]`  + `binning.Age(25,40]` ,
                family = "binomial", data = train)
summary(mod_2)
#========================================================================================================
mod_3 <- glm(formula = Survived ~ Pclass1 + Pclass2 + Sexfemale + 
               `binning.Age(0,16]` ,
             family = "binomial", data = train)
summary(mod_3)
#=========================================================================================================
#========================================================================================================
mod_4 <- glm(formula = Survived ~ Pclass1 + Pclass2 + Sexfemale ,
             family = "binomial", data = train)
summary(mod_4)
#=========================================================================================================
 logistic_final <- mod_3

#==========================================================================================================
predictions_logit <- predict(logistic_final, newdata = test[,-1],
                             type = "response")
summary(predictions_logit)
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$Survived, positive = 'yes')
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)

  }

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#Plotting cutoffs 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Let's choose a cutoff value of 9% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.09, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$Survived, positive = "yes")

acc <- conf_final$overall[1]
acc

sens <- conf_final$byClass[1]
sens

spec <- conf_final$byClass[2]
spec

#ROC curve

test$predicted_prob = predict(logistic_final, newdata = test,type = "response")

model_score_test <- prediction(test$predicted_prob,test$Survived)
model_perf_test <- performance(model_score_test, "tpr", "fpr")

plot(model_perf_test,col = "red", lab = c(10,10,10))

#AUC
model_auc<- performance(model_score_test,"auc")

model_auc@y.values
#0.8571301
cutoff

#============================================================================================================
#Unseen Data 
#============================================================================================================
test_data_titanic$binning.Age <- as.factor((cut(test_data_titanic$Age,
                                                breaks = c(0,16,25,40,54,100))))
test_data_titanic$binning.Fare<- as.factor((cut(test_data_titanic$Fare,
                                          breaks = c(0,15,25,35,45,55,66))))

str(test_data_titanic)
x_test_data_titanic <- test_data_titanic[,c(2,4,11,15,16,17)]
x_test_data_titanic$Pclass <- as.factor(x_test_data_titanic$Pclass)
str(x_test_data_titanic)

dummy_titanic_test <- dummy.data.frame(x_test_data_titanic[,-4])


predictions_logit_x<- predict(logistic_final, newdata = dummy_titanic_test,
                             type = "response")
prediction <- predict(logistic_final, dummy_titanic_test, type = "response")

solution <- data.frame(PassengerID = test_data_titanic$PassengerId, Survived = prediction)

solution$Survived <- factor(ifelse(solution$Survived > 0.3, 1, 0))

#-----------Writing the file-----------------------------------------------------------------------------
write.csv(solution,"H:/Upgrad_DA/New -ML/Kaggle/New folder/gender_submission.csv" ,row.names = F)
#----------------------------------------------------------------------------------------------------------