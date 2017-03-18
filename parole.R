############################ Parole Data Problem  ###########################

# The Parole dataset has the following fields
# male - describes if the parolee is a mail or female. 0-female,1-male
# race - describes if the parolee is a while or nonwhite. 1-White, 2-Non white
# age  - Age of the parolee in years
# state - State code of the parolee. 1-Other, 2-Kentucky,3-Lousiana,4-Virginia
# time.served - Months served in prision (limited by the inclusion criteria to not exceed 6 months)
# max.sentence - Maximum sentence in months (limited by the inclusion criteria to not exceed 18 months)
# multiple offences - Describes if the parolee has been jailed for multiple offences. 1- yes, 2-no
# crime - A code for the parolee's main crime. 1-Other,2-Larceny,3-drug,4-driving
# violator- Is the parolee a violator. 1 - parole violated, 0 - parole completed without violation


# Set the location of the dataset on local system
setwd('/Users/Ameet/Box Sync/Ameet/GitHub/R-Projects/parole')

# packages required
# install.packages('caTools')
# install.packages('rpart')
# install.packages('rattle')


# Read the file in memory as a dataframe to conduct analysis.
parole= read.csv("parole.csv")

# Data Preparation Phase
# In this phase you review the data structure, data format of the source data and make the necessary changes to
# conduct the data analysis. At lot of the data is provided as integer values. Since these data are of categorial
# type, it makes more sense to covert it to a more readable format. E.g. male variable - male, female value
# is more readable compared to 0 and 1.

# Review the data variables
names(parole)


# Review the range, mean and median for each variable of the dataset. This should give you some initial insight about the 
# variables in the dataset. E.g. The median age of the parolee is 33.70 and average is 34.51  
summary(parole)

# How many are parole violators in the sample data. Out of the 675 parolee 78 violated their parole
table(parole$violator)  # how many are violators

# converting non-metric variables to factor, 
# levels argument ensures the order of the categories
# if the categories are already in the right order, the levels arg can be dropped

# Convert male variable from 0,1 to women and men
# Convert race variable from 1,2 to White and 'Not White'
# Convert state variable from 1,2,3,4 to Other, Kentuchy, Lousiana and Virginia
# Convert crime variable from 1,2,3,3 to Other, Larceny, drug and driving
# Convert multiple.offences variable from 0,1 to no and yes
parole$male = factor(parole$male,
                     levels = c(0,1),
                     labels = c("women", "men"))
parole$race = factor(parole$race,
                     levels=c(1,2),
                     labels=c("White","Not White"))
parole$state = factor(parole$state, 
                      levels = c(1,2,3,4),
                      labels = c("Other","Kentucky","Lousiana","Virginia"))
parole$crime = factor(parole$crime,
                      levels = c(1,2,3,4),
                      labels=c("Other","Larceny","drug","driving"))
parole$multiple.offenses = factor(parole$multiple.offenses, 
                                  levels = c(0,1),
                                  labels = c("no", "yes"))

# To build a model and test it, we first split the data given to use into train and test data sets
# The model will be built on the train data and tested on test data.This will allow us to assess the accuracy
# of our model. The split ratio is train - 70% , test - 30%.

library(caTools)
set.seed(199)
split = sample.split(parole$violator,SplitRatio=0.7)
train = subset(parole,split==TRUE)
test = subset(parole,split==FALSE)

# We now have 473 records to train the model and 202 records to test it on.
nrow(train)
nrow(test)


## Exploratory data analysis
# This completes the data preparation phase. 
# In the Explore data phase we will explore the data vizually to get a better understanding of it. 



# Sex , Men have a higher violation rate compared to women
library(ggplot2)
tapply(train$violator,train$male,mean)
ggplot(data=train,aes(x=male,y=violator,fill=male))+
  geom_bar(stat='summary',fun.y='mean')


# Race , Non white race have a higher violation rate compared to white
tapply(train$violator,train$race,mean)
ggplot(data=train,aes(x=race,y=violator,fill=race))+
  geom_bar(stat='summary',fun.y='mean')

# Average Age of violator parolee is slightly lesser than non-violator parolee 
tapply(train$age,train$violator,mean)
ggplot(data=train,aes(x=factor(violator),y=age,fill=factor(violator)))+
  geom_bar(stat='summary',fun.y='mean')+
  xlab('Violator') + scale_x_discrete(labels=c("0" = "Non-violator", "1" = "Violator"))+
  scale_fill_discrete(name="Violators", breaks=c('0', '1'),labels=c('Non-Violator', 'Violator'))

##### This chart show the age frequency of parolee.Most parolee's are in their early twenty's
ggplot(data=train,aes(x=age,color=factor(multiple.offenses)))+
geom_freqpoly(size=2) + ylab('Count of parolee') 


##### State , Lousiana has the highest violators
tapply(train$violator,train$state,mean)
ggplot(data=train,aes(x=state,y=violator,fill=state))+
  geom_bar(stat='summary',fun.y='mean')

# Time.served , Violators have served lesser time in prision than non violators
tapply(train$time.served,train$violator,mean)
ggplot(data=train,aes(x=factor(violator),y=time.served,fill=factor(violator)))+geom_bar(stat='summary',fun.y='mean')+ 
  xlab('Violator') + scale_x_discrete(labels=c("0" = "Non-violator", "1" = "Violator"))+
  scale_fill_discrete(name="Violators",breaks=c('0', '1'),labels=c('Non-Violator', 'Violator'))



## Data Modeling Phase
# Create a Logistic Regression Model with violator as the target variable and all
# other variables as the predictor variables 
model1 = glm(violator~.,train,family="binomial")

# what is the interpretation of coefficient of multiple.offenses?
# multiple.offenses has p value <.01 showing strong co-relation.
# The AIC value is the Measure of relative quality of model. The value is 254.59
# We will compare this with the other model.Lower is better
summary(model1)

# a parolee who committed multiple offenses is 5 times more likely to violate parole
exp(model1$coefficient['multiple.offensesyes']) 


# Consider a parolee who is a white male, aged 50 years at prison release, 
# from the state of Maryland, served 3 months, 
# had a maximum sentence of 12 months, did not commit multiple offenses, 
# and committed a larceny. What are the odds of this individual being a violator? 
# What is the probability of this person being a violator?

varCoefficients = c(model1$coefficient[1:13]) # list of all coefficients from model
varScores = c(1,1,1,50,0,0,0,3,12,0,1,0,0)    # list of all var based on above description
logOdds = sum(varCoefficients*varScores)      # log odds is equal to sum of product of coeffs and var scores

# odds of being a violator
oddsOfBeingAViolator = exp(logOdds); oddsOfBeingAViolator 

# probability of being a violator. There is a 14.5% chance that this person will be a repeat violator
probabilityViolator = 1/(1+exp(-logOdds));probabilityViolator 

# Now that we created the model and tested on the training data set, let us run it against
# the Test dataset and see how well it performs.
# Applying predictions to test sample
predictTest = predict(model1,test,type="response")
# what is the max predicted probability
#summary(predictTest)
max(predictTest)

#ggplot(data=data.frame(predictTest),aes(x=predictTest))+
#geom_histogram(fill='steelblue3')

# Construct a Classification table or Confusion matrix using 0.5 threshold (cutoff)
#   
x = table(test$violator,predictTest>0.50); x

# Accuracy of the model. Higher the value, more accurate the model
accuracy = sum(x[1,1],x[2,2])/nrow(test); accuracy

# Specificity and Sentivity of the model
# Specificity -> True Negative/ ( True Negative+ False Positive)
# Sentivity   -> True Positive/ (False negative _ True Positive)
specificity = x[1,1]/sum(x[1,1],x[1,2]); specificity
sensitivity = x[2,2]/sum(x[2,1],x[2,2]); sensitivity
baseline = (167+12)/(167+12+11+12); baseline

## Checking for different cutoff values 
## specificity is important as False Positives are costly errors for this data
## for high specificity, threshold should be high
x = table(test$violator,predictTest>0.70);x
accuracy = sum(x[1,1],x[2,2])/nrow(test); accuracy
specificity = x[1,1]/sum(x[1,1],x[1,2]); specificity
sensitivity = x[2,2]/sum(x[2,1],x[2,2]); sensitivity

## ROC curves allow us to visualize the impact of different thresholds
## on Specificity and Sensitivity
## AUC is model performance measure that is independent of any particular threshold
## High threshold => Higher specificity and Lower sensitivity   
## Low threshold => Lower specificity and higher sensitivity
library(ROCR)
ROCRpred1 = prediction(predictTest,test$violator)
as.numeric(performance(ROCRpred1,"auc")@y.values) # auc measure
## construct plot
ROCRperf1 = performance(ROCRpred1,"tpr","fpr")
#plot(ROCRperf) # basic plot
#plot(ROCRperf,xlab="1 - Specificity",ylab="Sensitivity") # relabled axes
#plot(ROCRperf,colorize=TRUE) # color coded ROC curve
plot(ROCRperf1,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="False Positive Rate (1 - Specificity) ",ylab="True Positive Rate (Sensitivity)") # color coded and annotated ROC curve


###########################################
# How would this compare to a tree model?
# Would a tree model generate a higher AUC on the test data?
# Would a tree model be better than Logistics regression model?

library(rpart)
library(rpart.plot)
library(rattle)
tree = rpart(violator~.,data=train,method="class",control=rpart.control(minbucket=2))
prp(tree)
fancyRpartPlot(tree)
predTree = predict(tree,newdata=test,type="class")

##### Confusion matrix, Accuracy, Specificity and Sensitivity of the tree Model
x = table(test$violator,predTree);x
accuracy = sum(x[1,1],x[2,2])/nrow(test); accuracy
specificity = x[1,1]/sum(x[1,1],x[1,2]); specificity
sensitivity = x[2,2]/sum(x[2,1],x[2,2]); sensitivity


predTreeProb = predict(tree,newdata=test,type="prob")
ROCRpred = prediction(predTreeProb[,2],test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure
## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="False Positive Rate (1 - Specificity) ",ylab="True Positive Rate (Sensitivity)") # color coded and annotated ROC curve


## Conclusion
#  The Logistic regression model seems to perform better than the tree model in this scenario
#  It has higher AUC (Area under curve) value
