## Loading the datasets for this projects
library(plyr)
library(dplyr)
library(data.table)
library(Hmisc)
library(ggplot2)
library(gridExtra)

###  Loading the datasets
train<-read.csv("E:/train_gbW7HTd.csv",header = T,na.strings = "")
test<-read.csv("E:/test_2AFBew7.csv",header = TRUE,na.strings = "")

### Looking at the first ten obsn of the train & test datasets
head(train,n = 10)
head(test,n = 10)

### Univariate Analysis
## Using this part to explore every variables in the datasets, and this techiques

#depends on whether the variable types is categorical or continous variables

#### Looking at the structures of each variables 
str(train)
## The conclusion shows that we have 3 int(continous v) 
#& 9 factor(categorical variables) 
# we also identify the dependent(Income.Group) & independent variables 

## Univariate Analysis....
### choosing columns for continuous variables
cont_train<-train %>% select(ID,Age,Hours.Per.Week)
summary(cont_train)

## choosing columns for categorical variables

cat_train<- train%>%select(-ID,Age,Hours.Per.Week)
describe(cat_train)
summary(cat_train)

## In the case of categorical variables, will use the frequency table to understand
## each categorical, it can be measured by using two metrics, Count and Count%
## for each category
## checking the count of categories, let check the number of unique values for each
## variables
apply(cat_train,2,function(x){length(unique(x))})

##print out count for Race
table(cat_train$Race)
## print out the percentages of obsns in each category
## we can see that the category white accounts for ~ 85% 
as.matrix((prop.table(table(cat_train$Race))))

#print out count for first 20 of Native- Country 
head(sort(table(cat_train$Native.Country),decreasing = TRUE),20)

# print out the percentage of observations of  top 20 countries
# United states count for 90% of the observations,the closest was mexico with 
#only 1% of the population
head(round(sort(prop.table(table(cat_train$Native.Country)),decreasing = TRUE),6),20)

## Multivariate Analysis
## using the gmodels packages in R, we look at the cross-tabulation or confusion matrix
#of the two variables
library(gmodels)
## take a look at the Sex  and Income.Group
#using CrossTable functions in gmodels
#Out of total Females, 89.1% females have 
#income <= 50K and only ~ 11% females have income >50K
CrossTable(cat_train$Sex,cat_train$Income.Group)

##Out of total people which have income >50K, 
#only 15% are females and 85% are males
#out of total males , 69% have income <=50k and 31% has income >50k
#out of total people which have income >50k
#85% are males while 15% where females
#out of total people which have income <=50k,
#only 38% are females and 62% are males

### Looking at the density plot for the dependents variables
ggplot(data = train, aes(Income.Group)) + geom_density(aes(fill = Income.Group)) +
        theme(axis.text.x = element_blank()) +
        labs(list(title = "Income by Group", x = "Status", y = "Density"))

## we can plot the stacked bar chart (categorical vs categorical)
# we can see that majority of the females have income <=50k
ggplot(cat_train,aes(Sex, fill=Income.Group))+geom_bar()+labs(title= "Stacked Bar chart",x="Sex",y="Count")+theme_bw()

## we can use a boxplot for (categorical vs continous)
#we can a each boxplot for each category
# creating a box plot
#ggplot(train,aes(Sex,Hours.Per.Week)) +geom_boxplot()+labs(title= "Boxplot")

## The median of the working hours of both male and female are thesame
#for males,the first quarter and the median values are same
#for females, the median and the third quarter are thesame..
box_status <-ggplot(train,aes(Sex,Hours.Per.Week ))
box_status+geom_boxplot(aes(fill= Sex)) +
        theme(axis.text.x= element_blank())+
        labs(list(
                title ="boxplot by sex",
                x= "Sex",
                y="Hours per week"))

#### Checking missing values ################
##check the number of missing values in the complete data set
table(is.na(train))
table(is.na(test))
# check missing values column wise in train data
colSums(is.na(train))
# check missing values in the test data set
colSums(is.na(test))

### in both train & test data we found  missing values in 3 variables
#workclass, occupation and Native.country (categoricals)

### Missing values treatment -- using the mlr packages system
library(mlr)
#impute missing values with mode
imputed_data<- impute(train,classes = list(factor=imputeMode()))
#update train data set with imputed values
train<-imputed_data$data
#Now check the missing values in train again to comfirm
colSums(is.na(train))
# we can do thesame for the test
colSums(is.na(test))
## To check percentages of missing colums & row ######
#Pmiss<-function(x){sum(is.na(x))/length(x)*100}
#apply(train,2,Pmiss) # columns
#apply(train,1,Pmiss) #rows

#check the class of all the variables
sapply(train, class)
# Workclass Example
## determine  the percentage of observation in each category
as.matrix(prop.table(table(train$Workclass)))
#using recode function from packages in r
library(car)
##combining factor levels with a few observations in a new level named others
train$Workclass<- recode(train$Workclass,"c('State-gov','Self-emp-inc','Federal-gov','Without-pay',
                         'Never-worked ')='Others'")
test$Workclass<-recode(test$Workclass,"c('State-gov','Self-emp-inc','Federal-gov','Without-pay',
                       'Never-worked')= 'Others'")

#let's check the factor level percentages
as.matrix(prop.table(table(train$Workclass)))

## Here we can see that the categories have been successfully combined
## basic usage of cut with a numeric varables
# create a bins of size 5 from the Hours.Per.week and the bins that has the highest
# respondents is bin (36,41)
c1 <- cut(train$Hours.Per.Week, breaks = seq(1,99, by = 5))
table(c1)

## Predictive modelling
#Data pre-processing
# we can encode the the dependents variable into two levels 0 & 1, so that the
#algorithms will clearly classify the levels.in this encoding better.
#"<=50k" - This will be converted  to 0
#">50k" - This will be converted to 1

# need to consider  the dependent variable in both train and test datasets
table(train$Income.Group)

# we encode the dependents variables
train$Income.Group <- ifelse(train$Income.Group == "<=50K",0,1)
table(train$Income.Group)
## removing the ID from the datasets
train<- train%>%select(-ID)

### Building models ##########
## building decision tree we use the rpart() packages 
library(rpart)
#Build a model
set.seed(333)
train.tree<- rpart(Income.Group ~ .,data = train, method = "class",control = rpart.control
                   (minisplit= 20,minbucket = 100, maxdepth = 10),xval=5)

# To look at the summary of the these parameters:
#1 minisplit - refers to minimum of nos of obsn which exist in a node to split
#2 minibucket - refers to minimum number of observations which  exist in terminal node left
#3 maxdepth - refers to depth of the tree
#4 xval - refers to cross validation

#let's see the summary to check  variables importance & other importants terms
#summary  function shows  variable importance , cp table and the decision tree
summary(train.tree)
# for further understanding, let's plot this tree and visualize  the tree 
#install rpart.plot
library(rpart.plot)
# draw the tree
rpart.plot(train.tree)
# we can see that relation is the most important variables
#the first node: if the relationship is not in family','child','unmarried','other relatives', the tree
#predicts their  salary <=50k, else  if the relatonship is different,the tree
#moves  to node 2
#we get 6 terminal nodes (leafs)
#similarly  we do understand the splitting to other nodes

## Make predictions
#Now we can make predictions
#prediction for the train
predict_train <-predict(train.tree, newdata = train,type = "class")
# make prediction for the test data
predict_test <- predict(train.tree, newdata = test,type = "class")

##Analyzing the results
# we know we can use various metrics  to evalute the  a model depending  on the problem
#at hand but i will use prediction accuracy  here because that is all i have been doing 
# because we are using a classification problem, we will use confusion matrix
# to know the fractions of how accurate we are or hdid for the prediction
#load caret packages
library(caret)
# for the train accuracy 
confusionMatrix(predict_train, train$Income.Group)
# from the prediction we got 82% accuracy 
## Create a the data frame  of the final prediction
solun_frame<-data.frame(ID = test$ID, Income.Group = predict_test)
# writing  the solution file
write.csv(solun_frame, file = "final_solution.csv")


