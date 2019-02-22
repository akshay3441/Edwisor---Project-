rm (list=ls())
getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)

library(rpart)
library(C50)
library(ggplot2)

#Now we will load train and test data as per our project
data_train = read.csv("Train_data.csv" , header = T)
data_test = read.csv("Test_data.csv", header = T)

##Now we will do data preprocessing and first of all we will do missing value analysis..
#To find out how many missing value each train and test data contains we use below command..

sum(is.na(data_train))
sum(is.na(data_test))

# So there are no missing values in the data set of each test and train data..

str(data_train)
str(data_test)

# we will do data manupulation as R works on RAM and it will utilise each and every word..
#so in order to save the space we will convert categorical variable to some numeric form and will assign some levels and for this we will create a loop..
#for train and test data..
for(i in 1:ncol(data_train)){
  
  if(class(data_train[,i]) == 'factor'){
    
    data_train[,i] = factor(data_train[,i], labels=(1:length(levels(factor(data_train[,i])))))
    
  }
}

for(i in 1:ncol(data_test)){
  
  if(class(data_test[,i]) == 'factor'){
    
    data_test[,i] = factor(data_test[,i], labels=(1:length(levels(factor(data_test[,i])))))
    
  }
}

##Now we will do outlier analysis..
#with the help of box plot method and it only use for numerical variable..
#outlier analysis is only applied for numerical variables..


numeric_index = sapply(data_train,is.numeric) #selecting only numeric

numeric_index2 = sapply(data_test,is.numeric) #selecting only numeric

## Now we will separate numeric variable and categorical variable for both train and test data to detect outlier analysis..

numeric_data = data_train[,numeric_index]
numeric_data2 = data_test[,numeric_index2]

# Now we will select the columns for numeric variable data for each train and test..
cnames = colnames(numeric_data)
cnames2 = colnames(numeric_data2)

#Now we will draw the box plot for each numeric variable for train and test data respectively..

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(data_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}


for (i in 1:length(cnames2))
{
  assign(paste0("gk",i), ggplot(aes_string(y = (cnames2[i]), x = "Churn"), data = subset(data_test))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of responded for",cnames2[i])))
}


## Plotting plots together for train data..
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)


## Plotting plots together for test data..
gridExtra::grid.arrange(gk1,gk2,gk3,gk4,gk5,gk6,gk7,gk8,gk9,gk10,gk11,gk12,gk13,gk14,gk15,gk16,ncol=16)



# #loop to remove from all variables and remove the outliers from train and test data respectively..
for(i in cnames){
  print(i)
  val = data_train[,i][data_train[,i] %in% boxplot.stats(data_train[,i])$out]
  #print(length(val))
  data_train = data_train[which(!data_train[,i] %in% val),]
}


for(i in cnames2){
  print(i)
  val = data_test[,i][data_test[,i] %in% boxplot.stats(data_test[,i])$out]
  #print(length(val))
  data_test = data_test[which(!data_test[,i] %in% val),]
}

##crosscheck train and test data..

data_train

## Now our next preprocessing technique is feature selection technique..
# now for numerical variable we will use correlation as a feature selection technique
# and for categorical variable we will use chi square test as a feature selection technique for both train and test data..

library(corrgram)
corrgram(data_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot train data")

corrgram(data_test[,numeric_index2], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot test data")


## Now we will use feature seletion for categorical variable for both test and train data..
## Chi-squared Test of Independence
factor_index_train = sapply(data_train,is.factor)
factor_data_train = data_train[,factor_index_train]
factor_index_train


factor_index_test = sapply(data_test,is.factor)
factor_data_test = data_test[,factor_index_test]

names(factor_data_train)

#Test
for (i in 1:4)
{
  print(names(factor_data_test)[i])
  print(chisq.test(table(factor_data_test$Churn,factor_data_test[,i])))
}


#Train

for (i in 1:4)
{
  print(names(factor_data_train)[i])
  print(chisq.test(table(factor_data_train$Churn,factor_data_train[,i])))
}


## Dimension Reduction
data_train = subset(data_train, 
                    select = -(phone.number))

data_test = subset(data_test, 
                   select = -(phone.number))

## So after feature selection technique we extracted the train and test data having 20 variables..

## Now we will do some normalisation to convert all the variables in same range so that our test and train model should not be biased..
## now after feature selection we have 4 variables which are categorical variable and remaining 16 are numeric out of total 20 variables..
## Now cnames and cnames2 are the column names that contain numerical variable so we will do normalisation for this

#NOrmalisation on train data..

cnames = c("account.length","area.code","number.vmail.messages","total.day.minutes","total.day.calls","total.day.charge","total.eve.minutes","total.eve.calls",
           "total.eve.charge","total.night.minutes","total.night.calls","total.night.charge","total.intl.minutes","total.intl.calls","total.intl.charge","number.customer.service.calls")

for(i in cnames){
  print(i)
  data_train[,i] = (data_train[,i] - min(data_train[,i]))/
    (max(data_train[,i] - min(data_train[,i])))
}

#NOrmalisation on test data..

cnames = c("account.length","area.code","number.vmail.messages","total.day.minutes","total.day.calls","total.day.charge","total.eve.minutes","total.eve.calls",
           "total.eve.charge","total.night.minutes","total.night.calls","total.night.charge","total.intl.minutes","total.intl.calls","total.intl.charge","number.customer.service.calls")

for(i in cnames){
  print(i)
  data_test[,i] = (data_test[,i] - min(data_test[,i]))/
    (max(data_test[,i] - min(data_test[,i])))
}

#Now we will load the preprocessed test and train data in hard disk to our working directory location..
write.csv(data_train,"data_train_preprocessed.csv",row.names = T)
write.csv(data_test,"data_test_preprocessed.csv", row.names = T)  

## Now as this project is about classification as the output will be either 1 or 0 ( yes or no) so will follow classification approach..

### now we will use decision tree model on train data and we will predict the value for test..

##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(Churn ~., data_train, trials = 100, rules = TRUE)
library(C50)


#Summary of DT model
summary(C50_model)


#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")


#Lets predict for test cases
C50_Predictions = predict(C50_model, data_test[,-20], type = "class")

## Now we will apply some error metrics to check if our predicted model qualifies as per business standard.
## As this is classification problem statement so there is a matrix called confusion matrix from where we can find the accuracy and other parameters require to meet business requirment..

##Evaluate the performance of classification model
ConfMatrix_C50 = table(data_test$Churn, C50_Predictions)
library(caret)
confusionMatrix(ConfMatrix_C50)
##Now we will take a note of accuracy and FNR which is the concern in this problem as it is wrongly predicted in this case and 33 customers will actually churn out and we are wrongly predicted that.

#Accuracy = 95%
#FNR= 36%

#False Negative rate
FNR = FN/FN+TP

## So thats it from our first model developement( Decision Tree) that we use and we came with 95% accuracy and 36% FNR which is pretty much accecptable.. we can use other models as well to crosscheck these parameters..
library(ggplot2)
library(inTrees)
