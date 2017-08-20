
#Classification using SVM

rm(list=ls(all=T))
library(RCurl)

data=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth/Support_Vector_Machines/master/Bank_dataset.csv"), 
                   header=T, sep=',', col.names = c('ID', 'age', 'exp', 'inc', 
                                                        'zip', 'family', 'ccavg', 'edu', 
                                                        'mortgage', 'loan', 'securities', 
                                                        'cd', 'online', 'cc'))
str(data)

#Dropping id,zipcode and experience
drop_atr=c("ID","zip","exp")
final_data=data[setdiff(colnames(data),drop_atr)]
str(final_data)

#R Svm accepts only numeric data 
#Education,loan,cd,securities account,online,credit card into numeric using dummy function
#install.packages("dummies")
library(dummies)
ind_variable="loan"
num_atr=c("age","inc","family","ccavg","mortgage")
categ_atr=setdiff(names(final_data),c(num_atr,ind_variable))
final_data[num_atr]=data.frame(sapply(data[num_atr], as.numeric))
final_data[categ_atr]=data.frame(sapply(data[categ_atr], as.factor))
final_data[ind_variable]=data.frame(sapply(data[ind_variable], as.factor))
temp=dummy.data.frame(final_data[categ_atr])
final_data=cbind(final_data[num_atr],temp)
str(final_data)
names=c(num_atr,names(temp))

#Standardizing the data

#install.packages("vegan")
library(vegan)
final_data=decostand(x = final_data,method = "range")

#conbining dependent variable

final_data=cbind(final_data,data["loan"])

#Training and testing 
set.seed(123)
install.packages("caTools")
library(caTools)
temp=sample.split(final_data$loan,SplitRatio = 0.7)
train=final_data[temp,]
test=final_data[!temp,]

#Checking for stratified sampling
table(train$loan)
table(test$loan)

#Building SVM model for classifying loan defaulters
#install.packages("e1071")
library(e1071)
model=svm(loan~.,train,type="C-classification",kernel="linear",cost=1,gamma=0.1)
summary(model)

#Predicting on train data using SVM model
pred_train=predict(model,train[names])
summary(pred_train)
#Accuracy on training data

acc_train=table(train$loan,pred_train)
accuracy=sum(diag(acc_train))/sum(acc_train)
#On testing data

pred_test=predict(model,test[names])
pred_test2=table(test$loan,pred_test)
acc_test=round(sum(diag(pred_test2))/sum(pred_test2),2)


