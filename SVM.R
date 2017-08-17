rm(list=ls(all=T))
library(RCurl)
data=read.table(text = getURL("https://raw.githubusercontent.com
               /rajsiddarth/Datasets/master/Bank_dataset.csv"), header=T, sep=',')
                
str(data)
drop_atr=c("ID","ZIP.Code","Experience")
names=setdiff(colnames(data),drop_atr)
final_data=data[names]
str(final_data)
set.seed(123)
library(caret)
library(MASS)
temp=sample.split(final_data$Personal.Loan,SplitRatio = 0.7)
train=final_data[temp,]
test=final_data[!temp,]
ind_Atr=setdiff(colnames(train),"Personal.Loan")
table(train$Personal.Loan)
table(test$Personal.Loan)
model=svm(train[ind_Atr],train$Personal.Loan,type="C-classification",kernel="linear",cost=1,gamma=0.1)
summary(model)
pred=predict(model,train[ind_Atr])
summary(pred)
names(pred)
acc_train=table(train$Personal.Loan,pred)
accuracy=sum(diag(acc_train))/sum(acc_train)
pred_test=predict(model,test[ind_Atr])
pred_test2=table(test$Personal.Loan,pred_test)
acc_test=sum(diag(pred_test2))/sum(pred_test2)
#svm regression
rm(list=ls(all=T))

data2=read.csv("BostonHousing.csv")
str(data2)
temp2=sample
model2=svm()
