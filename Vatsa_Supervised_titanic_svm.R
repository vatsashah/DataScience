getwd()

rm(list = ls())

library(caret)

train=read.csv("C:/Users/Vatsa Shah/Documents/train.csv")
test=read.csv("C:/Users/Vatsa Shah/Documents/test.csv")

#creating new column to determine whether it is part of train data or test data
train$istrain = TRUE 
test$istrain = FALSE

dim(train)
dim(test)

test$Survived = NA #creating new column (Survived) in test data. 

fulldata=rbind(test,train) # merging test and train data

table(fulldata$istrain)

#assigning 'S' value to unknown values of embarked
fulldata[fulldata$Embarked=='',"Embarked"]='S' 
table(fulldata$Embarked)

# finding median of age from fulldata set
medianage=median(fulldata$Age,na.rm = TRUE) 
medianage

# assigning unknown values of age as median of age's from fulldata
fulldata[is.na(fulldata$Age),"Age"]=medianage

table(is.na(fulldata$Age))

# finding median of fare from fulldata set
medianfare=median(fulldata$Fare,na.rm = TRUE) 
medianfare

# assigning unknown values of fare as median of fare's from fulldata
fulldata[is.na(fulldata$Fare),"Fare"]=medianfare 
table(is.na(fulldata$Fare))

fulldata$Pclass=as.factor(fulldata$Pclass)
fulldata$Sex=as.factor(fulldata$Sex)
fulldata$Embarked=as.factor(fulldata$Embarked)

#split data set back into train and test
train=fulldata[fulldata$istrain==TRUE,] 
test=fulldata[fulldata$istrain==FALSE,]

Actual_data=read.csv("C:/Users/Vatsa Shah/Documents/gender_submission.csv")

train$Survived=as.factor(train$Survived)

ModFit_SVM = train(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare ,train,method="svmLinear",preProc=c("center","scale"))

predict_SVM = predict(ModFit_SVM,newdata=test)
predict_SVM

confusionMatrix(predict_SVM,factor(Actual_data$Survived))


