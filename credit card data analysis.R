#This is how yiu call the packages
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(plyr)

setwd("G:\\R Language\\session\\credit card data")

# Reading data; just change the file path to fetch the data.
data <- read.csv("credit card data.csv",header = TRUE,na.strings="")
##dim() function used to set or retrive the dimension of data frame , matrix , object
dim(data)
##number of row=5000 and no of column=23
str(data)
summary(data)

## Frequency distribution
table(data$Purposre_Credit_Taken)
##count() function will give you frequency distribution of assigned variables
count(data,"Purposre_Credit_Taken")

## seems A410 is a typing error, to replacing the values with A41
## First convert the variable into character then change with ifelse and then change it back to  factor again

#First change in character .....###
data$Purposre_Credit_Taken <- as.character(data$Purposre_Credit_Taken)

#Now upgrade A410 with A41 using elseif function
data$Purposre_Credit_Taken<-ifelse(data$Purposre_Credit_Taken=="A410","A41",data$Purposre_Credit_Taken)

##Now change the Purposre_Credit_Taken to Factor
data$Purposre_Credit_Taken<-as.factor(data$Purposre_Credit_Taken)

str(data$Purposre_Credit_Taken)
table(data$Purposre_Credit_Taken)
str(data)
# Converting necessary variables into factor
data$Inst_Rt_Income <- as.factor(data$Inst_Rt_Income)
str(data$Inst_Rt_Income)

##applying Logistic Regression on Full data

## Remove the insignificant variable : Customer ID & Count , because its not productive for logistic regression

model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	Credit_History+	 Purposre_Credit_Taken
             +	Credit_Amount+	Savings_Acc+	Years_At_Present_Employment+	Inst_Rt_Income+	 Marital_Status_Gender
             +	Other_Debtors_Guarantors+	Current_Address_Yrs+	Property+	Age+	 Other_Inst_Plans +	Housing
             +	Num_CC+	Job+	Dependents+	Telephone+	Foreign_Worker, data=data, family=binomial())
##the binomial family the links logit, probit, cauchit##
summary(model)
##removing insignificant data Current_Address_Yrs
model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	Credit_History+	 Purposre_Credit_Taken
             +	Credit_Amount+	Savings_Acc+	Years_At_Present_Employment+	Inst_Rt_Income+	 Marital_Status_Gender
             +	Other_Debtors_Guarantors+	Property+	Age+	 Other_Inst_Plans +	Housing
             +	Num_CC+	Job+	Dependents+	Telephone+	Foreign_Worker, data=data, family=binomial())
##the binomial family the links logit, probit, cauchit##
summary(model)

##Removing insignificant data Property because p-value<0.05
model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	Credit_History+	 Purposre_Credit_Taken
             +	Credit_Amount+	Savings_Acc+	Years_At_Present_Employment+	Inst_Rt_Income+	 Marital_Status_Gender
             +	Other_Debtors_Guarantors+	Age+	 Other_Inst_Plans +	Housing
             +	Num_CC+	Job+	Dependents+	Telephone+	Foreign_Worker, data=data, family=binomial())
summary(model)
##removing age P-value is very less
model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	Credit_History+	 Purposre_Credit_Taken
             +	Credit_Amount+	Savings_Acc+	Years_At_Present_Employment+	Inst_Rt_Income+	 Marital_Status_Gender
             +	Other_Debtors_Guarantors+	 Other_Inst_Plans +	Housing
             +	Num_CC+	Job+	Dependents+	Telephone+	Foreign_Worker, data=data, family=binomial())
summary(model)
model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	Credit_History+	 Purposre_Credit_Taken
             +	Credit_Amount+	I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +Years_At_Present_Employment+	Inst_Rt_Income+	I(Marital_Status_Gender=="A93")
             +	Other_Debtors_Guarantors+	I(Other_Inst_Plans=="A143") +	I(Housing=="A153")
             +		Dependents+	Telephone+	Foreign_Worker
             , data=data, family=binomial())
summary(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+	 Purposre_Credit_Taken
             +	Credit_Amount+	I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +Years_At_Present_Employment+	Inst_Rt_Income+	I(Marital_Status_Gender=="A93")
             +	Other_Debtors_Guarantors+	I(Other_Inst_Plans=="A143") +	I(Housing=="A153")
             +		Dependents+	Telephone+	Foreign_Worker
             , data=data, family=binomial())
summary(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	I(Credit_History=="A32")+ I(Credit_History=="A33")+ I(Credit_History=="A34")+	 
               I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")
             +	Credit_Amount+	I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +Years_At_Present_Employment+	I(Inst_Rt_Income=="3")+ I(Inst_Rt_Income=="4")+	I(Marital_Status_Gender=="A93")
             +	Other_Debtors_Guarantors+	I(Other_Inst_Plans=="A143") +
             +		Dependents+	Telephone+	Foreign_Worker
             , data=data, family=binomial())
summary(model)


model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	 I(Credit_History=="A33")+ I(Credit_History=="A32")+ I(Credit_History=="A34")+	 
               I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")
             +	Credit_Amount+ I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             + I(Years_At_Present_Employment=="A74")+	I(Inst_Rt_Income=="3")+ I(Inst_Rt_Income=="4")+	I(Marital_Status_Gender=="A93")
             +	Other_Debtors_Guarantors+	I(Other_Inst_Plans=="A143") +
               +		Dependents+	Telephone+	Foreign_Worker
             , data=data, family=binomial())
summary(model)

####multicollinerity Test####
vif(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc+	Duration_in_Months+	 I(Credit_History=="A33")+  I(Credit_History=="A34")+	 
               I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A42")+ I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")
             +	Credit_Amount+ I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             + I(Years_At_Present_Employment=="A74")+	I(Inst_Rt_Income=="3")+ I(Inst_Rt_Income=="4")+	I(Marital_Status_Gender=="A93")
             +	Other_Debtors_Guarantors+	I(Other_Inst_Plans=="A143") +
               +		Dependents+	Telephone+	Foreign_Worker
             , data=data, family=binomial())
summary(model)
vif(model)
############################################################################################################################
# R square (nagalkarke)
# Difference between -2LL of Null model and model with variables
modelChi <- model$null.deviance - model$deviance
#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
# With more decimal places
chisq.prob <- 1 - pchisq(modelChi, chidf)
R2.hl<-modelChi/model$null.deviance
R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data))
R.n <- R.cs /(1-(exp(-(model$null.deviance/nrow(data)))))
R.n
## value is 0.36 not close to 1 but it is okay model

#########################################################################################################
# Predicted Probabilities
prediction <- predict(model,newdata = data,type="response")
data$Default_On_Payment <- as.factor(data$Default_On_Payment)
library(pROC)
rocCurve   <- roc(response = data$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data$Default_On_Payment)))
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc (rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

##gini is 0.64 , its good model
##Auc 0.82 , trade off between sensitivity and specificity is excellent
#######################################################################################################
##KS statistics calculation
data$m1.yhat <- predict(model, data, type = "response")
library(ROCR)
m1.scores <- prediction(data$m1.yhat, data$Default_On_Payment)
plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")
m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70
###value is 0.5 lies between 0.4 to 0.7 so able to differentiate between good or bad disributors

##################################################################################################\

