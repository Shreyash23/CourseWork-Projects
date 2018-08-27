library(corrplot)
library(readxl)
library(ggplot2)
library(mice)
library(ROSE)
library(randomForest)
library(caret)
library(nnet)
library(ROCR)

#Cleaning the data:
summary(Predicting_Customer_Churn)
str(Predicting_Customer_Churn)
#Changing thr name of columns for convinience:
colnames(Predicting_Customer_Churn)[2]<-"Customer_Age"
colnames(Predicting_Customer_Churn)[3]<-"Churn"
colnames(Predicting_Customer_Churn)[4]<-"CHI_Score_Current"
colnames(Predicting_Customer_Churn)[5]<-"CHI_Score_Change"
colnames(Predicting_Customer_Churn)[6]<-"Support_Cases_Current"
colnames(Predicting_Customer_Churn)[7]<-"Support_Cases_Change"
colnames(Predicting_Customer_Churn)[8]<-"SP_Current"
colnames(Predicting_Customer_Churn)[9]<-"SP_Change"
colnames(Predicting_Customer_Churn)[10]<-"Logins_Change"
colnames(Predicting_Customer_Churn)[11]<-"Blog_Articles_Change"
colnames(Predicting_Customer_Churn)[12]<-"Views_Change"
colnames(Predicting_Customer_Churn)[13]<-"Days_lastlogin_Change"

#Creating new column CHI_monthly beacuse comparing CHI score with varied number of months is not proper.
#CHI_Current is not the correct parameter to compare different customers.
#CHI_Monthly= CHI_current/Customer_Age depicts a common scale for comparison.
Predicting_Customer_Churn$CHI_Monthly<-Predicting_Customer_Churn$CHI_Score_Current/Predicting_Customer_Churn$Customer_Age
#Changing the churn value from numeric to factor:
Predicting_Customer_Churn$Churn<-as.factor(Predicting_Customer_Churn$Churn)
table(Predicting_Customer_Churn$Churn)
#Shows the number of churns and non-churns.
sum(is.na(Predicting_Customer_Churn))
#No null values are present.

#Dropping the customer ID as we don't need it to build the models.
Predicting_Customer_Churn <- Predicting_Customer_Churn[,-c(1)]


barplot(table(Predicting_Customer_Churn$Churn))
#Imbalanced data - evaluation measure is precision and recall

#1.
#Checking Customer' age variable
plot(Predicting_Customer_Churn$Churn,Predicting_Customer_Churn$Customer_Age,xlab="churn", ylab="customer age in months", main="Distribution of churn wrt age")
#From the graph, we get to know that the median of customer age for churn 1 and 0 is almost same with a slightly higher value for churn =1
#Considering the fact that the data is imbalanced, we run a logistics regression model to understand the significance of the variable customer age 
str(Predicting_Customer_Churn$Customer_Age)
log_age <- glm(Churn~Customer_Age,data = Predicting_Customer_Churn,family = "binomial")
summary(log_age)
r_sqr <- 1-(log_age$deviance/log_age$null.deviance)
r_sqr*100
#From the above output, we understand that the variable- customer age is significant and can be factor in predicting churn.
#It explains 0.21% of the variation in the data.

#Taking into consideration Wall's belief
Predicting_Customer_Churn$Customer_Age_category <- ifelse(Predicting_Customer_Churn$Customer_Age>=14,1,ifelse(Predicting_Customer_Churn$Customer_Age<=6,2,3))
#Customer_Age_category = 1 (Customer age greater than 14)
#Customer_Age_category = 2 (Customer age less than 6)
#Customer_Age_category = 3 (Customer age between than 6 and 14)
ggplot(data = Predicting_Customer_Churn) +
  geom_bar(aes(x = factor(Customer_Age_category), fill = factor(Churn)), position = "fill")
#As per the above graph, we say that Wall's belief for customer who are more than 14 months old hold false as the churn is more than customer's who are less than 6 years old.
#For the customer's who are less than 6 months old, the churn is less as compared to other two categories.
#Finally, Wall' belief for the customer's age between 6 and 14 months holds true as the churn is highest for this category.

#2.
#Balancing the data
balanced_churn_data <- ROSE(Churn~.,data = Predicting_Customer_Churn,seed = 1)$data
table(balanced_churn_data$Churn)
#We use this dataset for further analysis.

#Training and testing datasets
set.seed(1234)
ind = sample(2, nrow(balanced_churn_data), replace = T, prob = c(0.7, 0.3))
TrainBalData = balanced_churn_data[ind == 1, ]
TestBalData = balanced_churn_data[ind == 2, ]

#Model using logistic regression.
log_churn <- train(Churn~.,data = balanced_churn_data,trControl=trainControl(method="cv", number=10),method="glm")
print(log_churn)
log.pred <- predict(log_churn,newdata = balanced_churn_data)
#
log_churn <- glm(Churn~.,data = TrainBalData,family = "binomial")
log.pred <- predict(log_churn,newdata = TestBalData,type ="response")
fitted.log.results <- ifelse(log.pred>0.5,1,0)
roc.curve(TestBalData$Churn,fitted.log.results)

#model using Random forest
rf_churn <- train(Churn~.,data = balanced_churn_data,trControl=trainControl(method="cv", number=10),method="rf")
print(rf_churn)
rf.pred <- predict(rf_churn,newdata = balanced_churn_data)

#
rf_churn <- randomForest(Churn~.,data = TrainBalData,proximity =TRUE,importance=TRUE)
rf.pred <- predict(rf_churn,newdata = TestBalData,type ="response")
roc.curve(TestBalData$Churn,rf.pred)

#model using Neural Networs
#Normalising the variables
maxs = apply(balanced_churn_data[ ,-2], 2, max)
mins = apply(balanced_churn_data[ ,-2], 2, min)
scaled.data = as.data.frame(scale(balanced_churn_data[ , -2], center = mins, scale = maxs - mins))
scaled.data <- cbind(scaled.data,balanced_churn_data$Churn)
colnames(scaled.data)[14] <- "Churn"

#Training and testing data set.
set.seed(1234)
ind = sample(2, nrow(scaled.data), replace = T, prob = c(0.7, 0.3))
TrainData = scaled.data[ind == 1, ]
TestData = scaled.data[ind == 2, ]

#Building the NN model
nn_churn <- nnet(Churn~.,data = TrainData,linout=F,size=10,decay=0.01,maxit=1000)

#Predicting the churn
nn.pred<- predict(nn_churn,TestData,type = "class")
roc.curve(TestData$Churn,nn.pred)


#Comparing all the models
pd1 <- prediction(as.numeric(TestBalData$Churn),as.numeric(fitted.log.results))
pf1 <- performance(pd1, "tpr","fpr")

pd2 <- prediction(as.numeric(TestBalData$Churn),as.numeric(rf.pred))
pf2 <- performance(pd2, "tpr","fpr")

pd3 <- prediction(as.numeric(TestData$Churn),as.numeric(nn.pred))
pf3 <- performance(pd3, "tpr","fpr")

plot(pf1,col = "red")
plot(pf2, add = TRUE, col = "green")
plot(pf3, add = TRUE, col = "blue")







