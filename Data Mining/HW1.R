#3.
#(a) Read the data into R. Call the loaded data to college.
college<-read.csv(file.choose())

#(b).Write a code to eliminate the first column.
rownames(college)<-college [,1]
college[,1]<-NULL

#(c) Provide a summary statistics for numerical variables in the data set.
str(college)
summary(college)

#(d) Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. Recall that you can reference the first ten columns of a matrix A using A[,1:10].
pairs(college[,1:10])

#(e) Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(Outstate~Private,data=college)

#(f).
#(i).
Elite<-rep("No",nrow(college))
#Creating a new variable Elite with all the variables as "NO" with same number of rows as the data set "college".
Elite[college$Top10perc > 50] <- "Yes"
#Substituting the Elite variable with "yes" where top10perc is greater than 50.
Elite <-as.factor(Elite)
#Converting Elite into a factor.
college <-data.frame(college,Elite)
#Adding the column Elite to the data-set college.

#(ii).
summary(Elite)
#It shows that we have 78 Elite universities and 699 non-elite universities.
plot(Outstate~Elite,data=college)
#The plot shows that Elite universities have higher outstate tuition fee.

#(g).
par(mfrow=c(2,2))
hist(college$Accept,breaks = 20,main = "Break = 20")
hist(college$Accept,breaks = 50,main = "Break = 50")
hist(college$Accept,breaks = 70,main = "Break = 70")
hist(college$Accept,breaks = 100,main = "Break = 100")
#Plotting histograms for college vs Accept with varibale bin sizes.

hist(college$Enroll,breaks = 20,main = "Break = 20")
hist(college$Enroll,breaks = 50,main = "Break = 50")
hist(college$Enroll,breaks = 70,main = "Break = 70")
hist(college$Enroll,breaks = 100,main = "Break = 100")
#Plotting histograms for college vs Enroll with varibale bin sizes.

hist(college$Outstate,breaks = 20,main = "Break = 20")
hist(college$Outstate,breaks = 50,main = "Break = 50")
hist(college$Outstate,breaks = 70,main = "Break = 70")
hist(college$Outstate,breaks = 100,main = "Break = 100")
#Plotting histograms for college vs Outside with varibale bin sizes.



#4.

auto<-read.csv(file.choose())

attach(auto)
library("mice")
#(a).Remove the missing values from this data set.
str(auto)
auto$horsepower <- sub("?","",auto$horsepower)
auto$horsepower <- as.numeric(auto$horsepower)
auto <- na.omit(auto)

#Horse-power had missing values, we convert it into integer to remove the missing values.

#(b).Which of the predictors are quantitative, and which are qualitative? How do you check this information?
str(auto) #Shows all the predictors in the dataset.
#Checking if the predictors are qualitative or quantitative.
auto$cylinders<-as.factor(auto$cylinders)
str(auto$cylinders)
#Factor with 5 levels.
auto$year<-as.factor(auto$year)
str(auto$year)
#Factor with 13 levels.
auto$origin<-as.factor(auto$origin)
str(auto$origin)
#Factor with 3 levels.
#Predictors which are qualitative also known as categorical variables have finite number of levels whereas  quantitative predictors also known as continous variables have infinite number of levels.
#From the definition, we have categorised the parameters into quantitative and qualitative predictors:
#quantitative: mpg,displacement,horsepower,weight,acceleration.
#Qualitative : cylinder,year,origin.

#(c)What is the range of each quantitative predictor?
range(auto$mpg)
#Range : 9.0 to 46.6
range(auto$displacement)
#Range : 68 to 455
range(auto$horsepower)
#Range : 1 to 94
range(auto$weight)
#Range : 1613 to 5140
range(auto$acceleration)
#Range : 8.0 to 24.8

#(d) What is the mean and standard deviation of each quantitative predictor?
install.packages("psych")
library(psych)
psych::describe(auto$mpg)
#mean: 23.52 , sd=7.83
psych::describe(auto$displacement)
#mean: 193.53 , sd= 104.38
psych::describe(auto$horsepower)
#mean: 51.7 , sd=29.7
psych::describe(auto$weight)
#mean: 2970.26 , sd= 847.9
psych::describe(auto$acceleration)
#mean: 15.56 , sd= 2.75

#(e) Remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
auto_new <- auto[-c(10:85),]
psych::describe(auto_new)
#gives range , mean and standard deviation of each predictor in the new dataset.

#(f) Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings.
cor_mat <- round(cor(auto[,c("mpg","displacement","horsepower","weight","acceleration")]),2)
cor_mat

corrplot::corrplot(cor_mat,type = "upper")

table(auto[,c("cylinders","year","origin")])

par(mfrow=c(1,3))
barplot(prop.table(table(auto$cylinders))*100,main = "Plot of Count vs number of Cylinder",xlab = "Cylinder",ylab = "Count")
barplot(prop.table(table(auto$year))*100,main = "Plot of Count vs Year",xlab = "Year",ylab = "Count")
barplot(prop.table(table(auto$origin))*100,main = "Plot of Count vs Origin",xlab = "Origin",ylab = "Count")
dev.off()

par(mfrow=c(1,3))
boxplot(mpg~cylinders,main = "mpg vs Cylinder",xlab = "Cylinder",data =auto)
boxplot(mpg~year,main = "mpg vs Year",xlab = "Year",data =auto)
boxplot(mpg~origin,main = "mpg vs Origin",xlab = "Origin",data =auto)
dev.off()

#From the correlation matrix, it is quite evident that there is a strong postive association
#between Weight and displacement.Also, there is a strong negative correlation between mpg-displacement
#and mpg-weight.
#From the boxplots, we can say that as the number of cylinder increases, the mpg decreases.
#We can also conclude that as the origin number increases, the mpg increases.
#There is no relationship between year and mpg


#(g) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your
#plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer.

#Answer: 
#As per the above plots we can conclude that displacement and weight would have been great
#candidate for the predictive model, however, as they are highly correlated independent variables -
#multicollinearity may make the model less accurate.
#Moreover, we can use number of cylinder and origin along with either weight or displacement to build our
# the model to predict mpg.
#From the correlation plot, we can observe that weight and displacement are negatively correlated to mpg.


#Problem 5.

library(party)
library(rpart)
library(rattle)
Salary_class <- read.csv(file.choose())
attach(Salary_class)
str(Salary_class)
colnames(Salary_class)
#(a) Import the data set. Use a partition node to divide the data into 60% train, 40% test.
set.seed(1234)
ind <- sample(2, nrow(Salary_class), replace=TRUE, prob=c(0.6, 0.4))
trainSalData <- Salary_class[ind==1,]
testSalData <- Salary_class[ind==2,]

#(b) Create the default C&R decision tree. How many leaves are in the tree?
myformula = INCOME ~ AGE+EMPLOYER+DEGREE+MSTATUS+JOBTYPE+SEX+C.GAIN+C.LOSS+HOURS+COUNTRY
#Using the above formula with Income as the dependent variable.
Decision_tree <- ctree(myformula,data = trainSalData)
table(predict(Decision_tree),trainSalData$INCOME)
print(Decision_tree)
plot(Decision_tree)
rpart.plot(Decision_tree_rpart)
summary(Decision_tree_rpart)
#Creating C&R decision tree.
Decision_tree_rpart <- rpart(myformula,data = trainSalData,method = "class")

Decision_tree_rpart$frame[,1:2]

Decision_tree_rpart$frame$yval2[,5]
#We observe that we have 7 leaf nodes.
print(Decision_tree_rpart)
summary(Decision_tree_rpart)
#(c) What are the major predictors of INCOME? Justify your choice. How can you get this information
#from the software?
#Answer: Different ways we found to plot decision tree
print(Decision_tree_rpart)
plot(Decision_tree_rpart)
rpart.plot(Decision_tree_rpart)
text(Decision_tree_rpart, use.n = T)
prp(Decision_tree_rpart)

fancyRpartPlot(Decision_tree_rpart)
install.packages("ggtree")
library(ggtree)

dev.off()

#From the above plot of the decision tree, we can say that MSTATUS,JOBTYPE,SEX and C.GAIN are
#the major predictors of INCOME. We found this information from the plot of the decision tree.
#Variable importance
#MSTATUS  JOBTYPE   C.GAIN      SEX   DEGREE      AGE    HOURS EMPLOYER  COUNTRY 
#31       19       13       10        9        8        5        4        1 
#We got this information from the function summary() of decision_tree_rpart.

#(d) Give three rules that describe who is likely to have an INCOME > 50K and who is likely to
#have an income an 50K. These rules should be relevant (support at least 5% in the training sample)
#and strong (either confidence more than 75% and =>50K or 90% =<50K). If there are no three rules that 
#meet these criteria, give the three best rules you can.
asRules(Decision_tree_rpart)
#Rule 1:
#IF	  MSTATUS= Married-AF-spouse, Married-civ-spouse
#AND   JOBTYPE= Craft-repair, Farming-fishing, Handlers-cleaners, Machine-op-inspct, Other-service, Priv-house-serv, Transport-moving
#AND   C.GAIN>=5096
#THEN INCOME =>50K
#   
#Rule 2:
#IF   MSTATUS= Divorced, Married-spouse-absent, Never-married, Separated, Widowed
#AND   C.GAIN>=7140
#THEN INCOME =>50K
#   
#Rule 3:
#IF    MSTATUS= Married-AF-spouse, Married-civ-spouse
#AND   JOBTYPE= Adm-clerical, Armed-Forces, Exec-managerial, Prof-specialty, Protective-serv, Sales, Tech-support
#AND   DEGREE= 10th, 11th, 12th, 1st-4th, 5th-6th, 7th-8th, 9th, Assoc-acdm, Assoc-voc, HS-grad, Some-college
#AND   C.GAIN>=5096
#THEN INCOME =>50K   
#
#Rule 4:
#IF    MSTATUS= Married-AF-spouse, Married-civ-spouse
#AND   JOBTYPE= Adm-clerical, Armed-Forces, Exec-managerial, Prof-specialty, Protective-serv, Sales, Tech-support
#AND   DEGREE= 10th, 11th, 12th, 1st-4th, 5th-6th, 7th-8th, 9th, Assoc-acdm, Assoc-voc, HS-grad, Some-college
#AND   C.GAIN< 5096
#THEN INCOME <= 50K
# 
#Rule 5:
#IF    MSTATUS= Married-AF-spouse, Married-civ-spouse
#AND   JOBTYPE= ?, Craft-repair, Farming-fishing, Handlers-cleaners, Machine-op-inspct, Other-service, Priv-house-serv, Transport-moving
#AND   C.GAIN< 5096
#THEN INCOME <= 50K
#
#Rule 6:
#IF    MSTATUS= Divorced, Married-spouse-absent, Never-married, Separated, Widowed
#AND   C.GAIN< 7140
#THEN INCOME <= 50K

#(e) Create two more C&R trees. The first is just like the default tree except you do not prune tree
#to avoid overfitting (you need to let the model to grow to its full depth). The other does prune,
#but you require 500 records in a parent branch and 100 records in a child branch. You can also
#play with the complexity parameter. How do the three trees differ (briefly). Which seems most
#accurate on the training data? Which seems most accurate on the test data?

Decision_tree_rpart_full <- rpart(myformula,data = trainSalData,method = "class",control = rpart.control(minbucket=2,minsplit=1,cp=-1))
plot(Decision_tree_rpart_full)
text(Decision_tree_rpart_full,use.n = T, xpd= T)

Decision_tree_rpart_prune <- rpart(myformula,data = trainSalData,method = "class",control = rpart.control(minbucket=100,minsplit=500,cp=-1))
plot(Decision_tree_rpart_prune)
text(Decision_tree_rpart_prune, use.n = T, xpd= T)

Decision_tree_rpart$variable.importance
Decision_tree_rpart_full$variable.importance
Decision_tree_rpart_prune$variable.importance
#These 3 differs in the number of leaf nodes and importance allocated to each variable.
########Accuracy on training and testing data
table(predict(Decision_tree_rpart,trainSalData,type = "class"),trainSalData$INCOME)
table(predict(Decision_tree_rpart_full,trainSalData,type = "class"),trainSalData$INCOME)
table(predict(Decision_tree_rpart_prune,trainSalData,type = "class"),trainSalData$INCOME)

table(predict(Decision_tree_rpart,testSalData,type = "class"),testSalData$INCOME)
table(predict(Decision_tree_rpart_full,testSalData,type = "class"),testSalData$INCOME)
table(predict(Decision_tree_rpart_prune,testSalData,type = "class"),testSalData$INCOME)

#Accuracy and Error in training data
Decision_tree_rpart$accuracy <- (sum(predict(Decision_tree_rpart,trainSalData,type = "class")==trainSalData$INCOME)/length(trainSalData$INCOME))
Decision_tree_rpart$accuracy
Decision_tree_rpart_full$accuracy <- (sum(predict(Decision_tree_rpart_full,trainSalData,type = "class")==trainSalData$INCOME)/length(trainSalData$INCOME))
Decision_tree_rpart_full$accuracy
Decision_tree_rpart_prune$accuracy <- (sum(predict(Decision_tree_rpart_prune,trainSalData,type = "class")==trainSalData$INCOME)/length(trainSalData$INCOME))
Decision_tree_rpart_prune$accuracy
#Full depth decision tree is the most accurate on training data

Decision_tree_rpart$error <- 1-(sum(predict(Decision_tree_rpart,trainSalData,type = "class")==trainSalData$INCOME)/length(trainSalData$INCOME))
Decision_tree_rpart$error
Decision_tree_rpart_full$error <- 1-(sum(predict(Decision_tree_rpart_full,trainSalData,type = "class")==trainSalData$INCOME)/length(trainSalData$INCOME))
Decision_tree_rpart_full$error
Decision_tree_rpart_prune$error <- 1-(sum(predict(Decision_tree_rpart_prune,trainSalData,type = "class")==trainSalData$INCOME)/length(trainSalData$INCOME))
Decision_tree_rpart_prune$error

#Accuracy and Error in testing data
Decision_tree_rpart$accuracy <- (sum(predict(Decision_tree_rpart,testSalData,type = "class")==testSalData$INCOME)/length(testSalData$INCOME))
Decision_tree_rpart$accuracy
Decision_tree_rpart_full$accuracy <- (sum(predict(Decision_tree_rpart_full,testSalData,type = "class")==testSalData$INCOME)/length(testSalData$INCOME))
Decision_tree_rpart_full$accuracy
Decision_tree_rpart_prune$accuracy <- (sum(predict(Decision_tree_rpart_prune,testSalData,type = "class")==testSalData$INCOME)/length(testSalData$INCOME))
Decision_tree_rpart_prune$accuracy
#Full pruned with minimum 500 records in a parent branch and 100 records in a child branch is the most accurate on testing data

Decision_tree_rpart$error <- 1-(sum(predict(Decision_tree_rpart,testSalData,type = "class")==testSalData$INCOME)/length(testSalData$INCOME))
Decision_tree_rpart$error
Decision_tree_rpart_full$error <- 1-(sum(predict(Decision_tree_rpart_full,testSalData,type = "class")==testSalData$INCOME)/length(testSalData$INCOME))
Decision_tree_rpart_full$error
Decision_tree_rpart_prune$error <- 1-(sum(predict(Decision_tree_rpart_prune,testSalData,type = "class")==testSalData$INCOME)/length(testSalData$INCOME))
Decision_tree_rpart_prune$error