library(randomForest)
library(caret)
library(corrplot)
library(readxl)
library(ROCR)
library(pROC)
library(partykit)
library(party)
library(rpart)
dean <- read_xlsx(file.choose())
#Cleaning the data
summary(dean)
str(dean)
dean$`Gender-B`<-as.factor(dean$`Gender-B`)
colnames(dean)[3]<-"GenderB"
dean$Board_CBSE<-as.factor(dean$Board_CBSE)
dean$Board_ICSE<-as.factor(dean$Board_ICSE)
dean$Degree_Engg<-as.factor(dean$Degree_Engg)

dean$`S-TEST`<-as.factor(dean$`S-TEST`)
dean$Placement<-as.factor(dean$Placement)
dean$Placement_B<-as.factor(dean$Placement_B)
colnames(dean)[19]<-"S_TESTSCORE"
colnames(dean)[16]<-"S_TEST"

which(is.na(dean$Entrance_Test))
for (i in which(is.na(dean$Entrance_Test))){
  dean$Entrance_Test[i]="None"
}
which(is.na(dean$Entrance_Test))

#To look at the number of applicants who got placed
barplot(table(dean$Placement_B))
#`Gender-B`,Percent_SSC,Board_CBSE,Board_ICSE,Percent_HSC,Percent_Degree,Degree_Engg,Experience_Yrs,s-`S-TEST`,Percentile_ET,s-`S-TEST*SCORE`

dean$HSC_CBSE <- ifelse(grepl("CBSE",dean$Board_HSC,ignore.case = T),1,0)

dean$HSC_ISC <- ifelse(grepl("ISC",dean$Board_HSC,ignore.case = T),1,0)

dean$Stream_HSC_C <- ifelse(grepl("Commerce",dean$Stream_HSC,ignore.case = T),1,0)

dean$Stream_HSC_A <- ifelse(grepl("Arts",dean$Stream_HSC,ignore.case = T),1,0)

dean$Stream_HSC_S <- ifelse(grepl("Science",dean$Stream_HSC,ignore.case = T),1,0)

dean$Degree_mgmt <- ifelse(grepl("Management",dean$Course_Degree,ignore.case = T),1,0)

dean$Degree_cmrc <- ifelse(grepl("Commerce",dean$Course_Degree,ignore.case = T),1,0)

dean$HSC_CBSE<-as.factor(dean$HSC_CBSE)
dean$HSC_ISC<-as.factor(dean$HSC_ISC)
dean$Stream_HSC_C <- as.factor(dean$Stream_HSC_C)
dean$Stream_HSC_A <- as.factor(dean$Stream_HSC_A)
dean$Stream_HSC_S <- as.factor(dean$Stream_HSC_S)
dean$Degree_cmrc <- as.factor(dean$Degree_cmrc)
dean$Degree_mgmt <- as.factor(dean$Degree_mgmt)
#dean_new <- dean[,-c(3,4,6,7,8,11,13,14,16,17,19,27,28,29,30,31,25)]
dean_new <- dean[,-c(1,2,5,9,10,12,15,19,20,24,26)]

#1.
null = glm(Placement_B~1, data= dean_new,family = "binomial") # only includes intercept
full = glm(Placement_B~., data= dean_new,family = "binomial") # includes all the

# We can perform step-wise selection using the command:
step(null, scope=list(lower=null, upper=full), direction="both")
#Therefore, we will consider Percent_SSC ,Percentile_ET ,Marks_Communication,Marks_Projectwork,S_TEST, GenderB
#We are finding variables to predict placement by using random forest:
set.seed(1234)
a.r<-randomForest(Placement_B~.,data = dean_new)
varImpPlot(a.r)
#From the plot , we observe that *** are the important variables we should consider while predicting placement of a student.

#2
a1<-dean_new[,c(2,5,6,8,10,11,12,13,14)]
corrplot(cor(a1),method="number")
dev.off()
#From the correlation matrix we observe that marks in communication,projectwork and BOCA are highly dependent on Percent_MBA.
#And Percent_MBA is dependent on Percent_SSC, Percent_HSC, Percent_Degree.
#Therefore, By incorporating these 3 parameters,we are taking MBA marks into consideration.

#3.
mylogreg <- glm(Placement_B~Percent_SSC, data= dean_new,family = "binomial") # only includes SSC percent
summary(mylogreg)
#As per the above output, we can say that as the percent SSC increases by 1 unit, the log of odds increases by 3.9%
pred1 <- predict(mylogreg,newdata = dean_new,type = "response")
plot(dean_new$Percent_SSC,pred1)
#From the above graph we get to know that as the SSC percent increase, the probablity of getting placed also increases.

#4.
#Logistic function
# Log(odds)= -1.10800+(Percent_SSCC)*0.03922
#Log(odds)=-1.10800 + (0.03922*60)
#odds= exp(1.2452)=3.473629
#P=(odds/1+odds)=0.7764678
# Probability that a student with 60% in SSC will be placed is 0.7764678 ~0.78
#Log(odds)=-1.10800 + (0.03922*80)
#odds= exp(2.0296)=7.611041
#P=(odds/1+odds)=0.88387
# Probability that a student with 80% in SSC will be placed is 0.88387 ~0.88

#5.
pred = prediction(pred1,dean_new$Placement_B)
#using model built in part 3.
perf = performance(pred,"tpr","fpr")
plot(perf)
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}
print(opt.cut(perf, pred))
#optimal cut-off value is 0.7864729

#6.
mylogit<-glm(Placement_B~., data = dean_new,family="binomial")
summary(mylogit)
#From the above output, we can observe that GenderB,Percent_SSC,S-test,percentile_ET which are 10% significant.
mymod<-glm(Placement_B~GenderB+Percent_SSC+S_TEST+Percentile_ET,data = dean_new,family = "binomial")
summary(mymod)
#From the above output, we can say that 
#If the GenderB value  is 1, the probablity of odds of getting placed decreases by 49.8%
#For increase in percent_SCC by 1 unit, the probablity of log odds increases by 3.57%
#If the Stest value  is 1, the probablity of odds of getting placed decreases by 96.51%
#For increase in percentile_ET by 1 unit, the probablity of log odds increases by 1.79%

#7
pred3 <- predict(mymod,newdata =dean_new, type = "response")
cpred = prediction(pred1,dean_new$Placement_B)
perf = performance(cpred,"tpr","fpr")
plot(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- round(cutoffs,2)
reqcutoff <- cutoffs[cutoffs[,1]==0.77,]
reqcutoff$specificity <- 1-reqcutoff$fpr
reqcutoff
#Having specificity of around 50% says that the model is able to identify applicant who will not get placed 50% of times.
#High specificity gives you few false positive.
#However, as the number of applicants increases, the number of applicants who will not get placed will also increase.
#To conclude, Easwaran Iyer should NOT use the cut-off probability of 0.77 in deciding whether to admit the students. 

#8.
cost.perf = performance(cpred, "cost", cost.fp = 4, cost.fn = 1)
plot(cost.perf)
cpred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
#We can see that we get the lowest cost at 0.759 cut-off.
#So, we choose 0.759 as the cut-off probability after considering what Easwaran Iyer believes.

#9
dean$Gender<-as.factor(dean$Gender)
dean$Board_SSC<-as.factor(dean$Board_SSC)
dean$Board_HSC<-as.factor(dean$Board_HSC)
dean$Stream_HSC<-as.factor(dean$Stream_HSC)
dean$Course_Degree<-as.factor(dean$Course_Degree)
dean$Experience_Yrs<-as.factor(dean$Experience_Yrs)
dean$Entrance_Test<-as.factor(dean$Entrance_Test)
dean$Specialization_MBA<-as.factor(dean$Specialization_MBA)
dean$Placement_B<-as.factor(dean$Placement_B)

dean_new2<-dean[,c(2,5,9,10,12,14,15,20,25)]
myformula= Placement_B~Gender + Board_SSC+Board_HSC+Stream_HSC+Course_Degree+Experience_Yrs+Entrance_Test+Specialization_MBA
chaid_cat<-ctree(myformula,data=dean_new2)
summary(chaid_cat)
chaid_cat


# predict new data
predClass = predict(chaid_cat, newdata=dean, type="response")
predProb = sapply(predict(chaid_cat, newdata=dean,type="prob"),'[[',2)  # obtain probability of class 1 (second element from the lists)
predNode = predict(chaid_cat, newdata=dean, type="node")   # obtain the predicted node (in case you need it)
# plot ROC
roc_pred <- prediction(predProb, dean$Placement_B)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, col="red")
abline(0,1,col="grey")
# get area under the curve
performance(roc_pred,"auc")@y.values



