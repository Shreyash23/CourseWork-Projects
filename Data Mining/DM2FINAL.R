#Problem 5
library(kernlab)
library(caret)
library(e1071)
library(randomForest)
data("ticdata")
attach(ticdata)
tic0 = ticdata[1:5822, ]
tic1 = ticdata[-(1:5822), ]
#install.packages("randomForest")
library(randomForest)

#a)
set.seed(1234)
randForTic <- randomForest(CARAVAN~.,data = tic0, proximity =TRUE,importance=TRUE)
print(randForTic)
randForTic$confusion
#The accuracy of this model is calculated from the confusion matrix plotted above which is 93.99%

#b)
set.seed(1234)
randForTic_1 <- randomForest(CARAVAN~.,data = tic1, proximity =TRUE,importance=TRUE)
InsuranceBuy <- as.data.frame(randForTic_1$votes)
mostlikely1000 <- head(InsuranceBuy[order(InsuranceBuy$insurance,decreasing = TRUE),],1000)
#Code to choose all the 1000 individuals who,based on the model, seem 
#most likely to buy policies.
n <- tic1[row.names(mostlikely1000),]
table(n$CARAVAN)

#Using the below function to understand the question.
bestsize <- function(n0=696,mtry=9,nselect=1000,form=CARAVAN~.,data=tic0[,-1])
{
  ticm.rf <- randomForest(form, sampsize=c(n0,348),mtry=mtry,data=data)
  nr <- (1:nrow(tic0))[order(ticm.rf$votes[,2], decreasing=T)[1:nselect]]
  sum(tic0[nr, 86]=="insurance")
}
#function to plot the accuracy, recall, precision and the number of insurance buyer predicted by each model due to the
#changes in the n0 value of the sample size.
optimalSampSize <- function(n0=369,lastnum=1000,breaks=50,n1=348,mtry=9,nselect=1000,form=CARAVAN~.,data=tic0[,-1])
{
  a<-NULL
  b<-NULL
  r<-NULL
  p<-NULL
  par(mfrow=c(2,2))
  ptm <- proc.time()
  for (n0 in seq(from=n0, to=lastnum, by=breaks))
  { cat(n0 , "of" , lastnum,"\n")
    set.seed(1234)
    ticm.rf <- randomForest(form, sampsize=c(n0,348),mtry=mtry,data=data)
    nr <- (1:nrow(tic0))[order(ticm.rf$votes[,2], decreasing=T)[1:nselect]]
    b<-rbind(b,c(n0,sum(tic0[nr, 86]=="insurance")))
    pred3 <- predict(ticm.rf,tic0,type="class")
    a <- rbind(a,c(n0,confusionMatrix(pred3,tic0$CARAVAN)$overall['Accuracy']))
    r <- rbind(r,c(n0,confusionMatrix(pred3,tic0$CARAVAN)$byClass['Recall']))
    p <- rbind(p,c(n0,confusionMatrix(pred3,tic0$CARAVAN)$byClass['Precision']))
  }
  maxa <- max(a[,2])
  maxb <- max(b[,2])
  maxr <- max(r[,2])
  maxp <- max(p[,2])
  cat("Highest Accuracy :", subset(a,a[,2]==maxa),"\n")
  cat("Highest Recall :", subset(r,r[,2]==maxr),"\n")
  cat("Highest Precision :", subset(p,p[,2]==maxp),"\n")
  cat("Highest Number :", subset(b,b[,2]==maxb),"\n")
  plot(a,xlab="n0",ylab="Accuracy")
  plot(r,xlab="n0",ylab="Recall")
  plot(p,xlab="n0",ylab="Precision")
  plot(b,xlab="n0",ylab="Number of correct insurance purchasers predicted")
  
  proc.time() - ptm
}
#We observe that the recall and Accuracy increase as we increase the n0, however the Precision decreases.
optimalSampSize(lastnum = 1000,n0=100,breaks = 100)

##From the above output, we see that we get highest number of insurance buyer when we use sampsize for n0 as 200 and 400.Looking at the Accuracy, we tend to select the sampsize of 400.

#c)
#1)
set.seed(1234)
acc <- NULL
insur <- NULL
for (i in seq(from=1, to=15)){
  set.seed(1234)
  randForTic_2 <- randomForest(CARAVAN~.,data = tic0,mtry=i,sampsize=c(400,348))
  pred4 <- predict(randForTic_2,tic0,type="class")
  acc <- rbind(acc,c(i,confusionMatrix(pred4,tic0$CARAVAN)$overall['Accuracy']))
  nr <- (1:nrow(tic0))[order(randForTic_2$votes[,2], decreasing=T)[1:1000]]
  insur <- rbind(insur,c(i,sum(tic0[nr, 86]=="insurance")))
}
par(mfrow=c(1,2))
plot(acc,xlab="mtry",ylab="Accuracy")
plot(insur,xlab="mtry",ylab="No. of insurance purchaser")
#The above set commands tells us that when we keep mtry as 10 or 13 we get the highest number of insurance purchasers, However, we can see that the accuracy of the model with mtry=10 is higher than the model with mtry=13.
#So we choose mtry=10 as the optimal value.
#Using the above found mtry we get better estimated number of insurance purchasers as compared to using the default mtry. 

#2)
set.seed(1234)
tuneRF(tic0[,-86],tic0[,86],trace = TRUE,plot = TRUE,doBest = TRUE,sampsize=c(400,348))

#Optimal value of mtry using tuneRF function is 18

#Here, using the mtry suggested by the tuneRF gives less accurate model for the specified arguments as compared to the above manual detection of mtry for optimal accuracy and number of insurance purchasers.

#d)
set.seed(1234)
subrow <- sample(1:nrow(tic0), 2000)
tic9 <- tic0[subrow, ]
tab9 <- table(tic9[,86])
ssize <- as.vector(rep(tab9[2], 2))
set.seed(1234)
ticm9.rf <- randomForest(CARAVAN ~ ., samp_size=ssize, mtry=10, data=tic9[,-1], proximity=TRUE)
pts <- cmdscale(1-ticm9.rf$proximity)

#Now plot the points, identifying the levels of STYPE:

x <-pts[,1]
y <-pts[,2]
xyplot(y ~ x, panel=function(x,y){panel.text(x,y,labels=paste(1:37))})
order(table(tic9$STYPE),decreasing = TRUE)
#[1] 15 16 32 13 21 36  4 10 34 14 18 35 19 12 39 29  7 20  9 33  3 24 17  2 23 37 11 27  1  5 25
#[32]  6 22 30 38 28  8 26 31
club <- NULL
club <- cbind(club,pts,tic9$STYPE)
club <- as.data.frame(club)

#Plotting individual plots for each factor and club the one's which overlap and making such 5-6 groups

par(mfrow=c(2,2))
for (i in 1:39){
  sub <-subset(club,club$V3==c(i))
  plot(sub$V1,sub$V2,main=i)
}


#Observing the above plots, we group the observations in the following way.
#Category A: 1,5,11,23,33,37
#Category B: 2,3,9
#Category C: 4,7,10,12,13,14,15,16,18,20,21,24,29,32,34,35,36,39
#Category D: 6,19,17,22,25,30,28,27
#Category E: 8,31,38
#Creating 5 variables based on the above observations.
for (i in 1: nrow(tic9)){
  tic9$ASTYPE[i] <- ifelse(tic9$STYPE[i]=="Affluent senior apartments"||tic9$STYPE[i]=="Dinki's (double income no kids)"||tic9$STYPE[i]=="Large family farms"||tic9$STYPE[i]=="Porchless seniors: no front yard"||tic9$STYPE[i]=="Very Important Provincials"||tic9$STYPE[i]=="Young seniors in the city",1,0)
  tic9$BSTYPE[i] <- ifelse (tic9$STYPE[i]=="Affluent young families"||tic9$STYPE[i]=="Career and childcare"||tic9$STYPE[i]=="High Income"||tic9$STYPE[i]=="expensive child",1,0)
  tic9$CSTYPE[i] <- ifelse (tic9$STYPE[i]=="Couples with teens 'Married with children'"||tic9$STYPE[i]=="Family starters"||tic9$STYPE[i]=="High status seniors"||tic9$STYPE[i]=="Large family, employed child"||tic9$STYPE[i]=="Large religous families"||tic9$STYPE[i]=="Low income catholics"||tic9$STYPE[i]=="Lower class large families"||tic9$STYPE[i]=="Middle class families"||tic9$STYPE[i]=="Mixed rurals"||tic9$STYPE[i]=="Mixed small town dwellers"||tic9$STYPE[i]=="Modern, complete families"||tic9$STYPE[i]=="Religious elderly singles"||tic9$STYPE[i]=="Stable family"||tic9$STYPE[i]=="Traditional families"||tic9$STYPE[i]=="Village families"||tic9$STYPE[i]=="Young all american family"||tic9$STYPE[i]=="Young and rising"||tic9$STYPE[i]=="Young,low educated",1,0)
  tic9$DSTYPE[i] <- ifelse (tic9$STYPE[i]=="Etnically diverse"||tic9$STYPE[i]=="Mixed seniors"||tic9$STYPE[i]=="Mixed apartment dwellers"||tic9$STYPE[i]=="Own home elderly"||tic9$STYPE[i]=="Residential elderly"||tic9$STYPE[i]=="Students in apartments"||tic9$STYPE[i]=="Single youth"||tic9$STYPE[i]=="Seniors in apartments",1,0)
  tic9$ESTYPE[i] <- ifelse (tic9$STYPE[i]=="Fresh masters in the city"||tic9$STYPE[i]=="Suburban youth"||tic9$STYPE[i]=="Young urban have-nots",1,0)
}

tic9$ASTYPE <- as.factor(tic9$ASTYPE)
tic9$BSTYPE <- as.factor(tic9$BSTYPE)
tic9$CSTYPE <- as.factor(tic9$CSTYPE)
tic9$DSTYPE <- as.factor(tic9$DSTYPE)
tic9$ESTYPE <- as.factor(tic9$ESTYPE)

#Training the random forest model on the new dataset with aditional variables(Groups of STYPE)
set.seed(1234)
ticm9.new.rf <- randomForest(CARAVAN ~ ., sampsize=ssize, mtry=10, data=tic9[,-1], proximity=TRUE)
ticm9.new.rf$importance
print(ticm9.new.rf)
#Comparing the confusion matrix of both the random forest that is before and after the grouping of STYPE variable
ticm9.new.rf$confusion
ticm9.rf$confusion
print(ticm9.rf)
#From the above confusion matrix, we can clearly see that the number of actual predicted insurance purchasers which we are concerned about has increased at the cost of accuracy.
#Accuracy of model without grouping of STYPE : 93.8%
#Accuracy of model with grouping of STYPE : 82.65%
#e)
#So, the final model is with grouping,mtry=10 and sampsize for n0 as 400.
#Incoporating the new groups of STYPE (5 new variables) in the tic0 and tic1 data, followed by training the random forest model as per the optimal sampsize and mtry value found from the previous sub-question.
#Adding the new variables in tic1 and tic0 data
for (i in 1: nrow(tic1)){
  tic1$ASTYPE[i] <- ifelse(tic1$STYPE[i]=="Affluent senior apartments"||tic1$STYPE[i]=="Dinki's (double income no kids)"||tic1$STYPE[i]=="Large family farms"||tic1$STYPE[i]=="Porchless seniors: no front yard"||tic1$STYPE[i]=="Very Important Provincials"||tic1$STYPE[i]=="Young seniors in the city",1,0)
  tic1$BSTYPE[i] <- ifelse (tic1$STYPE[i]=="Affluent young families"||tic1$STYPE[i]=="Career and childcare"||tic1$STYPE[i]=="High Income"||tic1$STYPE[i]=="expensive child",1,0)
  tic1$CSTYPE[i] <- ifelse (tic1$STYPE[i]=="Couples with teens 'Married with children'"||tic1$STYPE[i]=="Family starters"||tic1$STYPE[i]=="High status seniors"||tic1$STYPE[i]=="Large family, employed child"||tic1$STYPE[i]=="Large religous families"||tic1$STYPE[i]=="Low income catholics"||tic1$STYPE[i]=="Lower class large families"||tic1$STYPE[i]=="Middle class families"||tic1$STYPE[i]=="Mixed rurals"||tic1$STYPE[i]=="Mixed small town dwellers"||tic1$STYPE[i]=="Modern, complete families"||tic1$STYPE[i]=="Religious elderly singles"||tic1$STYPE[i]=="Stable family"||tic1$STYPE[i]=="Traditional families"||tic1$STYPE[i]=="Village families"||tic1$STYPE[i]=="Young all american family"||tic1$STYPE[i]=="Young and rising"||tic1$STYPE[i]=="Young,low educated",1,0)
  tic1$DSTYPE[i] <- ifelse (tic1$STYPE[i]=="Etnically diverse"||tic1$STYPE[i]=="Mixed seniors"||tic1$STYPE[i]=="Mixed apartment dwellers"||tic1$STYPE[i]=="Own home elderly"||tic1$STYPE[i]=="Residential elderly"||tic1$STYPE[i]=="Students in apartments"||tic1$STYPE[i]=="Single youth"||tic1$STYPE[i]=="Seniors in apartments",1,0)
  tic1$ESTYPE[i] <- ifelse (tic1$STYPE[i]=="Fresh masters in the city"||tic1$STYPE[i]=="Suburban youth"||tic1$STYPE[i]=="Young urban have-nots",1,0)
}
tic1$ASTYPE <- as.factor(tic1$ASTYPE)
tic1$BSTYPE <- as.factor(tic1$BSTYPE)
tic1$CSTYPE <- as.factor(tic1$CSTYPE)
tic1$DSTYPE <- as.factor(tic1$DSTYPE)
tic1$ESTYPE <- as.factor(tic1$ESTYPE)

for (i in 1: nrow(tic0)){
  tic0$ASTYPE[i] <- ifelse(tic0$STYPE[i]=="Affluent senior apartments"||tic0$STYPE[i]=="Dinki's (double income no kids)"||tic0$STYPE[i]=="Large family farms"||tic0$STYPE[i]=="Porchless seniors: no front yard"||tic0$STYPE[i]=="Very Important Provincials"||tic0$STYPE[i]=="Young seniors in the city",1,0)
  tic0$BSTYPE[i] <- ifelse (tic0$STYPE[i]=="Affluent young families"||tic0$STYPE[i]=="Career and childcare"||tic0$STYPE[i]=="High Income"||tic0$STYPE[i]=="expensive child",1,0)
  tic0$CSTYPE[i] <- ifelse (tic0$STYPE[i]=="Couples with teens 'Married with children'"||tic0$STYPE[i]=="Family starters"||tic0$STYPE[i]=="High status seniors"||tic0$STYPE[i]=="Large family, employed child"||tic0$STYPE[i]=="Large religous families"||tic0$STYPE[i]=="Low income catholics"||tic0$STYPE[i]=="Lower class large families"||tic0$STYPE[i]=="Middle class families"||tic0$STYPE[i]=="Mixed rurals"||tic0$STYPE[i]=="Mixed small town dwellers"||tic0$STYPE[i]=="Modern, complete families"||tic0$STYPE[i]=="Religious elderly singles"||tic0$STYPE[i]=="Stable family"||tic0$STYPE[i]=="Traditional families"||tic0$STYPE[i]=="Village families"||tic0$STYPE[i]=="Young all american family"||tic0$STYPE[i]=="Young and rising"||tic0$STYPE[i]=="Young,low educated",1,0)
  tic0$DSTYPE[i] <- ifelse (tic0$STYPE[i]=="Etnically diverse"||tic0$STYPE[i]=="Mixed seniors"||tic0$STYPE[i]=="Mixed apartment dwellers"||tic0$STYPE[i]=="Own home elderly"||tic0$STYPE[i]=="Residential elderly"||tic0$STYPE[i]=="Students in apartments"||tic0$STYPE[i]=="Single youth"||tic0$STYPE[i]=="Seniors in apartments",1,0)
  tic0$ESTYPE[i] <- ifelse (tic0$STYPE[i]=="Fresh masters in the city"||tic0$STYPE[i]=="Suburban youth"||tic0$STYPE[i]=="Young urban have-nots",1,0)
} 
tic0$ASTYPE <- as.factor(tic0$ASTYPE)
tic0$BSTYPE <- as.factor(tic0$BSTYPE)
tic0$CSTYPE <- as.factor(tic0$CSTYPE)
tic0$DSTYPE <- as.factor(tic0$DSTYPE)
tic0$ESTYPE <- as.factor(tic0$ESTYPE)  

#Training the new random forest model based on the mtry, sampsize and the new variables
randForTic_3 <- randomForest(CARAVAN~.,data = tic0,mtry=10,sampsize=c(400,348),proximity =TRUE,importance=TRUE)
randForTic_3$confusion
randForTic_3$importance
print(randForTic_3)

#Predicting the values of tic1 response using this model
pred5 <- predict(randForTic_3,tic1,type="class")
confusionMatrix(pred5,tic1$CARAVAN)
#Confusion matrix on the predicted data for tic1
tab10 <- table(tic1[,86])
ssize1 <- as.vector(rep(tab10[2],2))
set.seed(1234)
randForTic_2 <- randomForest(CARAVAN~.,data = tic1,mtry=10,sampsize=ssize1,proximity =TRUE,importance=TRUE)
InsuranceBuy_2 <- as.data.frame(randForTic_2$votes)
mostlikely1000_2 <- head(InsuranceBuy_2[order(InsuranceBuy_2$insurance,decreasing = TRUE),],1000)
#Code to choose all the 1000 individuals who,based on the model, seem 
#most likely to buy policies.
n_2 <- tic1[row.names(mostlikely1000_2),]
table(n_2$CARAVAN)
#we find that 127 hold insurance out of the most likely 1000.
