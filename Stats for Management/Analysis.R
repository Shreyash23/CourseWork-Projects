#########################################################################################################################################################################
#Analysis part
#########################################################################################################################################################################
##
library(car) 
library(ggplot2)


#Univariate analysis

#For Average Salary
par(mfrow=c(1,2))
plot(density(nyc$AverageSalary),main = "Distribution of Annual Average Salary")
#To transform the distribution of average salary to normal form
plot(density(sqrt(nyc$AverageSalary)),main = "Distribution of sqrt(Annual Average Salary)")
dev.off()

nyc$sqrt_averageSalary <- sqrt(nyc$AverageSalary)

Sal_category <- nyc[c(28,35:51)]
Sal_category <- melt(Sal_category,id.vars = c("AverageSalary"))
Sal_category <- subset(Sal_category,value=='Yes')
names(Sal_category)[2] = "JobCategory"

#creating a table with only the sqrt average salary and job categories column.
Sal_category <- nyc[c(52,34:50)]
Sal_category <- melt(Sal_category,id.vars = c("sqrt_averageSalary"))
Sal_category <- subset(Sal_category,value=='Yes')
names(Sal_category)[2] = "JobCategory"
#job category
#To check the count and central tendency for each job category against Average Salary
ggplot(Sal_category, aes(factor(JobCategory),fill=Sal_category$JobCategory)) + geom_bar() + coord_flip() + ggtitle("Univariate Analysis of Job Category")
prop.table(table(Sal_category$JobCategory))*100

#ExecJobs
ggplot(nyc, aes(factor(ExecJob),fill=nyc$ExecJob)) + geom_bar() + coord_flip() + ggtitle("Univariate Analysis of Executive Jobs")
prop.table(table(nyc$ExecJob))*100

#Degree Level
ggplot(nyc, aes(factor(Degree),fill=nyc$Degree)) + geom_bar() + coord_flip() + ggtitle("Univariate Analysis of Degree Level")
prop.table(table(nyc$Degree))*100
#Seasons
ggplot(nyc, aes(factor(season),fill=nyc$season)) + geom_bar() + coord_flip() + ggtitle("Univariate Analysis of Seasons")
prop.table(table(nyc$season))*100

#To showcase that there is no difference between external,Internal and ALl job postings.
#So we don't delete the duplicates(External and Internal)
#nyc_internal <- subset(nyc, nyc$Posting.Type == "Internal")
#nyc_external <- subset(nyc, nyc$Posting.Type == "External")

#bivariate analysis
Sal_category <- nyc[c(28,34:50)]
Sal_category <- melt(Sal_category,id.vars = c("AverageSalary"))
Sal_category <- subset(Sal_category,value=='Yes')
names(Sal_category)[2] = "JobCategory"

ggplot(Sal_category, aes(factor(JobCategory), AverageSalary,fill=Sal_category$JobCategory)) + geom_boxplot() + coord_flip() +ggtitle("Bivariate Analysis of Job Category")

Sal_category %>% group_by(JobCategory) %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])

#ExecJobs
ggplot(nyc, aes(factor(ExecJob), AverageSalary,fill=nyc$ExecJob)) + geom_boxplot() + coord_flip() +ggtitle("Bivariate Analysis of Executive Jobs")

nyc %>% group_by(ExecJob) %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])

#Degree Levels
ggplot(nyc, aes(factor(Degree), AverageSalary,fill=nyc$Degree)) + geom_boxplot() + coord_flip() +ggtitle("Bivariate Analysis of Degree Levels")

nyc %>% group_by(Degree) %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])

#Season
ggplot(nyc, aes(factor(season), AverageSalary,fill=nyc$season)) + geom_boxplot() + coord_flip() +ggtitle("Bivariate Analysis of Degree Levels")

nyc %>% group_by(season) %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])

#NYC Residency criteria
ggplot(nyc, aes(factor(NYC_Residency_Required), AverageSalary,fill=nyc$NYC_Residency_Required)) + geom_boxplot() + coord_flip() +ggtitle("Bivariate Analysis of NYC Residency Criteria")

nyc %>% group_by(NYC_Residency_Required) %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])

#########Hypothesis Testing
##ExecJobs
t.test(nyc$AverageSalary[nyc$ExecJob=="Executive"],alternative="two.sided",mu=mean(nyc$AverageSalary[nyc$ExecJob=="Not Executive"]))
##Season
t.test(nyc$AverageSalary[nyc$season=="Winter"],alternative="two.sided",mu=mean(nyc$AverageSalary[nyc$season=="Summer"]))
##NYC Residency Criteria
t.test(nyc$AverageSalary[nyc$NYC_Residency_Required=="Yes"],alternative="two.sided",mu=mean(nyc$AverageSalary[nyc$NYC_Residency_Required=="No"]))
##Degree
t.test(nyc$AverageSalary[nyc$Degree=="Masters"],alternative="two.sided",mu=mean(nyc$AverageSalary[nyc$Degree=="None"]))
##Job category
t.test(Sal_category$AverageSalary[Sal_category$JobCategory=="Engineering_Architecture"],alternative="two.sided",mu=mean(Sal_category$AverageSalary[Sal_category$JobCategory=="Finance"]))

##ANOVA testing 
par(mfrow=c(2,2))
ExecJob.aov <- aov(sqrt_averageSalary~ExecJob,data = nyc)
summary(ExecJob.aov)
plot(ExecJob.aov)

Degree.aov <- aov(sqrt_averageSalary~Degree,data = nyc)
summary(Degree.aov)
plot(Degree.aov)

season.aov <- aov(sqrt_averageSalary~season,data = nyc)
summary(season.aov)
plot(season.aov)

NYC_Residency_Required.aov <- aov(sqrt_averageSalary~NYC_Residency_Required,data = nyc)
summary(NYC_Residency_Required.aov)
plot(NYC_Residency_Required.aov)

JobCategory.aov <- aov(sqrt_averageSalary~JobCategory,data = Sal_category)
summary(JobCategory.aov)
plot(JobCategory.aov)
 
##Linear regression
nycMod1 <- lm(sqrt_averageSalary~ExecJob, data = nyc) 
summary(nycMod1)

nycMod2 <- lm(sqrt_averageSalary~Degree, data = nyc)
summary(nycMod3)

nycMod3 <- lm(sqrt_averageSalary~NYC_Residency_Required, data = nyc) 
summary(nycMod4)

nycmod4 <- lm(sqrt_averageSalary~NYC_Residency_Required+ExecJob, data = nyc)
summary(nycmod4)

nycmod5 <- lm(sqrt_averageSalary~NYC_Residency_Required+ExecJob+Degree, data = nyc)
summary(nycmod5)

##OUTLIERS
tail(round(sort(cooks.distance(re_nycmod5)),2),5)
outliers <- c(620,162,619,93,596)
nyc_no_out <- nyc[-outliers,]

re_nycmod5 <- lm(sqrt_averageSalary~NYC_Residency_Required+ExecJob+Degree, data = nyc_no_out)
summary(re_nycmod5)

plot(re_nycmod5)

##VIF
vif(re_nycmod5) 
sqrt(vif(re_nycmod5)) > 2

