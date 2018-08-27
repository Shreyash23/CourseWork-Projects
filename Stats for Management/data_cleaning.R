#Research Question
#What are the factors that drive the salary of jobs offered in New York City?

setwd("C:/Users/shrey/Downloads")
nyc <- read.csv("NYC_Jobs1.csv",header = TRUE)
#install.packages("sqldf")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("reshape")
library("dplyr")
library("lubridate")
library("reshape")
require('gridExtra')
library("sqldf")
#########################################################################################################################################################################
##Cleaning the data
#########################################################################################################################################################################



#Adding an AverageSalary Column
nyc$Salary.Range.From <- as.character(nyc$Salary.Range.From)
nyc$Salary.Range.From <- as.numeric(nyc$Salary.Range.From)
AverageSalary <- (nyc$Salary.Range.From + nyc$Salary.Range.To)/2
nyc <- cbind(nyc,AverageSalary)


for(i in 1:nrow(nyc)){
  if (nyc$Salary.Frequency[i]=="Hourly"){
    nyc$AverageSalary[i]<- nyc$AverageSalary[i]*7*250
  }
  
  if (nyc$Salary.Frequency[i]=="Daily"){
    nyc$AverageSalary[i]<- nyc$AverageSalary[i]*250
  }
}


nyc$sqrt_averageSalary <- sqrt(nyc$AverageSalary)
#To convert 250 days, 7hrs daily, 35 hrs/week, 5 days/week  total working days
#If hourly/daily - then convert the salary range from and salary range to Annual payment
#If salary frequency is an integer then salary average is replaced by salary frequency
#for hourly payments : payment *7*250
#for Daily payments : payment *250



t.test(nyc$AverageSalary[nyc$Posting.Type=="Internal"],alternative="greater",mu=mean(nyc$AverageSalary[nyc$Posting.Type=="External"]))
nyc %>% group_by() %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])
#Delete the duplicate rows

nyc$Posting.Type<-NULL
nyc<-sqldf('SELECT DISTINCT* FROM nyc ')

nyc %>% group_by() %>%  summarise(Mean = mean(AverageSalary), median = median(AverageSalary), sd = sd(AverageSalary),Min_Salary = range(AverageSalary)[1], Max_Salary = range(AverageSalary)[2])


#Changing the type of all the dates from factors to date format(yyyy-mm-dd)
nyc$Posting.Date <- as.character(nyc$Posting.Date)
nyc$Posting.Date <- substr(nyc$Posting.Date, 1, 10)
nyc$Posting.Date <- ymd(nyc$Posting.Date)
class(nyc$Posting.Date)

nyc$Posting.Updated <- as.character(nyc$Posting.Updated)
nyc$Posting.Updated <- substr(nyc$Posting.Updated, 1, 10)
nyc$Posting.Updated <- ymd(nyc$Posting.Updated)
class(nyc$Posting.Updated)

nyc$Process.Date <- as.character(nyc$Process.Date)
nyc$Process.Date <- substr(nyc$Process.Date, 1, 10)
nyc$Process.Date <- ymd(nyc$Process.Date)
class(nyc$Process.Date)

nyc$Post.Until <- as.character(nyc$Post.Until)
nyc$Post.Until <- substr(nyc$Post.Until, 1, 10)
nyc$Post.Until <- ymd(nyc$Post.Until)
class(nyc$Post.Until)


#To create a column to segregate the Business Title to Executive and Not-Executive
ExecJob <-  ifelse(grepl("director|executive",nyc$Business.Title, ignore.case=T), "Executive", "Not Executive")
nyc <- cbind(nyc,ExecJob)

#To create a Column to get the degree requirements for a particular job
#for Example: Bachelor : Yes or No, Masters: Yes or No,Matriculation :  Yes or No.
Bachelors <- ifelse(grepl("Bachelors|Bachelor's|BA|BS|Baccalaureate|baccalaureate", nyc$Minimum.Qual.Requirements, ignore.case=T), "Yes", "No")
nyc <- cbind(nyc,Bachelors)
Masters <- ifelse(grepl("Masters|Master's|Graduate", nyc$Minimum.Qual.Requirements, ignore.case=T), "Yes", "No")
nyc <- cbind(nyc,Masters)

for(i in 1:nrow(nyc)){
if(nyc$Masters[i]=="Yes"){
   nyc$Bachelors[i]<-"Yes"
   }
 }

#Creating Degree Column
for(i in 1:nrow(nyc)){
  if (nyc$Bachelors[i]=="No"){
    nyc$Degree[i]<- "None"
  }
  
  if (nyc$Bachelors[i]=="Yes" & nyc$Masters[i]=="No"){
    nyc$Degree[i]<- "Bachelors"
  }
  
  if(nyc$Masters[i]=="Yes"){nyc$Degree[i]<-"Masters"}
}

#To create a column with NYC residency requirement.
nyc$NYC_Residency_Required <- ifelse(grepl("New York City Residency is not required for this position|NYCHA has no residency requirements|City Residency is not required|NYC Residency is not required|No Residency Requirement", nyc$Residency.Requirement, ignore.case=T), "No", "Yes")

#To create columns with job.category.
nyc$Engineering_Architecture <- ifelse(grepl("Engineering|Architecture, & Planning", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Finance <- ifelse(grepl("Finance", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Accounting <- ifelse(grepl("Accounting|Procurement", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Telecommunications <- ifelse(grepl("Telecommunications", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Information.Technology <- ifelse(grepl("Information|Technology", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Legal <- ifelse(grepl("Legal", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Maintenance.Operations <- ifelse(grepl("Maintenance|Operations", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Public.Safety <- ifelse(grepl("Public|Safety|Inspections|Enforcement|Social|Services|Community", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$clerical.admin <- ifelse(grepl("Clerical|Administrative|Support", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Human.Resources <- ifelse(grepl("Human Resources", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$BusinessServices <- ifelse(grepl("Business Services", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Health <- ifelse(grepl("Health", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Data.Innovation <- ifelse(grepl("Data|Innovation", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Policy.Analysis <- ifelse(grepl("Policy & Analysis", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Research.Analysis <- ifelse(grepl("Research & Analysis", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Communications <- ifelse(grepl("Communications", nyc$Job.Category, ignore.case=T), "Yes", "No")
nyc$Government <- ifelse(grepl("Inter|Governmental", nyc$Job.Category, ignore.case=T), "Yes", "No")

###converting these columns from character to factors.
nyc$Engineering_Architecture <- as.factor(nyc$Engineering_Architecture)
nyc$Finance <- as.factor(nyc$Finance)
nyc$Accounting <- as.factor(nyc$Accounting)
nyc$Telecommunications <- as.factor(nyc$Telecommunications)
nyc$Information.Technology <- as.factor(nyc$Information.Technology)
nyc$Legal <- as.factor(nyc$Legal)
nyc$Maintenance.Operations <- as.factor(nyc$Maintenance.Operations)
nyc$Public.Service <- as.factor(nyc$Public.Service)
nyc$clerical.admin <- as.factor(nyc$clerical.admin)
nyc$Human.Resources <- as.factor(nyc$Human.Resources)
nyc$BusinessServices <- as.factor(nyc$BusinessServices)
nyc$Health <- as.factor(nyc$Health)
nyc$Data.Innovation <- as.factor(nyc$Data.Innovation)
nyc$Policy.Analysis <- as.factor(nyc$Policy.Analysis)
nyc$Research.Analysis <- as.factor(nyc$Research.Analysis)
nyc$Communications <- as.factor(nyc$Communications)
nyc$Government <- as.factor(nyc$Government)
nyc$Masters <- as.factor(nyc$Masters)
nyc$Bachelors <- as.factor(nyc$Bachelors)
nyc$ExecJob <- as.factor(nyc$ExecJob)
nyc$Level <- as.factor(nyc$Level)
nyc$NYC_Residency_Required <- as.factor(nyc$NYC_Residency_Required)


#Creating a Seasons Columns
getSeason <- function(DATES) {
  #WS <- 12		#as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  #SE <- 3      #as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  #SS <- 6		#as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  #FE <- 9     #as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  d<-day(DATES)
  m<-month(DATES)
  
  # Convert dates from any year to dates
  #d <- month(as.Date(strftime(DATES, format="%y-%m-%d")))
  #1 = Spring
  #2 = Summer
  #3 = Fall
  #4 = Winter
  ifelse (((m==12 && d>=15  ) || m==1 || m==2 || (d<15 && m==3)), "Winter",
          ifelse ((( m==3 && d>=15 ) ||m==4|| m==5|| (d<15 && m==6)), "Spring",
                  ifelse (((m==6 && d>=15 ) || m==7 ||m==8 || (m==9 && d<15  )), "Summer", "Fall")))
}
for(i in 1:nrow(nyc)){
  nyc$season[i] <- getSeason(nyc$Posting.Date[i])
}
nyc$season <- as.factor(nyc$season)

#COnverting yes and no to 1 and 0 respectively.
#nyc$Engineering_Architecture <-ifelse(nyc$Engineering_Architecture=="Yes",1,0)
#nyc$Finance <-ifelse(nyc$Finance=="Yes",1,0)
#nyc$Accounting <-ifelse(nyc$Accounting=="Yes",1,0)
#nyc$Telecommunications <-ifelse(nyc$Telecommunications=="Yes",1,0)
#nyc$Information.Technology <-ifelse(nyc$Information.Technology=="Yes",1,0)
#nyc$Legal <-ifelse(nyc$Legal=="Yes",1,0)
#nyc$Maintenance.Operations <-ifelse(nyc$Maintenance.Operations=="Yes",1,0)
#nyc$Public.Service <-ifelse(nyc$Public.Service=="Yes",1,0)
#nyc$clerical.admin <-ifelse(nyc$clerical.admin=="Yes",1,0)
#nyc$Human.Resources <-ifelse(nyc$Human.Resources=="Yes",1,0)
#nyc$BusinessServices <-ifelse(nyc$BusinessServices=="Yes",1,0)
#nyc$Health <-ifelse(nyc$Health=="Yes",1,0)
#nyc$Data.Innovation <-ifelse(nyc$Data.Innovation=="Yes",1,0)
#nyc$Policy.Analysis <-ifelse(nyc$Policy.Analysis=="Yes",1,0)
#nyc$Research.Analysis <-ifelse(nyc$Research.Analysis=="Yes",1,0)
#nyc$Communications <-ifelse(nyc$Communications=="Yes",1,0)
#nyc$Government <-ifelse(nyc$Government=="Yes",1,0)

#Converting the factor attributes to character(not job categories).
# nyc$Work.Location <- as.character(nyc$Work.Location)
# nyc$Division.Work.Unit <- as.character(nyc$Division.Work.Unit)
# nyc$Job.Description <- as.character(nyc$Job.Description)
# nyc$Preferred.Skills <- as.character(nyc$Preferred.Skills)
# nyc$Work.Location.1 <- as.character(nyc$Work.Location.1)
# nyc$Job.ID <- as.character(nyc$Job.ID)
# nyc$Business.Title <- as.character(nyc$Business.Title)
# nyc$Civil.Service.Title <- as.character(nyc$Civil.Service.Title)
# nyc$Title.Code.No <- as.character(nyc$Title.Code.No)

#Dropping Redundant columns
#nyc$Salary.Range.From <- NULL
#nyc$Salary.Range.To <- NULL
#nyc$Job.Description <- NULL
#nyc$Minimum.Qual.Requirements <- NULL
#nyc$Preferred.Skills <- NULL
#nyc$Additional.Information <- NULL
#nyc$To.Apply<-NULL
#nyc$Residency.Requirement <- NULL
#nyc$Business.Title <- NULL
#nyc$Civil.Service.Title <- NULL
#nyc$Hours.Shift <-NULL
#nyc$Recruitment.Contact <- NULL
#nyc$Job.Category <- NULL
#nyc$Work.Location <- NULL
#nyc$Work.Location.1 <- NULL
#nyc$Bachelors<-NULL

#creating a table with only the average salary and job categories column.
Sal_category <- nyc[c(29,35:50)]
Sal_category <- melt(Sal_category,id.vars = c("sqrt_averageSalary"))
Sal_category <- subset(Sal_category,value=='Yes')
names(Sal_category)[2] = "JobCategory"

write.csv(nyc,"Projectv3.csv")