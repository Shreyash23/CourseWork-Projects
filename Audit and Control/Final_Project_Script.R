library(dplyr)
library(sqldf) 
library(plyr) 
library(readxl) 
library(stringr) 
library(lubridate) 
library(benford.analysis)
library(pwr)

setwd('C:/Users/shrey/Desktop/Spring 18/A&C/Final Project')
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

# To remove the irrelevant data, keeping the data of the year 2017 only.
AR_2017.12.31$date <- as.Date(as.character(AR_2017.12.31$date), format = "%Y-%m-%d")
AR_2017.12.31_2017 <- subset(AR_2017.12.31,year(ymd(AR_2017.12.31$date)) == "2017")

AR_Confirm_OK$date <- as.Date(as.character(AR_Confirm_OK$date), format = "%Y-%m-%d")
AR_Confirm_OK <- subset(AR_Confirm_OK,year(ymd(AR_Confirm_OK$date)) == "2017")

AR_Customer.Claims.Incorrect.billing$date <- as.Date(as.character(AR_Customer.Claims.Incorrect.billing$date), format = "%Y-%m-%d")
AR_Customer.Claims.Incorrect.billing <- subset(AR_Customer.Claims.Incorrect.billing,year(ymd(AR_Customer.Claims.Incorrect.billing$date)) == "2017")

AR_Customer.Claims.Not.received$date <- as.Date(as.character(AR_Customer.Claims.Not.received$date), format = "%Y-%m-%d")
AR_Customer.Claims.Not.received <- subset(AR_Customer.Claims.Not.received,year(ymd(AR_Customer.Claims.Not.received$date)) == "2017")

AR_Customer.Claims.Unrecorded.Allowance.on.price$date <- as.Date(as.character(AR_Customer.Claims.Unrecorded.Allowance.on.price$date), format = "%Y-%m-%d")
AR_Customer.Claims.Unrecorded.Allowance.on.price <- subset(AR_Customer.Claims.Unrecorded.Allowance.on.price,year(ymd(AR_Customer.Claims.Unrecorded.Allowance.on.price$date)) == "2017")

AR_Customer.Refused.delivery$date <- as.Date(as.character(AR_Customer.Refused.delivery$date), format = "%Y-%m-%d")
AR_Customer.Refused.delivery <- subset(AR_Customer.Refused.delivery,year(ymd(AR_Customer.Refused.delivery$date)) == "2017")

AR_Return...Failed.Quality.Control$date <- as.Date(as.character(AR_Return...Failed.Quality.Control$date), format = "%Y-%m-%d")
AR_Return...Failed.Quality.Control <- subset(AR_Return...Failed.Quality.Control,year(ymd(AR_Return...Failed.Quality.Control$date)) == "2017")

cash_sales$sales_date <- as.Date(as.character(cash_sales$sales_date), format = "%Y-%m-%d")
cash_sales_2017 <- subset(cash_sales,year(ymd(cash_sales$sales_date)) == "2017")

credit_sales$sales_date <- as.Date(as.character(credit_sales$sales_date), format = "%Y-%m-%d")
credit_sales_2017 <- subset(credit_sales,year(ymd(credit_sales$sales_date)) == "2017")

expenses$expense_date <- as.Date(as.character(expenses$expense_date), format = "%Y-%m-%d")
expenses <- subset(expenses,year(ymd(expenses$expense_date)) == "2017")

payroll$date <- as.Date(as.character(payroll$date), format = "%Y-%m-%d")
payroll <- subset(payroll,year(ymd(payroll$date)) == "2017")

purchases$date <- as.Date(as.character(purchases$date), format = "%Y-%m-%d")
purchases <- subset(purchases,year(ymd(purchases$date)) == "2017")

shipments$shipment_date <- as.Date(as.character(shipments$shipment_date), format = "%Y-%m-%d")
shipments <- subset(shipments,year(ymd(shipments$shipment_date)) == "2017")

collections$collection_date<- as.Date(as.character(collections$collection_date), format = "%Y-%m-%d")
collections_2017 <- subset(collections,year(ymd(collections$collection_date)) == "2017")

#Checking for any missing values in the dataset:
apply(AR_2017.12.31,2,function(x) sum(is.na(x)))
apply(AR_Confirm_OK,2,function(x) sum(is.na(x)))
apply(AR_Customer.Claims.Incorrect.billing,2,function(x) sum(is.na(x)))
apply(AR_Customer.Claims.Not.received,2,function(x) sum(is.na(x)))
apply(AR_Customer.Claims.Unrecorded.Allowance.on.price,2,function(x) sum(is.na(x)))
apply(AR_Customer.Refused.delivery,2,function(x) sum(is.na(x)))
apply(AR_Return...Failed.Quality.Control,2,function(x) sum(is.na(x)))
apply(cash_sales,2,function(x) sum(is.na(x)))
apply(credit_sales,2,function(x) sum(is.na(x)))
apply(expenses,2,function(x) sum(is.na(x)))
apply(payroll,2,function(x) sum(is.na(x)))
apply(purchases,2,function(x) sum(is.na(x)))
apply(shipments,2,function(x) sum(is.na(x)))

#We observe that there are no missing values in the data set.
#Therefore, we can proceed with our analysis:

#Planning and Risk Assestment
#1.High risk accounts based on Analytical Review:

#2. 
#Suggesting sample size for internal control and substantive tests:
#WE have taken the complete dataset for the internal controls and substantive tests.This is because  considering the entire dataset would enable us to audit the accounts precisely and efficiently.
#We have used R Studio that helped us take the entire  dataset into account.

#Substantive Tests:

#Trial Balance Reconcilation:
#We have imported all the files.
#Reconciling the Trial balance:

#Checking Expenses:
expense_foot<-sum(expenses$amount)
expense_foot
#This doesnot reconcile with the stated trial balance.

#Checking sales revenue:
sales_revenue_foot<-sum(cash_sales_2017$amount)+sum(credit_sales_2017$amount)
sales_revenue_foot
#This doesnot reconcile with the stated trial balance.

#Checking Accounts receivable:
Ar_foot<-sum(AR_2017.12.31_2017$amount)-sum(AR_Customer.Claims.Not.received$original_amount)
Ar_foot
#This doesnot reconcile with the stated trial balance.

#checking payroll:
payroll_foot<-sum(as.numeric(payroll$amount))
payroll_foot
#This reconciles with the stated trial balance.

#Checking Cost of goods sold:
purchase_foot<-sum(purchases$purchase)
purchase_foot
#This doesnot reconciles with the stated trial balance.

#This doesnot reconciles with the stated trial balance.

#Checking Cash:
cash_foot<-sum(cash_sales_2017$amount)
cash_foot
#This doesnot reconciles with the stated trial balance.

#We can't compare the trial balance of Cash, Fixed Assests, Accounts payable,long term liabilities, Depreciation expense and Taxes as we don;t have enough data.
#Therefore, this is a control weakness.

#2: Looking at the transaction distribution file:
plot(density(AR_2017.12.31$amount))
plot(density(AR_Confirm_OK$original_amount))
plot(density(AR_Customer.Claims.Incorrect.billing$original_amount))
plot(density(AR_Customer.Claims.Incorrect.billing$customer_claimed_amount))
plot(density(AR_Customer.Claims.Not.received$original_amount))
plot(density(AR_Customer.Claims.Unrecorded.Allowance.on.price$original_amount))
plot(density(AR_Customer.Claims.Unrecorded.Allowance.on.price$customer_claimed_amount))
plot(density(AR_Customer.Refused.delivery$original_amount))
plot(density(AR_Return...Failed.Quality.Control$original_amount))
plot(density(AR_Return...Failed.Quality.Control$customer_claimed_amount))

#We observe that they have high number of AR at low range and low AR at high range and it is right skewed.
#Since, we have more number of ARs at low range, whihc means there is a high risk of it converting into bad debt without any actions.
#This is applicable to all the AR transaction files.

plot(density(cash_sales$amount))
#This implies that there is a high amount of cash sales at low range as compared to high range products.
plot(density(expenses$amount))
#This implies that number of transactions for staff and labor are more than supervisor.
#Since, there is a restriction on the amount of expense a staff can incur, therefore , we can say that staff and labor are comparatively very high as to number of supervisors.
par(mfrow=c(1,2))
plot(density(inventory_1_1_2017$sales_price))
plot(density(inventory_1_1_2017$cost))
#we observe that, Sales price is almost twice of the cost price.Mid range products are sold more than other ranges.
#Low range and high range products are manufactured more than other ranges.
dev.off()
plot(density(payroll$amount))
#Payroll is normally distributed, company is following the bell curve.
plot(density(purchases$purchase))
#Purchases is a right skewed data and we can observe that there is high amount of fluctuations in the data. 

#Rest of the factors are not as risky as AR amounts.


#Employee Expenditure Audit
#1.Read File at the Beginning
#Identifying any gaps or duplicates:
apply(expenses,2,function(x) sum(is.na(x)))
#No Missing values.
which(duplicated(expenses))
#No duplicate values.
#Smpling the expense data as 1% of the entire dataset: 
expenses_sample <- sample_n(expenses,round(0.01*nrow(expenses)))


#2.
#Assuming type of employee is identified by the first character of the employee number
for (i in 1:nrow(expenses_sample)){
  if (substr(expenses_sample$employee_no[i],0,1)=="X"){
    expenses_sample$Type[i] <- "X"
  }
  else if (substr(expenses_sample$employee_no[i],0,1)=="Y"){
    expenses_sample$Type[i] <- "Y"
  }
  else if (substr(expenses_sample$employee_no[i],0,1)=="Z"){
    expenses_sample$Type[i] <- "Z"
  }
}

#Creating a column to find if the transactions are greater than $500
for (i in 1: nrow(expenses_sample)){
  if(any(expenses_sample$amount[i] > 500))
  expenses_sample$GreaterThan500[i] <- "1"
  else
    expenses_sample$GreaterThan500[i] <- "0"
}
#Creating a list of employees who are abusing the authorisation control and the number of times they have abused.
abusingEmployees <- sqldf("select employee_no,count(*) as Number_of_Abuses from expenses_sample where GreaterThan500=1 group by employee_no")
#List of 10 abusing employees:
head(abusingEmployees,10)
#We observe that there are a 900 employees who exceed this regulation.
#We think this is because 50$ is less amount and they might need more for the expenses. 

#3,4,5.
#Conducting benford analysis:
ben.exp.emp <- benford(as.numeric(expenses_sample$employee_no))
ben.exp.amt <- benford(expenses_sample$amount)
#Plotting graph of predicted vs actual first digits in "amount":
plot(ben.exp.amt)
#We report that we have no suspicious findings in this dataset.
#As per the above graph, there seem to be no anomaly in the amount spent by the emloyees.

#Accounts Receivable Audit:

#6.

#WE have read all the files at the starting of the report.
#Read AR 12-31 file
#Read Credit Limits file
#Read Confirm file, taking a sample of this file as instructed in the question.
AR_Confirm_sample <- sample_n(AR_Confirm_OK,round(0.1*nrow(AR_Confirm_OK)))
#Read Shipments file.

#7
sum(AR_2017.12.31$amount)
#The trial balance matches with the sum of amount of the Outstanding 12-31 file

#8
#Number of Omissions 
which(is.na(AR_2017.12.31$invoice))
#Duplicate invoices
which(duplicated(AR_2017.12.31$invoice))
#WE have duplicate data in this file.

#Number of Omissions 
which(is.na(AR_Confirm_OK$invoice))
#Duplicate invoices
which(duplicated(AR_Confirm_OK$invoice))
#We ahve 1 duplicated value in this file.

#Number of Omissions
which(is.na(shipments$invoice))
#Duplicate invoices
which(duplicated(shipments$invoice))
#WE have multiple duplicate values.

#Number of Omissions
which(is.na(shipments$shipping_document_no))
#Duplicate Shipping numbers
which(duplicated(shipments$shipping_document_no))
#WE have multiple duplicate values.

#Number of Omissions
which(is.na(collections$invoice))
#Duplicate Shipping numbers
which(duplicated(collections$invoice))
#WE have multiple duplicate values.

#In all the data files, we have no gaps(omissions).

#9
#a.
#Calculations for invoices under 30 days old
invoiceUnder30Days <- as.data.frame(AR_2017.12.31[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(AR_2017.12.31$date),format = "%Y-%m-%d"),units="days")<30),])
#Total Amount
sum(invoiceUnder30Days$amount)
#Percentage totals
(sum(invoiceUnder30Days$amount)/sum(AR_2017.12.31$amount))*100

#b.
#Calculations for invoices between 30 and 60 days old
invoicebetween30_60Days <- as.data.frame(AR_2017.12.31[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(AR_2017.12.31$date),format = "%Y-%m-%d"),units="days")<=60 & difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(AR_2017.12.31$date),format = "%Y-%m-%d %H:%M:%S"),units="days")>=30),])
#Total Amount
sum(invoicebetween30_60Days$amount)
#Percentage totals
(sum(invoicebetween30_60Days$amount)/sum(AR_2017.12.31$amount))*100

#c.
#Calculations for invoices over 60 days old
invoiceOver60Days <- as.data.frame(AR_2017.12.31[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(AR_2017.12.31$date),format = "%Y-%m-%d"),units="days")>60),])
#Total Amount
sum(invoiceOver60Days$amount)
#Percentage totals
(sum(invoiceOver60Days$amount)/sum(AR_2017.12.31$amount))*100

#d.
invoiceOver60Days
#We can observe that all these data are of OCtober end.
#We can say that, we don't have any data prior to October 27th.

#10.
#Creating a table with customer number , total credit and credit limit
credit_customer_limit <- sqldf("select cs.customer_no as Customer,sum(cs.amount) as Total_credit,cr.credit as Credit_limit from cr_limits as cr ,credit_sales as cs where cr.customer=cs.customer_no group by cs.customer_no")
#Creating a column which indicates if the credit limit exceed for any customer
for (i in 1: nrow(credit_customer_limit)){
  if(credit_customer_limit$Total_credit[i] > credit_customer_limit$Credit_limit[i])
  credit_customer_limit$limitExceedIndicator[i] <- 1
  else
    credit_customer_limit$limitExceedIndicator[i] <- 0
}
#Number of customers whose balances is greater than their credit limits
sqldf("select count(*) as Customers_who_have_Exceeded_their_credit_Limit from credit_customer_limit where limitExceedIndicator = 1 ")
#Basically 1238 customers have exceeded their credit limits.

#11.
#Sales revenue of all the invoices including 2018
sum(cash_sales$amount)+sum(credit_sales$amount)
#Sales revenue of all the invoices of only 2017
sum(cash_sales_2017$amount)+sum(credit_sales_2017$amount)
#The sales revenue is incorrectly stated in the trial balances as 20,355,635,096
#The correct amount corresponding to 2017 invoices should be 19,196,372,247

#12.
#Basically 1238  customers have their credit balances exceeded.
#Below are 6 such customer
head(credit_customer_limit[credit_customer_limit$limitExceedIndicator==1,])
#These excess credit balance would effect the bad debt and we cannot be sure about the repayment of those credit balances by the customers.

#13.
#a.
Tolerable_error <- 10000000
pwr.t.test (n = NULL, d=(Tolerable_error)/sum(AR_Confirm_OK$original_amount), sig.level = 0.05, power = 0.8, type = "one.sample")
#We get the value of sample to be 1652

#b.
#Sampling the AR_Confirmation file:
sampled_AR_Confirms <- sample_n(AR_Confirm_OK,1652)

#14.
Sample_AR_Collection <- sqldf("select a.invoice as Invoice,b.amount as Collected_Amount,a.original_amount as Original_Amount from sampled_AR_Confirms a,collections as b where b.invoice=a.invoice")
Sample_AR_Collection$Collected_Amount <- as.numeric(Sample_AR_Collection$Collected_Amount)
Sample_AR_Collection$Original_Amount <- as.numeric(Sample_AR_Collection$Original_Amount)
#Error in collections and confirmation amounts
((sum(Sample_AR_Collection$Collected_Amount)-sum(Sample_AR_Collection$Original_Amount))/sum(Sample_AR_Collection$Original_Amount))*100
#Hence, we can conclude that the year end AR are "fairly stated" as the difference is very small almost equal to zero.

#Inventory:
#1.
which(is.na(physical_inventory_counts$sku))
which(duplicated(physical_inventory_counts$sku))
#WE found few duplicate values.
#Removing the duplicates from the dataset:
physical_inventory_counts_No_Dups<-physical_inventory_counts[-which(duplicated(physical_inventory_counts$sku)),]
which(duplicated(inventory_1_1_2017$sku))
#Removing the duplicate values from the dataset:
inventory_1_1_2017_No_Dups <- inventory_1_1_2017[-which(duplicated(inventory_1_1_2017$SKU)),]

#2
#To find the beginning inventory
for (i in 1:nrow(inventory_1_1_2017)){
  inventory_1_1_2017$Total_Cost[i] <- inventory_1_1_2017$sales_price[i]*inventory_1_1_2017$inventory_balance[i]
}
beginning_inventory <- sum(inventory_1_1_2017$Total_Cost)

#To find the year end inventory
year_end_inventory <- sqldf("select a.sku,a.physical_count,b.sales_price from physical_inventory_counts as a,inventory_1_1_2017 as b where a.sku=b.SKU")
for(i in 1:nrow(year_end_inventory)){
  year_end_inventory$total_Cost[i] <- year_end_inventory$physical_count[i]*year_end_inventory$sales_price[i]
}
ending_inventory<-sum(year_end_inventory$total_Cost)

#Checking Manufacturing Inventory:
year_end_foot<-sum(year_end_inventory$total_Cost)
year_end_foot

#To find the CGS
CGS <- sum(purchases$purchase)
CGS
#Manufactoring Inventory on trial balance.
beginning_inventory-ending_inventory
#The value does not much with the trial balance, perhaps there is a significant difference which cannot be ignored.

#3.
collections$amount <- as.numeric(collections$amount)
#Calculations for invoices under 10 days old
InventoryUnder10days <- as.data.frame(collections[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(collections$collection_date),format = "%Y-%m-%d"),units="days")<10),])
#Total Amount
InventoryUnder10days$amount <- as.numeric(InventoryUnder10days$amount)
sum(InventoryUnder10days$amount)
#Percentage totals
(sum(InventoryUnder10days$amount)/sum(collections$amount))*100

#Calculations for invoices under 30 days old
InventoryUnder30days <- as.data.frame(collections[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(collections$collection_date),format = "%Y-%m-%d"),units="days")<30),])
#Total Amount
InventoryUnder30days$amount <- as.numeric(InventoryUnder30days$amount)
InventoryUnder30days$amount <- as.numeric(InventoryUnder30days$amount)
sum(InventoryUnder30days$amount)
#Percentage totals
(sum(InventoryUnder30days$amount)/sum(collections$amount))*100

#Calculations for invoices under 100 days old
InventoryUnder100days <- as.data.frame(collections[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(collections$collection_date),format = "%Y-%m-%d"),units="days")<100),])
#Total Amount
InventoryUnder100days$amount <- as.numeric(InventoryUnder100days$amount)
sum(InventoryUnder100days$amount)
#Percentage totals
(sum(InventoryUnder100days$amount)/sum(collections$amount))*100

#Calculations for invoices over 100 days old
InventoryOver100days <- as.data.frame(collections[which(difftime(as.Date(as.character("2017-12-31"),format = "%Y-%m-%d"),as.Date(as.character(collections$collection_date),format = "%Y-%m-%d"),units="days")<100),])
#Total Amount
InventoryOver100days$amount <- as.numeric(InventoryOver100days$amount)
sum(InventoryOver100days$amount)
#Percentage totals
(sum(InventoryOver100days$amount)/sum(collections$amount))*100
#This is greater than the tolerable amount for inventory.
#Inventory over 100 days should be written down as bad debts.

#4.

inventory_turnOver <- sqldf("select sku,count(*) as Total_sales from collections group by sku")
inventory_turnOver <- sqldf("select a.sku,a.Total_sales,b.physical_count from inventory_turnOver as a,physical_inventory_counts as b where a.sku=b.sku")

for(i in 1:nrow(inventory_turnOver)){
  inventory_turnOver$turnOver[i] <- inventory_turnOver$physical_count[i]/inventory_turnOver$Total_sales[i]
}

for(i in 1:nrow(inventory_turnOver)){
  if(inventory_turnOver$turnOver[i] < 5)
    inventory_turnOver$indicator[i]=1
  else
    inventory_turnOver$indicator[i]=0
}

table(inventory_turnOver$indicator)
#We have 1609 item with turnover less than 5times and 13391 items with turnover of more than 5 times.
#This is risk because we have a low turn over for these products.
#This will affect out net income , thus overstating the net income in the trial balance.

#5.
LowerCostTest <- inventory_1_1_2017_No_Dups[,-c(1,6)]
for(i in 1:nrow(LowerCostTest)){
  LowerCostTest$Sales_cost_diff[i] <- LowerCostTest$sales_price[i]-LowerCostTest$cost[i]
}

for(i in 1:nrow(LowerCostTest)){
  if(LowerCostTest$Sales_cost_diff[i] < 0)
  LowerCostTest$sales_cost_indicator[i]<- 1
  else
    LowerCostTest$sales_cost_indicator[i]<- 0
}

table(LowerCostTest$sales_cost_indicator)

LowerCostTest <- sqldf("select a.sku,a.sales_price,a.cost,a.inventory_balance as beg_balance,a.Sales_cost_diff,a.sales_cost_indicator,b.physical_count from LowerCostTest as a,physical_inventory_counts_No_Dups as b where a.sku=b.sku")
for(i in 1:nrow(LowerCostTest)){
  LowerCostTest$Realizable_sales_cost[i] <- LowerCostTest$Sales_cost_diff[i]*LowerCostTest$physical_count[i]
}

LowerCostItems <- subset(LowerCostTest,LowerCostTest$sales_cost_indicator==1)
sum(LowerCostItems$Realizable_sales_cost)
any(abs(sum(LowerCostItems$Realizable_sales_cost)) > 10000000)
#The realizable amount is greater than the tolerable error

#5a.
nrow(LowerCostTest[LowerCostTest$cost > LowerCostTest$Sales_cost_diff,])
#yes, there are items where net realizable value is less than cost
#List of top 10 items:
head(LowerCostTest[LowerCostTest$cost > LowerCostTest$Sales_cost_diff,],10)
#This suggests that there is very less margin, thus, company will incur loss.

#5b.
nrow(LowerCostTest[LowerCostTest$Sales_cost_diff < 2.1*LowerCostTest$cost,])
a <- LowerCostTest[LowerCostTest$Sales_cost_diff < 2.1*LowerCostTest$cost,]
#List of top 10 items:
head(a,10)

#Yes, there are items where net realizable value (Sales - Cost) is less than 110% of cost.
#Therefore, only 3492 items which are earning profit of 10%.

#6.
Inventory_Shrinkage <- physical_inventory_counts_No_Dups
Inventory_Shrinkage <- sqldf("select a.sku,a.perpetual_count,a.physical_count,b.cost from Inventory_Shrinkage as a,inventory_1_1_2017_No_Dups as b where a.sku=b.sku")
for(i in 1:nrow(Inventory_Shrinkage)){
  Inventory_Shrinkage$Shrinkage[i] <- Inventory_Shrinkage$perpetual_count[i]-Inventory_Shrinkage$physical_count[i]
}
for(i in 1:nrow(Inventory_Shrinkage)){
  Inventory_Shrinkage$Shrinkage_value[i] <- Inventory_Shrinkage$Shrinkage[i]*Inventory_Shrinkage$cost[i]
}
sum(Inventory_Shrinkage$Shrinkage_value)

any(sum(Inventory_Shrinkage$Shrinkage_value) > 10000000)

#6a.
pwr.t.test (n = NULL, d=(Tolerable_error)/sum(Inventory_Shrinkage$Shrinkage_value), sig.level = 0.05, power = 0.8, type = "one.sample")
#Sample size= 161487

#7.
#We conclude that year end inventory is not fairly stated.
