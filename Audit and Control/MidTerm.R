library(readr)
library(lubridate)
library(benford.analysis)
library(sqldf)
#Importing files
shipments <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/shipments.csv", 
                      col_types = cols(shipment_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
inventory_replenishments <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/inventory_replenishments.csv")
inventory_1_1_2017 <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/inventory_1_1_2017.csv")
daily_total_AR_collections <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/daily_total_AR_collections.csv")
customer_credit_limits <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/customer_credit_limits.csv")
AR_confirm_dif_amt <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/AR_confirm_dif_amt.csv")
AR_confirm_no_problem <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/AR_confirm_no_problem.csv")
AR_confirm_no_sale <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/AR_confirm_no_sale.csv")
AR_confirm_not_paid <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/AR_confirm_not_paid.csv")
AR_OS_ALL <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/AR_OS.csv", 
                  col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
cash_sales_ALL <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/cash_sales.csv", 
                       col_types = cols(sales_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
credit_sales_ALL <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/credit_sales.csv", 
                         col_types = cols(sales_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
sku_info_all <- read_csv("C:/Users/shrey/Desktop/Spring 18/A&C/Midterm/missing_information.csv", 
                     col_types = cols(`date AR collected` = col_date(format = "%Y-%m-%d"), 
                                      `sales date` = col_date(format = "%Y-%m-%d"), 
                                      `ship date` = col_date(format = "%Y-%m-%d")))
AR_OS <- subset(AR_OS_ALL,year(ymd_hms(AR_OS_ALL$date))=="2017")
cash_sales <- subset(cash_sales_ALL,year(ymd_hms(cash_sales_ALL$sales_date))=="2017")
credit_sales <- subset(credit_sales_ALL,year(ymd_hms(credit_sales_ALL$sales_date))=="2017")
sku_info_all <- na.omit(sku_info_all)
sku_info <- subset(sku_info_all,year(ymd(sku_info_all$`ship date`))=="2017" & year(ymd(sku_info_all$`sales date`))=="2017")
#Combining the date columns into 1 single variable
daily_total_AR_collections$date <- as.Date(with(daily_total_AR_collections,paste(year, month, day,sep="-")), "%Y-%m-%d")
inventory_replenishments$date <- as.Date(with(inventory_replenishments,paste(year, month, day,sep="-")), "%Y-%m-%d")

#Checking for the missing data
apply(AR_confirm_dif_amt,2,function(x) sum(is.na(x)))
apply(AR_confirm_no_problem,2,function(x) sum(is.na(x)))
apply(AR_confirm_no_sale,2,function(x) sum(is.na(x)))
apply(AR_confirm_not_paid,2,function(x) sum(is.na(x)))
apply(AR_OS,2,function(x) sum(is.na(x)))
apply(cash_sales,2,function(x) sum(is.na(x)))
apply(credit_sales,2,function(x) sum(is.na(x)))
apply(shipments,2,function(x) sum(is.na(x)))
apply(inventory_replenishments,2,function(x) sum(is.na(x)))
apply(inventory_1_1_2017,2,function(x) sum(is.na(x)))
apply(daily_total_AR_collections,2,function(x) sum(is.na(x)))
apply(customer_credit_limits,2,function(x) sum(is.na(x)))

#Question 1
#Totals for outstanding accounts receivable
sum(AR_OS$AR_outstanding_amount)
#Totals for inventory at cost
sum(inventory_1_1_2017$cost * inventory_1_1_2017$inventory_balance)
#Sales at retail
sum(sum(cash_sales$amount),sum(credit_sales$amount))
#cost of goods sold
sum(sku_info$quantity*sku_info$cost)
#Total of cash
sum(cash_sales$amount)
#Trial Balance Amount
sum(sum(cash_sales$amount),sum(credit_sales$amount)) - (sum(AR_OS$AR_outstanding_amount) + sum(inventory_1_1_2017$cost * inventory_1_1_2017$inventory_balance) + sum(sku_info$quantity*sku_info$cost) + 5553546.00)
#It seems to me that the trialbalance is not 0.

#Question 2
ben.obj_cashSales <- benford(cash_sales$amount)
plot(ben.obj_cashSales)
fraud_transactions_cashSales <- getSuspects(ben.obj_cashSales,cash_sales,how.many = 2)
fraud_transactions_cashSales

ben.obj_creditSales <- benford(credit_sales$amount)
plot(ben.obj_creditSales)
fraud_transactions_creditSales <- getSuspects(ben.obj_creditSales,credit_sales,how.many = 2)
fraud_transactions_creditSales

ben.obj_invReplenish <- benford(inventory_replenishments$replenishment_quantity)
plot(ben.obj_invReplenish)
fraud_transactions_invReplenish <- getSuspects(ben.obj_invReplenish,inventory_replenishments,how.many = 2)
fraud_transactions_invReplenish

#The above analysis shows some anomalies. Howvever, we need to concrete our evidence by performing more tests.
#We can perform t-test to supoort our evidence. Due to control Weakness, we are not able to perform t-test as we do not have 2 variables to compare.
#The above tests are peformed in the substantive planning phase in the Test of Balances.

#Question3
print("Total of 'Amount'")
sum(AR_OS$AR_outstanding_amount)
#1.
print("Positive 'Amount' entries in the file")
length(which(AR_OS$AR_outstanding_amount > 0))
print("Total of Positive 'Amount' entries in the file")
sum(AR_OS[which(AR_OS$AR_outstanding_amount > 0),5])
#2.
print("Negative 'Amount' entries in the file")
which(AR_OS$AR_outstanding_amount < 0)
#sum(AR_OS[which(AR_OS$AR_outstanding_amount < 0),5])
#3.
print("Range of 'Amount' in the file")
range(AR_OS$AR_outstanding_amount)
#4.
print("Zero 'Amount' entries in the file")
length(which(AR_OS$AR_outstanding_amount == 0))

#Question4
#1.
which(duplicated(cash_sales))
which(duplicated(credit_sales))
which(duplicated(inventory_replenishments))
which(duplicated(sku_info))
#2.
#No Duplicates.If there were any, client should reconcile the files which the invoices and check if the transaction is legit or not
#3.
# In the Testing phases of the audit we would report these Duplicates or Omissions.

#Question5
#1.
collectable <- as.data.frame(AR_OS[which(difftime(as.Date(as.character("2017-12-31 23:59:59"),format = "%Y-%m-%d %H:%M:%S"),as.Date(as.character(AR_OS$date),format = "%Y-%m-%d %H:%M:%S"),units="days")<45),])
print("Sum of collectable amount")
sum(collectable$AR_outstanding_amount)
#2
uncollectable_records_45_120 <- as.data.frame(AR_OS[which(difftime(as.Date(as.character("2017-12-31 23:59:59"),format = "%Y-%m-%d %H:%M:%S"),as.Date(as.character(AR_OS$date),format = "%Y-%m-%d %H:%M:%S"),units="days")<=120 & difftime(as.Date(as.character("2017-12-31 23:59:59"),format = "%Y-%m-%d %H:%M:%S"),as.Date(as.character(AR_OS$date),format = "%Y-%m-%d %H:%M:%S"),units="days")>=45),])
uncollectable_records_45_120
uncollectable_amount <- sum(uncollectable_records_45_120$AR_outstanding_amount*0.01)
print("Sum of uncollectable amount which are older than 45 days and less than 120 days ")
uncollectable_amount
#3.
uncollectable_records_120 <-  as.data.frame(AR_OS[which(difftime(as.Date(as.character("2017-12-31 23:59:59"),format = "%Y-%m-%d %H:%M:%S"),as.Date(as.character(AR_OS$date),format = "%Y-%m-%d %H:%M:%S"),units="days")>120),])
nrow(uncollectable_records_120)
uncollectable_amount <- sum(sum(uncollectable_records_120$AR_outstanding_amount)*0.05)
print("Total of uncollectable amount")
uncollectable_amount

#Question6
#1.
cust_grps <- sqldf("select customer_no,sum(AR_outstanding_amount) as total_amount from AR_OS group by customer_no")
head(cust_grps)
#2.
Ar_creditLimit <- merge(cust_grps,customer_credit_limits,by="customer_no",all.x = TRUE)
Ar_creditLimit$X1 <- NULL
Ar_creditLimit[is.na(Ar_creditLimit)] <- 0
#3.
Ar_creditLimit$credit_limit_Exceed <- ifelse(Ar_creditLimit$total_amount > Ar_creditLimit$credit_limit,"Yes","No")
print("Number of customer who have exceeded their limits")
nrow(Ar_creditLimit[Ar_creditLimit$credit_limit_Exceed=="Yes",])
print("(Not all)Customer who have exceeded their limits")
head(Ar_creditLimit)
#4.
#The customers who have exceeded their credit limit are the ones for which there are no credit limits.
#A credit limit should be set for these customers

#Question7
ar_shipping <- merge(AR_OS,shipments,by="invoice")
ar_ship_2018 <- ar_shipping[(which(ymd_hms(ar_shipping$shipment_date) > ymd_hms("2017-12-31 23:59:59"))),c(1,3,5,7,8)]
print("Number of AR which have been shipped after the year end")
nrow(ar_ship_2018)
print("(Not all )ARs which have been shipped after the year end")
head(ar_ship_2018)

#Question8
sample_AR <- head(AR_OS,n=1000)
dif_amt_AR_confirm <- merge(sample_AR, AR_confirm_dif_amt,by="invoice", all.x=TRUE)
no_sale_AR_confirm <- merge(sample_AR, AR_confirm_no_sale,by="invoice", all.x=TRUE)
no_sale_AR_confirm <- na.omit(no_sale_AR_confirm)
not_paid_AR_confirm <- merge(sample_AR, AR_confirm_not_paid,by="invoice", all.x=TRUE)
not_paid_AR_confirm <- na.omit(not_paid_AR_confirm)
no_problem_AR_confirm <- merge(sample_AR, AR_confirm_no_problem,by="invoice", all.x=TRUE)
dif_amt_AR_confirm <- na.omit(dif_amt_AR_confirm)
#1.
t.test(no_problem_AR_confirm$AR_outstanding_amount,no_problem_AR_confirm$amount)
#The above does show some anomalies as the mean of the amounts differ by significant values.
#This may be because of typo error while entering the amounts - We need to confirm this with the sales team.
#2.
t.test(no_sale_AR_confirm$AR_outstanding_amount,no_sale_AR_confirm$amount)
#No anomalies at all for the chosen sample
#Need to confirm this with the respective teams.
#3.
t.test(not_paid_AR_confirm$AR_outstanding_amount,not_paid_AR_confirm$amount)
#No anomalies at all for the chosen sample
#Need to confirm this with the respective teams.
#4.
t.test(dif_amt_AR_confirm$AR_outstanding_amount,dif_amt_AR_confirm$claimed_amount,var.equal = TRUE)
#A slight difference in the means of the amount, Need to check the records where the amount does not match.
#To calculate the error amount
n1 <- which(no_problem_AR_confirm$AR_outstanding_amount != no_problem_AR_confirm$amount)
Total_mishap <- abs(sum(no_problem_AR_confirm[n1,8])-sum(no_problem_AR_confirm[n1,5])) 
n2 <- which(no_sale_AR_confirm$AR_outstanding_amount != no_sale_AR_confirm$amount)
Total_mishap <- Total_mishap + abs(sum(no_sale_AR_confirm[n2,8]) - sum(no_sale_AR_confirm[n2,5]))
n3 <- which(not_paid_AR_confirm$AR_outstanding_amount != not_paid_AR_confirm$amount)
Total_mishap <- Total_mishap + abs(sum(not_paid_AR_confirm[n3,8]) - sum(not_paid_AR_confirm[n3,5]))
n4 <- which(dif_amt_AR_confirm$AR_outstanding_amount != dif_amt_AR_confirm$claimed_amount)
Total_mishap <- Total_mishap + abs(sum(dif_amt_AR_confirm[n4,9]) - sum(dif_amt_AR_confirm[n4,5]))
print("Total error for the selected sample is:")
Total_mishap
#This amount is below the "Materiality" or "Tolerable Error" amount set for auditing Accounts Receivable which is $10,000.

#Question9
ben.obj_AR <- benford(AR_OS$AR_outstanding_amount)
plot(ben.obj_AR)
AR_anomalies <- getSuspects(ben.obj_AR,AR_OS,how.many = 2)
AR_anomalies
#We could have performed t-test to compare the amounts,However, due to control weakness in the data we have been constrained to do so.
t.test(AR_OS$AR_outstanding_amount,credit_sales$amount)
#There is a difference in the means of the amount, Need to check the records where the amount does not match.
#These test would be the part of Substantive planning phase in the test of balances.
#The documentation requirements of SAS no. 99,, requiring documentation supporting compliance with substantially all the major requirements of the standard.

#Question10
#1.
inventory_1_1_2017$Amount_cost <- (inventory_1_1_2017$cost*inventory_1_1_2017$inventory_balance)
length(which(inventory_1_1_2017$Amount_cost > 0))
sum(inventory_1_1_2017[which(inventory_1_1_2017$Amount_cost > 0),6])
#2.
length(which(inventory_1_1_2017$Amount_cost < 0))
#sum(inventory_1_1_2017[which(inventory_1_1_2017$Amount_cost < 0),6])
#3.
range(inventory_1_1_2017$Amount_cost)
#4.
length(which(inventory_1_1_2017$Amount_cost == 0))

#Question11
print("SKU's LOCM")
for (i in (1:nrow(inventory_1_1_2017))){
  if (inventory_1_1_2017$sales_price[i] < (inventory_1_1_2017$cost[i]*1.2)){
          print(inventory_1_1_2017$SKU[i])
  }
}

#Question12
sku_count <- sqldf("select SKU,count(*) as count from sku_info group by SKU")
sku_count
sku_info$Total_cost <- (sku_info$quantity*sku_info$cost)
sku_info$Total_Sale <- (sku_info$quantity*sku_info$`sales price`)
#To check the anomalies
ben.obj_sku_info_cost <- benford(sku_info$Total_cost )
plot(ben.obj_sku_info_cost)
fraud_transactions_sku_info_cost <- getSuspects(ben.obj_sku_info_cost,sku_info,how.many = 2)
fraud_transactions_sku_info_cost
sum(sku_info$Total_cost)
ben.obj_sku_info_sale <- benford(sku_info$Total_Sale)
plot(ben.obj_sku_info_sale)
fraud_transactions_sku_info_sale <- getSuspects(ben.obj_sku_info_sale,sku_info,how.many = 2)
fraud_transactions_sku_info_sale
#We can also use t-test to check the variatiions of the amounts in inventory files and the replenishment and sales files.
#Not enough data to identify the anomalies in the inventory accounting(Its a Control Weakness).
#We would report this in the Substantive testing phase test of balances.