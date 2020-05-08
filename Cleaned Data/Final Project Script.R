#load necessary libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(TeachingDemos)
library(tm)

#PART 1: DATA CLEANING

#reads in the dataset
drugs<- as.data.frame(read_xlsx("C:/Users/17082/Downloads/Drug WAC Database.xlsx"))
view(drugs)
#removes unnecessary columns and NAs
drugs<-(drugs[,-c(11:26)])
drugs<-drop_na(drugs)
drugs<-drugs[,-c(3,6,7,8,11)]
#restricts data only to single source products with active patents
drugs<-subset(drugs,drugs$`Drug Source Type`=="Single Source Drug")
drugs$`Patent Expiration Date`<-ymd(drugs$`Patent Expiration Date`)
drugs<-subset(drugs,drugs$`Patent Expiration Date`>="2020-5-3")

#isolates the unique product code from the middle of the OSHPD ID
product.code<-as.numeric(drugs$NDC)
product.code<-as.data.frame(digits(product.code,simplify=TRUE))
product.code<-product.code[c(6:9),]
drugs$newID<-paste(product.code[1,],product.code[2,],product.code[3,],product.code[4,])
space<-' '
remove<-''
drugs$newID<-sub(space,remove,drugs$newID)
drugs$newID<-sub(space,remove,drugs$newID)
drugs$newID<-sub(space,remove,drugs$newID)
drugs$newID<-as.numeric(drugs$newID)
drugs$`OSHPD ID`<-drugs$newID
drugs$`OSHPD ID`<-format(drugs$`OSHPD ID`,digits=4)

#removes duplicates from the dataset
drugs$duplicate<-duplicated(drugs$`OSHPD ID`)
drugs<-subset(drugs,drugs$duplicate==FALSE)
drugs$drug.name<-word(drugs$`Drug Product Description`)
drugs$duplicate<-duplicated(drugs$drug.name)
drugs<-subset(drugs,drugs$duplicate==FALSE)
drugs$drug.name<-removeNumbers(drugs$drug.name)
drugs<-subset(drugs, duplicate==FALSE)

#there are now 126 unique, patent-protected drugs in our dataset

#PART 2: Sample selection
#okay so I had to do just about all of this manually  
#because I waited too long to request API keys and didn't
#get them in time.
#I know scraping for this data was kind of the point 
#and I tried my best I promise but I'm so tired I'm sorry

#Starting with the drugs dataset, I gathered the average
#price and target condition from GoodRx for each drug

#I then compared the target conditions with the Lancet's
#mortality database, and removed any non-lethal conditions

#Next, I gathered the net income of each company from MacroTrends,
# except for a couple of privately held companies that did not have
#data available on those sites. I used annual reports from those companies
#There was only one country with negative net income, and it was removed

#I now have 36 drugs left in the sample:
drugs.1<-as.data.frame(read_xlsx("C:/Users/17082/Downloads/Drug Database 1.xlsx", sheet=2))

#using the clinical trial database, I assign 1 to drugs
#that have clinical testing the showed an improvement
#in overall survivability. This essentially removed
#any drug meant to mitigate a disease instead of cure it

#the sample is now restricted to 5 drugs which meet both
#criteria for inclusion in the analysis (elective pricing
# and a clear relationship between dollars and lives)

drugs.2<-as.data.frame(read_xlsx("C:/Users/17082/Downloads/Drug Database 1.xlsx", sheet=3))
view(drugs.2)


#STEP 3: Feature engineering


#returning to the clinical trials, I create two new variables:
#"treated mortality" is the reported mortality rate of those treated with the drug
#"untreated mortality" is the mortality rate of the control group, which
#received the next best treatment available

drugs.2$Untreated<- c(.01107,.0394, .081, .15, .20)
drugs.2$Treated<- c(.008972, .0354, .063, .085, .17)

#From the NIH National Center for Health Statistcs,
#I gather the estimated number of people afflicted 
#by the target condition in the US

drugs.2$afflicted<- c(2200000, 2200000,58000, 250000, 6500000)



#Mortality Differential is the average number of additional people
#who would survive if everyone afflicted with the target condition
#received the patented treatment

drugs.2$Diff<- (drugs.2$afflicted*drugs.2$Untreated)-(drugs.2$afflicted*drugs.2$Treated)


#Forgone Profits is equal to
#afflicted*average price. It reflects how much money a company
#woulld lose if they were to hand out a full course of
#their product for free to every afflicted person in America

drugs.2$forgone.profits<- drugs.2$afflicted*drugs.2$`Avg US Price`

#note that each value for forgone.profits is lower than the
#net income for each respective company
