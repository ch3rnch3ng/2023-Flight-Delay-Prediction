library(dplyr)
library(zoo)
library(psych)
library(Matrix)
library(lfe)
library(stargazer)
library(lubridate)
library(broom)
library(kableExtra)
library(vtable)
library(lattice)
library(caret) 
library(readr)
library(Matrix)
library(lfe)
library(ggplot2)
library(readxl)
library(writexl)
library(stringr)
### Set up environment:
options(scipen=999, digits=4) # avoid scientific display, keep 4 digits in display
rm(list=ls()) # clear Environment
#rm(x) remove variable x from env, function is remove(), rm() for short

#####################Clean the test data(2017.06-2017.08) and merge into one dataframe#################################
test201706a<-read_csv('2017.06Test.csv')
test201706b<-readxl::read_xlsx('2017.06Weather Condition.xlsx')
test201706c<-readxl::read_xlsx('2017.06Delay Reasons.xlsx')
codetocity<-readxl::read_xlsx('Code to City.xlsx')
codetolocation<-readxl::read_xlsx('Code to Loc.xlsx')
##########2017.06 test data
# Clean the 2017.06 test data:
str(test201706a)
summary(is.na(test201706a))
summary(factor(test201706a$Cancel))
# Convert float-type column to date type
test201706a<-test201706a%>%mutate(EArrTime=as.POSIXct(EArrTime, origin = "1970-01-01", tz = "GMT"),
                                  EDepTime=as.POSIXct(EDepTime, origin = "1970-01-01", tz = "GMT"),
                                  RArrTime=as.POSIXct(RArrTime, origin = "1970-01-01", tz = "GMT"),
                                  RDepTime=as.POSIXct(RDepTime, origin = "1970-01-01", tz = "GMT"))
# Transform the Departure time to date data for merge:
test201706a<-test201706a%>%mutate(daydate=as.Date(EArrTime))
#remove the rows that with same airport ,airline code and same departure time
test201706a<- test201706a%>%distinct(across(c('DepAirport', 'AirlineCode','EDepTime')), .keep_all = TRUE)

# merge the location column into the testing data set:
test201706a<-left_join(test201706a, codetolocation, by = c("DepAirport" = '...1'))

#Clean the 2017.06 weather data:

#Remove the duplicate rows in weather data frame
test201706b <- unique(test201706b)
# Drop the weather rows contains NA
test201706b<-test201706b%>%filter(is.na(天气)==FALSE)
summary(is.na(test201706b))

# clean the test delay reason data:
# get the datedata for merge:
test201706c<-test201706c%>%mutate(daydate=as.Date(开始时间))
# change the name of columns:
test201706c<-test201706c%>%rename(特情开始时间 = 开始时间, 特情结束时间 = 结束时间)
#Drop the irrelvent column:
test201706c<-test201706c%>%select(-c('收集时间'))

# Merge the code to city dataset into weather dataset by '城市名称'：
test201706b<-left_join(test201706b, codetocity, by = c("城市" = '城市名称'))
# remove the airport code equals to na:
test201706b<-test201706b%>%filter(is.na(机场编码)==FALSE)
##### Merge the weather condition into the testing data by airport code and date
# Transform the 日期 to date data for merge:
test201706b<-test201706b%>%mutate(日期=as.Date(日期))
str(test201706a)
str(test201706b)
#merge the test data to weather data:
test201706<-left_join(test201706a, test201706b, by = c("DepAirport" = '机场编码','daydate'='日期'))

#merge the delay reason to above by airport code and date:
test201706<-left_join(test201706, test201706c, by = c("DepAirport" = '特情机场','daydate'='daydate'))
summary(is.na(test201706))
summary(factor(test201706$Cancel))
# Transform the 特情日期 to POSIXct data for merge:
test201706<-test201706%>%mutate(特情开始时间=as.POSIXct(特情开始时间),特情结束时间=as.POSIXct(特情结束时间),
                                EDepTime=as.POSIXct(EDepTime),EArrTime=as.POSIXct(EArrTime),
                                RDepTime=as.POSIXct(RDepTime),RArrTime=as.POSIXct(RArrTime))
# Change the name in order to merge:
# save the file as CSV and XLSX:
write.csv(test201706, file = "Cleaned_test201706.csv", row.names = FALSE)
write_xlsx(list(test201706=test201706), path = "Cleaned_test201706.xlsx")

#############2017.07 test data

test201707a<-read_csv('2017.07Test.csv')
test201707b<-readxl::read_xlsx('2017.07Weather Condition.xlsx')
test201707c<-readxl::read_xlsx('2017.07Delay Reasons.xlsx')

# Clean the 2017.07 test data:
str(test201707a)
summary(is.na(test201707a))
summary(factor(test201707a$Cancel))
# Convert float-type column to date type
test201707a<-test201707a%>%mutate(EArrTime=as.POSIXct(EArrTime, origin = "1970-01-01", tz = "GMT"),
                                  EDepTime=as.POSIXct(EDepTime, origin = "1970-01-01", tz = "GMT"),
                                  RArrTime=as.POSIXct(RArrTime, origin = "1970-01-01", tz = "GMT"),
                                  RDepTime=as.POSIXct(RDepTime, origin = "1970-01-01", tz = "GMT"))
# Transform the Departure time to date data for merge:
test201707a<-test201707a%>%mutate(daydate=as.Date(EArrTime))
#remove the rows that with same airport ,airline code and same departure time
test201707a<- test201707a%>%distinct(across(c('DepAirport', 'AirlineCode','EDepTime')), .keep_all = TRUE)

# merge the location column into the testing data set:
test201707a<-left_join(test201707a, codetolocation, by = c("DepAirport" = '...1'))

#Clean the 2017.06 weather data:

#Remove the duplicate rows in weather data frame
test201707b <- unique(test201707b)
# Drop the weather rows contains NA
test201707b<-test201707b%>%filter(is.na(天气)==FALSE)
# clean the test delay reason data:
# get the datedata for merge:
test201707c<-test201707c%>%mutate(daydate=as.Date(开始时间))
# change the name of columns:
test201707c<-test201707c%>%rename(特情开始时间 = 开始时间, 特情结束时间 = 结束时间)
#Drop the irrelvent column:
test201707c<-test201707c%>%select(-c('采集时间'))

# Merge the code to city dataset into weather dataset by '城市名称'：
test201707b<-left_join(test201707b, codetocity, by = c("机场" = '城市名称'))
# remove the airport code equals to na:
test201707b<-test201707b%>%filter(is.na(机场编码)==FALSE)
##### Merge the weather condition into the testing data by airport code and date
# Transform the 日期 to date data for merge:
test201707b<-test201707b%>%mutate(日期=as.Date(日期))
str(test201707a)
str(test201707b)
#merge the test data to weather data:
test201707<-left_join(test201707a, test201707b, by = c("DepAirport" = '机场编码','daydate'='日期'))

#merge the delay reason to above by airport code and date:
test201707<-left_join(test201707, test201707c, by = c("DepAirport" = '机场','daydate'='daydate'))
#Change the name to merge:
test201707<- test201707%>%rename(城市 =机场, 特情内容 = 特情)
summary(is.na(test201707))
summary(factor(test201707$Cancel))
# save the file as CSV and XLSX:
write.csv(test201707, file = "Cleaned_test201707.csv", row.names = FALSE)
write_xlsx(list(test201707=test201707 ), path = "Cleaned_test201707.xlsx")


#############2017.08 test data
test201708a<-read_csv('2017.08Test.csv')
test201708b<-readxl::read_xlsx('2017.08Weather Condition.xlsx')
test201708c<-readxl::read_xlsx('2017.08Delay Reasons.xlsx')

# Clean the 2017.08 test data:
str(test201708a)
summary(is.na(test201708a))
summary(factor(test201708a$Cancel))
# Convert float-type column to date type
test201708a<-test201708a%>%mutate(EArrTime=as.POSIXct(EArrTime, origin = "1970-01-01", tz = "GMT"),
                                  EDepTime=as.POSIXct(EDepTime, origin = "1970-01-01", tz = "GMT"),
                                  RArrTime=as.POSIXct(RArrTime, origin = "1970-01-01", tz = "GMT"),
                                  RDepTime=as.POSIXct(RDepTime, origin = "1970-01-01", tz = "GMT"))
# Transform the Departure time to date data for merge:
test201708a<-test201708a%>%mutate(daydate=as.Date(EArrTime))
#remove the rows that with same airport ,airline code and same departure time
test201708a<- test201708a%>%distinct(across(c('DepAirport', 'AirlineCode','EDepTime')), .keep_all = TRUE)

# merge the location column into the testing data set:
test201708a<-left_join(test201708a, codetolocation, by = c("DepAirport" = '...1'))

#Clean the 2017.06 weather data:

#Remove the duplicate rows in weather data frame
test201708b <- unique(test201708b)
# Drop the weather rows contains NA
test201708b<-test201708b%>%filter(is.na(天气)==FALSE)
# clean the test delay reason data:
# get the datedata for merge:
test201708c<-test201708c%>%mutate(daydate=as.Date(开始时间))
# change the name of columns:
test201708c<-test201708c%>%rename(特情开始时间 = 开始时间, 特情结束时间 = 结束时间)
#Drop the irrelvent column:
test201708c<-test201708c%>%select(-c('采集时间'))

# Merge the code to city dataset into weather dataset by '城市名称'：
test201708b<-left_join(test201708b, codetocity, by = c("城市" = '城市名称'))
# remove the airport code equals to na:
test201708b<-test201708b%>%filter(is.na(机场编码)==FALSE)
##### Merge the weather condition into the testing data by airport code and date
# Transform the 日期 to date data for merge:
test201708b<-test201708b%>%mutate(日期=as.Date(日期))
str(test201708a)
str(test201708b)
#merge the test data to weather data:
test201708<-left_join(test201708a, test201708b, by = c("DepAirport" = '机场编码','daydate'='日期'))

#merge the delay reason to above by airport code and date:
test201708<-left_join(test201708, test201708c, by = c("DepAirport" = '机场','daydate'='daydate'))
#Change the name to merge:
test201708<- test201708%>%rename(特情内容 = 特情)
summary(is.na(test201708))
summary(factor(test201708$Cancel))
# save the file as CSV and XLSX:
write.csv(test201708, file = "Cleaned_test201708.csv", row.names = FALSE)
write_xlsx(list(test201708=test201708), path = "Cleaned_test201708.xlsx")

#Bind three testing dataset:
str(test201706)
str(test201707)
str(test201708)
test201706<-read.csv('Cleaned_test201706.csv')
test201707<-read.csv('Cleaned_test201707.csv')
test201708<-read.csv('Cleaned_test201708.csv')
Cleaned_test<-bind_rows(test201706,test201707,test201708)
# Create the sign data(Delay or not)
# First compute the delay hours, Real Departure time minus Estimate Departure time
Cleaned_test<-Cleaned_test%>%mutate(Delay_hours = difftime(RDepTime, EDepTime, units = "hours"))
# If the delay hours > 1 , then we consider it as delay, create a signal colomn with 
#Cleaned_test<-Cleaned_test%>%mutate(Delay_hours=round((RDepTime-EDepTime)/3600,0))
Cleaned_test<-read_csv('Cleaned_test.csv')
Cleaned_test<-Cleaned_test%>%rename('CCity'='中文名','Longit'='经度','Lat'='维度','City'='城市','Weather'='天气',
                       'LTemp'='最低气温','HTemp'='最高气温','Special_Begin'='特情开始时间',
                       'Special_End'='特情结束时间','Special_Content'='特情内容')
# Create the column of delay or not:
# In domestic flight,delay hours large than 2 hours, we consider as a delay , and if a flight is cancelled, we consider as delay as well,(Sign=1)：
Cleaned_test<-Cleaned_test%>%mutate(Delay=ifelse(Delay_hours>=2,1,0))%>%mutate(Delay=ifelse(Sign==1,1,Delay))
summary(factor(Cleaned_test$Sign))
summary(is.na(Cleaned_test$Sign))
summary(is.na(Cleaned_test$Delay_hours))
write.csv(Cleaned_test, file = "Cleaned_test.csv", row.names = FALSE)
write_xlsx(list(Cleaned_test=Cleaned_test), path = "Cleaned_test.xlsx")

####################################Clean the training data(2015.05-2017.05) ###################################
#############train_data(2015.05-2017.05)

traina<-read_csv('2015.05-2017.05 Flight Data.csv')
trainb<-read_csv('2015.05-2017.05 Weather Condition.csv')
trainc<-readxl::read_xlsx('2015.05-2017.05 Special Condition.xlsx')
codetocity<-readxl::read_xlsx('Code to City.xlsx')
codetolocation<-readxl::read_xlsx('Code to Loc.xlsx')
# Clean the training data:
str(traina)
summary(is.na(traina))
# Convert float-type column to date type
traina<-traina%>%mutate(EArrTime=as.POSIXct(EArrTime, origin = "1970-01-01", tz = "GMT"),
                                  EDepTime=as.POSIXct(EDepTime, origin = "1970-01-01", tz = "GMT"),
                                  RArrTime=as.POSIXct(RArrTime, origin = "1970-01-01", tz = "GMT"),
                                  RDepTime=as.POSIXct(RDepTime, origin = "1970-01-01", tz = "GMT"))
# Transform the Departure time to date data for merge:
traina<-traina%>%mutate(daydate=as.Date(EArrTime))
#remove the rows that with same airport ,airline code and same departure time
traina<- traina%>%distinct(across(c('DepAirport', 'AirlineCode','EDepTime')), .keep_all = TRUE)

# merge the location column into the testing data set:
traina<-left_join(traina, codetolocation, by = c("DepAirport" = '...1'))


#Remove the duplicate rows in weather data frame
trainb <- unique(trainb)
# Drop the weather rows contains NA
trainb<-trainb%>%filter(is.na(天气)==FALSE)
# clean the test delay reason data:
# get the datedata for merge:
trainc<-trainc%>%mutate(daydate=as.Date(开始时间))
# change the name of columns:
trainc<-trainc%>%rename(特情开始时间 = 开始时间, 特情结束时间 = 结束时间)
#Drop the irrelvent column:
trainc<-trainc%>%select(-c('收集时间'))
# Transform the airport code to capital letters to merge:
trainc<-trainc%>%mutate(特情机场=toupper(特情机场))

# Merge the code to city dataset into weather dataset by '城市名称'：
trainb<-left_join(trainb, codetocity, by = c("城市" = '城市名称'))
# remove the airport code equals to na:
trainb<-trainb%>%filter(is.na(机场编码)==FALSE)
#Drop the non-value data:
trainb<-trainb%>%select(-c('...6'))

##### Merge the weather condition into the testing data by airport code and date
#merge the test data to weather data:
train<-left_join(traina, trainb, by = c("DepAirport" = '机场编码','daydate'='日期'))

#merge the delay reason to above by airport code and date:
train<-left_join(train, trainc, by = c("DepAirport" = '特情机场','daydate'='daydate'))

# Transform the 特情日期 to POSIXct data：
train<-train%>%mutate(特情开始时间=as.POSIXct(特情开始时间),特情结束时间=as.POSIXct(特情结束时间))
summary(is.na(train))

#calculate the delay hours:
train<-train%>%mutate(Delay_hours = as.numeric(difftime(RDepTime, EDepTime, units = "hours")))
# Change the Chinese name to English name in every column
train<-train%>%rename('CCity'='中文名','Longit'='经度','Lat'='维度','City'='城市','Weather'='天气',
                      'LTemp'='最低气温','HTemp'='最高气温','Special_Begin'='特情开始时间',
                      'Special_End'='特情结束时间','Special_Content'='特情内容')
# Create the column of delay or not:
# In domestic flight,delay hours large than 2 hours, we consider as a delay , and if a flight is cancelled, we consider as delay as well,(Sign=1)
train<-train%>%mutate(Delay=ifelse(Delay_hours>=2,1,0))
# save the file as CSV and XLSX:
write.csv(train, file = "Cleaned_train.csv", row.names = FALSE)
write_xlsx(list(train=train), path = "Cleaned_train.xlsx")

train<-read_csv("Cleaned_train.csv")




