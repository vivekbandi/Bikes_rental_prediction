#Getting the structure of data#Remove all the objects stored
rm(list=ls())

#set the working directory
setwd("E:/documents")
getwd()

#Load the data into R
df=read.csv('bikes.csv',header=T)
 
#copy the dataframe
copy=df

#Check the  of the data
dim(df)

str(df)

#observe the top 10 rows in dataset
head(df,10)

# convert the variables into proper datatype
df$dteday=as.factor(df$dteday)
df$season=as.factor(df$season)
df$yr=as.factor(df$yr)
df$mnth=as.factor(df$mnth)
df$holiday=as.factor(df$holiday)
df$weekday=as.factor(df$weekday)
df$workingday=as.factor(df$workingday)
df$weathersit=as.factor(df$weathersit)
df$cnt=as.numeric(df$cnt)

#missing value analysis
missing_val=data.frame(apply(df,2,function(x){sum(is.na(df))})) # no missing values

#Remove duplicate elements
isTRUE(duplicated(df)) # no duplicate elements in the dataset

#drop the casual and registered columns as these columns are redundant
df=subset(df,select=-c(14,15))

# rename the categorical variable name
names(df)[14]='demand'


#EXPLORATORY DATA ANALYSIS

#VISUALIZE THE DATA USING ggplot2 LIBRARY
# DEAL WITH CATEGORICAL VARIABLES
library(ggplot2)

#Observe the relation of categorical variables with target variable
ggplot(data=df,aes(x= season,y= demand))+geom_bar(stat = "identity")
# we can observe that the demand is high in fall and low in spring

ggplot(data=df,aes(x=yr,y=demand))+geom_bar(stat = 'identity',color="blue")
# we can observe the demand increased by year but we don't have much data to conclude 
# So better to drop year variable to avoid confusion to the model

ggplot(data=df,aes(x=mnth,y=demand))+geom_bar(stat = 'identity')
# demand is higher in the months between 5-10

ggplot(data=df,aes(x=holiday,y=demand))+geom_bar(stat='identity')
# from this plot we can say that the working people are mostly using the bikes 
# so we have to park the more bikes in the public transport places in the morning 
#and in the office premises


ggplot(data=df,aes(x=weekday,y=demand))+geom_bar(stat = 'identity',color='green')
# weekday has no effect on demand so better to drop this variable

ggplot(data=df,aes(x=workingday,y=demand))+geom_bar(stat = 'identity')
# This variable resembles the holiday variable so better to drop one variable


ggplot(data=df,aes(x=weathersit,y=demand))+geom_bar(stat = 'identity')
# weathersit does have an effect on demand variable 


# DEALING WITH NUMERICAL VARIABLES

#plot the scatter plot to observe the relation between independent numerical variable and the target variable
ggplot(data=df,aes(x=temp,y=demand))+geom_point()
#we can observe there is a linear relationship with the demand variable


ggplot(data=df,aes(x=atemp,y=demand))+geom_point()
# It resembles the temp variable so better to drop one variable after checking the correlation

ggplot(data=df,aes(x=hum,y=demand))+geom_point()
# Humidity does have an effect on demand but it is not storngly correlated

ggplot(data=df,aes(x=windspeed,y=demand))+geom_point()
# Demand is constant till the windspeed reaches to 0.25 and after that demand gradually decreasing

hist(df$demand,xlab = 'demand',col='green', border='red')
#It resembles the normal distribution so we don't need to worry about target varaible

# Drop the unwanted variables from the dataset
df=subset(df,select = -c(1,2,4,7))

# seperate the variables based on datatype
df_num=subset(df,select = c(6:10))
df_cat=subset(df,select = -c(6:10))

# check correlation between independent numeric variables
num_index=sapply(df,is.numeric)
library(corrgram)
corrgram(df[,num_index],order = F,upper.panel = panel.pie,text.panel = panel.txt,main='correlation plot')
# we can observe that the correlation between temp and atemp is very high, so it's better to remove
# one variable to avoid multi-collinearity 
df=df[,-c(7)]

#chisquare test 
fac_index=sapply(df,is.factor)
fac_data=df[,fac_index]

for(i in 1:5){
  for(j in 1:5){
    print(chisq.test(table(fac_data[,i],fac_data[,j])))
  }
}
# By chisquare test there is a dependency between independent categorical variable 
# and the target variable

#Check for Auto-Correlation
acf(df['demand'], lag.max = 10)
# you can observe high correlation in the dependent variable
# So we can use the time lagged data as an independent variables 

# Create the time lag series
library(zoo)
z=zoo(df['demand'])
t_1=lag(z,-1,na.pad=TRUE)
t_2=lag(z,-2,na.pad=TRUE)
t_3=lag(z,-3,na.pad = TRUE)

# bind the data with the time lagged series
df1=cbind(t_1,t_2,t_3)
df1=data.frame(df1)
df=cbind(df,df1)
 
# delete rows having null values
delete_na = function(DF) {
  DF[!(rowSums(is.na(DF))),]
}

df=delete_na(df)

#One Hot Encoding of categorical variables
library(caret)
dummy = dummyVars(" ~ .", data=df)
newdata = data.frame(predict(dummy, newdata = df))


# Scaling of the data 
num_index=sapply(df,is.numeric)
num_data=df[,num_index]
num_data_col=names(num_data)


for(i in num_data_col){
  newdata[,i]=((newdata[,i]-min(newdata[,i]))/(max(newdata[,i])-min(newdata[,i])))
}


#Splitting of data into train and test
x=newdata[,-c(27)]
y=newdata[,c(27)]
y=data.frame(y)
names(y)[1]='demand'

train_size=0.7*nrow(newdata)
train_size=as.integer(train_size)
          
x_train=x[1:train_size,]
y_train=y[1:train_size,]


x_test=x[train_size:nrow(newdata),]
y_test=y[train_size:nrow(newdata),]

train=newdata[1:train_size,]
test=newdata[train_size:nrow(newdata),]


# Applying model to the data
#Linear Regression
library(rpart)

lm_model=lm(demand ~.,data=train)

predictions_LR = predict(lm_model,x_test)


#Model evaluation
#Root Mean Squared Error
rmse=RMSE(predictions_LR,y_test)

#Mean Absolute Error
mae =MAE(predictions_LR,y_test)








