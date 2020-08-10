#......... CAB FARE PREDICTION.....
rm(list=ls())
setwd("C:/Users/Best Lappy/Desktop/EDWISOR/Projects/CPF")

getwd()
###########LOAD LIBRARIES ##########
x = c("ggplot2", "corrgram", "DMwR", "usdm","randomForest","geosphere","rpart",'MASS','stats')
lapply(x, require, character.only = TRUE)
rm(x)

# loading datasets
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))

########Check data shape ########
str(train)
summary(train)
head(train,5)

# Changing the data types of variables
train$fare_amount = as.numeric(as.character(train$fare_amount))
train$passenger_count=round(train$passenger_count)

####### Eliminate cells with same pickup and dropoff location
train=subset(train, !(train$pickup_longitude==train$dropoff_longitude & train$pickup_latitude==train$dropoff_latitude))

####replace "0's" with NA
train[train==0] = NA

## Removing values which are not within desired range(outlier) depending upon basic understanding of dataset.

# 1.Fare amount has a negative value, which doesn't make sense. A price amount cannot be -ve. So we will remove these fields.
train[which(train$fare_amount < 0),]
nrow(train[which(train$fare_amount < 0),])
train = train[-which(train$fare_amount < 0),]

#2.Passenger_count variable
for (i in seq(4,11,by=1)){
  print(paste('passenger_count above ' ,i,nrow(train[which(train$passenger_count > i ),])))
}
# so 18 observations of passenger_count  is consistenly above from 6,7,8,9,10 passenger_counts, let's check them.
train[which(train$passenger_count > 6),]
# We will remove observations which are above 6 value because a cab cannot hold these number of passengers.
train = train[-which(train$passenger_count > 6),]

# 3.Latitudes range from -90 to 90.Longitudes range from -180 to 180.Removing which does not satisfy these ranges
print(paste('pickup_longitude above 180=',nrow(train[which(train$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(train[which(train$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(train[which(train$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(train[which(train$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(train[which(train$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(train[which(train$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(train[which(train$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(train[which(train$dropoff_latitude > 90 ),])))
#As ther is only one value in 'pickup_latitude which is greater than 90. We will remove it.
train = train[-which(train$pickup_latitude > 90),]

# Make a copy
df=train
# train=df

################Missing Value Analysis ###################
missingvalue= function(data){
  missing_value = data.frame(apply(data, 2 , function(x){sum(is.na(x))}))
  colnames(missing_value)="Missing_Value_count"
  missing_value$percentage=apply(missing_value , 1 , function(x){x/nrow(train)*100})
  missing_value = cbind(row.names(missing_value), missing_value)
  row.names(missing_value)=NULL
  colnames(missing_value)[1]="Variables"
  print(missing_value)
  
  ######plot Missing Values#######################
  library(ggplot2)
  ggplot(data = missing_value, aes(x=reorder(Variables , -percentage),y = percentage))+
    geom_bar(stat = "identity",fill = "blue")+xlab("Variables")+
    ggtitle("Missing Values") + theme_bw()
}
######Calculate Missing Values#######
missingvalue(train)

####As PAssenger_count is a categorical Variable , so we will use mode for Imputation######### 
#####calculate mode - create function ###########
mode= function(data){
  uniq=unique(data)
  as.numeric(as.character(uniq[which.max(tabulate(match(data,uniq)))]))
  #print(mode_d)
}

mode(train$passenger_count)

##############IMPUTATION#################
train$passenger_count[is.na(train$passenger_count)] = mode(train$passenger_count)
######Choose for suitable method for imputation of missing values for other variables ###########
# ####Taking a subset of data 
# #train[40,1]= 17.5  #######Data noted to compare ####Actual value
# 
# ###Mean method
# train$fare_amount[is.na(train$fare_amount)] = mean(train$fare_amount, na.rm = T)
# 
# #Mean= 15.12488
# 
# ####Median Method 
# 
# train$fare_amount[is.na(train$fare_amount)] = median(train$fare_amount, na.rm = T)
# 
# #Median= 8.5
# 
# ######KNN Method 
# 
# train = knnImputation(train, k = 5)
# #KNN= 15.90051

####Saving the data in df set ###########
df=train
#train = df
train=train[complete.cases(train[,1]),]

#As KNN is giving the value closest to Actual Value, We choose KNN for missing value imputation

library(DMwR)
train=knnImputation(train, k=5)

missingvalue(train)

#############OUTLIER ANALYSIS################
sum(train$fare_amount>500)
#As observed there are 2 observations where the fare_amount is very high. We remove them.
train = train[-which(train$fare_amount > 500 ),]

#Outlier analysis of location points will be done after feature engineering

df2=train
# train=df2
################## Feature Engineering##########################
# 1.Feature Engineering for timestamp variable
# we will derive new features from pickup_datetime variable
# new features will be year,month,day_of_week,hour
#Convert pickup_datetime from factor to date time
train$pickup_date = as.Date(as.character(train$pickup_datetime))
train$pickup_weekday = as.factor(format(train$pickup_date,"%u"))# Monday = 1
train$pickup_mnth = as.factor(format(train$pickup_date,"%m"))
train$pickup_yr = as.factor(format(train$pickup_date,"%Y"))
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$pickup_hour = as.factor(format(pickup_time,"%H"))

sum(is.na(train))# there was 1 'na' in pickup_datetime which created 5 na's in above feature engineered variables.
train = na.omit(train) # we will remove that 1 row of na's

train = subset(train,select = -c(pickup_datetime,pickup_date))

# 2.Calculate the distance travelled using longitude and latitude
library(geosphere)
train$dist= distHaversine(cbind(train$pickup_longitude, train$pickup_latitude), cbind(train$dropoff_longitude,train$dropoff_latitude))
#the output is in metres, Change it to kms
train$dist=as.numeric(train$dist)/1000

df=train
#train=df

outlier_values <- boxplot.stats(train$dist)$out  # outlier values.
boxplot(train$dist, main="dist", boxwex=0.1)

# Replace the values with NA
train[train$dist %in% outlier_values, "dist"] = NA
missingvalue(train)

#As KNN is giving the value closest to Actual Value, We choose KNN for missing value(Outliers) imputation
library(DMwR)
train=knnImputation(train, k=5)

# We will remove the variables which were used to feature engineer new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

str(train)
summary(train)

#now convert Passenger_count into factor
train$passenger_count=as.factor(train$passenger_count)


########### Feature selection ###################
numeric_index = sapply(train,is.numeric) #selecting only numeric
numeric_data = train[,numeric_index]
cnames = colnames(numeric_data)
#######Correlation Analysis########
corrgram(train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

#ANOVA for categorical variables with target numeric variable

#aov_results = aov(fare_amount ~ passenger_count * pickup_hour * pickup_weekday,data = train)
aov_results = aov(fare_amount ~ passenger_count + pickup_hour + pickup_weekday + pickup_mnth + pickup_yr,data = train)

summary(aov_results)
# pickup_weekday has p value greater than 0.05 
train = subset(train,select=-pickup_weekday)


##################### Feature Scaling ###################
#Normality check
#qqnorm(train$dist)
#histogram(train$dist)

library(car)
# dev.off()
par(mfrow=c(1,2))
qqPlot(train$dist)                             # qqPlot, it has a x values derived from gaussian distribution, if data is distributed normally then the sorted data points should lie very close to the solid reference line 
truehist(train$dist)                           # truehist() scales the counts to give an estimate of the probability density.
lines(density(train$dist))  # Left skewed      # lines() and density() functions to overlay a density plot on histogram

#Normalisation
train[,'dist'] = (train[,'dist'] - min(train[,'dist']))/
  (max(train[,'dist'] - min(train[,'dist'])))

##remove unnecessary variables####
rm(df,df2,pickup_time,i,outlier_values,aov_results,numeric_data,cnames,numeric_index,)

############### MODEL DEVELOPMENT #####################
#create sampling and divide data into train and test

set.seed(123)
train_index = sample(1:nrow(train), 0.8 * nrow(train))

train1 = train[train_index,]#do not add column if already removed
test1 = train[-train_index,]#do not add column if already removed

########### Define Mape - The error matrix to calculate the error and accuracy ################

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y*100))
}

#################Linear Regression###########################################################

lm_model = lm(fare_amount ~. , data = train1)
summary(lm_model)

predictions_LR = predict(lm_model, test1[,-1])
MAPE(test1[,1], predictions_LR)

#error 26.12016
#Accuracy 73.88

#####################Decision Tree#####################################

library(rpart)
fit = rpart(fare_amount ~. , data = train1, method = "anova", minsplit=5)

summary(fit)
predictions_DT = predict(fit, test1[,-1])

MAPE(test1[,1], predictions_DT)

#write.csv(predictions_DT, "DT_R_PRed5.csv", row.names = F)

#Error 27.75005
#Accuracy 73.25

#############Random Forest###############################################

library(randomForest)
RF_model = randomForest(fare_amount ~.  , train1, importance = TRUE, ntree=100)
RF_Predictions = predict(RF_model, test1[,-1])

MAPE(test1[,1], RF_Predictions)
importance(RF_model, type = 1)

#error 22.50844 for n=100
#accuracy = 77.50

##############KNN Implementation############################################################

library(class)
KNN_Predictions = knn(train1[, 2:6], test1[, 2:6], train1$fare_amount, k = 1)

#convert the values into numeric
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))

#Calculate MAPE
MAPE(test1[,1], KNN_Predictions)

#error 33.7978
#Accuracy = 66.21

################Predict VAlues in Test Data###############
pred_data=read.csv("test.csv", header= T)

#####create distance variable
pred_data=subset(pred_data, !(pred_data$pickup_longitude==pred_data$dropoff_longitude & pred_data$pickup_latitude==pred_data$dropoff_latitude))
pred_data[pred_data==0]= NA

missingvalue(pred_data)
# COnnvert Data into proper data types

str(pred_data)
pred_data$passenger_count=as.factor(pred_data$passenger_count)

#calculate distance

pred_data$dist= distHaversine(cbind(pred_data$pickup_longitude, pred_data$pickup_latitude), cbind(pred_data$dropoff_longitude,pred_data$dropoff_latitude))

#the output is in metres, Change it to kms
pred_data$dist=as.numeric(pred_data$dist)/1000

#Feature Engineering for timestamp variable
# we will derive new features from pickup_datetime variable
# new features will be year,month,day_of_week,hour
#Convert pickup_datetime from factor to date time
pred_data$pickup_date = as.Date(as.character(pred_data$pickup_datetime))
pred_data$pickup_weekday = as.factor(format(pred_data$pickup_date,"%u"))# Monday = 1
pred_data$pickup_mnth = as.factor(format(pred_data$pickup_date,"%m"))
pred_data$pickup_yr = as.factor(format(pred_data$pickup_date,"%Y"))
pickup_time = strptime(pred_data$pickup_datetime,"%Y-%m-%d %H:%M:%S")
pred_data$pickup_hour = as.factor(format(pickup_time,"%H"))

pred_data = subset(pred_data,select = -c(pickup_datetime,pickup_date))

# Create the target variable
pred_data$fare_amount=0
pred_data=pred_data[,c(5,6,8,9,10,11)]

#Random Forest
RF_model = randomForest(fare_amount ~.  , train, importance = TRUE, ntree=100, mtry=2)
pred_data$fare_amount = predict(RF_model, pred_data[,-6])

write.csv(pred_data, "Predicted_Data_R.csv", row.names = F)
