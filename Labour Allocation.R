train<-read.csv("train - Sheet1.csv",header = TRUE)
test<-read.csv("test.csv",header=TRUE)

rf.preds<-read.csv("rf.Pred.csv",header = TRUE)
colnames(rf.preds)[1]<-"Weekday?"
rf.preds$`Weekday?`<-as.factor(rf.preds$`Weekday?`)
rf.preds$month<-as.numeric(rf.preds$month)


library(lubridate)
library(ggplot2)
library(randomForest)
library(DescTools)

train$Out.Date...Time <- as.POSIXlt(train$Out.Date...Time, TZ ="Asia/Calcutta",format="%d-%m-%y %H:%M")

train_days<- weekdays(train$Out.Date...Time)
train_date<- as.Date(train$Out.Date...Time)
train_date_00<- force_tz(as.POSIXlt(train_date,tz),tz="Asia/Calcutta")
train_time<- seconds_to_period(train$Out.Date...Time - train_date_00)

train.modified<-cbind.data.frame(train,train_date_00,train_time,train_days,deparse.level = 1)


train.uniqe.dates<-as.data.frame.Date(unique(train.modified$train_date_00))
train.daily.freq<-as.data.frame(table(train.modified$train_date_00))
colnames(train.daily.freq)<- c("Date","Number of Truck")
train.daily.freq$Date<-as.Date(train.daily.freq$Date)
train.daily.freq.day<-weekdays(train.daily.freq$Date)
train.daily.freq<-cbind.data.frame(train.daily.freq,train.daily.freq.day,deparse.level = 1)
colnames(train.daily.freq)[3]<-"Day"

train.weekday<-matrix(1,nrow(train.daily.freq),1)
for(i in 1:nrow(train.daily.freq)){  
  if(train.daily.freq[i,3]=="Sunday")train.weekday[i]<-0 
}

train.daily.freq<-cbind.data.frame(train.daily.freq,train.weekday,deparse.level = 1)
colnames(train.daily.freq)[4]<-"Weekday?"
train.daily.freq$`Weekday?`<-as.factor(train.daily.freq$`Weekday?`)
train.daily.freq$month<- month(train.daily.freq$Date)

train.monthly<-matrix(,nrow = 12,ncol=2)
for(i in 1:12){
  truck.that.month<-0
  train.monthly[i,1]<-i
  for(j in 1:nrow(train.daily.freq)){
    if(train.daily.freq[j,5]==i)truck.that.month=truck.that.month+train.daily.freq[j,2]
  }
  train.monthly[i,2]<-truck.that.month
}

colnames(train.monthly)<-c("Month","No. of Trucks")

#-----Making Time Bins
time.bins<-c(0,16200,32400,45000,59400,73800,84600)

 #for 1/2 hr time slots
#time.bins<-as.array(0,7)
#for(i in 1:48){
#  time.bins[i+1]<-time.bins[i]+1800
#}
time.bins<-as.period(time.bins)
time.bins<-seconds_to_period(time.bins)


#------Make train.half hourly schedule for 2017
b<-matrix(0,nrow=nrow(train.daily.freq),ncol=6)
for(i in 1:nrow(train.daily.freq)){
  
  #-------Make array of arrival time at ith day in train.daily.freq
  a<-0
  a<-as.period(a)
  for(j in 1:nrow(train.modified)){
    if(train.daily.freq[i,1]==train.modified[j,18]){
      a<-append(a,train.modified[j,19])}
  }
  a<-a[-1]
  
  #-------Make Schedule for whole year half hourly
  for(m in 1:6){
    for(n in 1:length(a)){
    if(a[n]>=time.bins[m] && a[n]<time.bins[m+1]){b[i,m]<-b[i,m]+1}
    }
  }
  
}

train.bin.freq<-as.data.frame(b)
colnames(train.bin.freq)<-time.bins[-7]
train.daily.freq<-cbind.data.frame(train.daily.freq,train.bin.freq)

all.dates<-read.csv("All dates 2017.csv",header = FALSE)
all.dates<-as.Date(as.array(all.dates[,1]),format = "%m-%d-%Y")

no.truck.date<-as.Date(as.array(all.dates[which(!(all.dates %in% train.daily.freq$Date))]))
no.truck.days.num.of.trucks<-array(0,length(no.truck.days))
no.truck.days.weekdays<-weekdays(no.truck.days)
no.truck.sunday<-array(1,length(no.truck.date))
for(i in 1:length(no.truck.date)){
  if(no.truck.days.weekdays[i]=="Sunday"){no.truck.sunday[i]<-0}
}
no.truck.days.month<-month(no.truck.days)
no.truck.days.daily.freq<-matrix(0,length(no.truck.days),6)
no.truck<-cbind.data.frame(no.truck.date,no.truck.days.num.of.trucks,no.truck.days.weekdays,no.truck.sunday,no.truck.days.month,no.truck.days.daily.freq)
colnames(no.truck)<-colnames(train.daily.freq)

train.daily.freq<-rbind.data.frame(train.daily.freq,no.truck)
#DOnt know why below line is not working
#colnames(rf.preds)[3:50]<-time.bins[-7]
#-----------------Compile from here
  
for(i in 1:6){
  
  rf.train<-train.daily.freq[,4:5]
  rf.label<-train.daily.freq[,5+i]
  
  set.seed(1234)
  rf<-randomForest(x = rf.train, y = rf.label , ntree = 500,importance = TRUE)
  
  rf.preds[,i+2]<-predict(rf, rf.preds[,1:2])
}

rf.preds[,3:8]<-round(rf.preds[,3:8])
colnames(rf.preds)[3:8]<-c("0S","4H 30M 0S","9H 0M 0S","12H 30M 0S","16H 30M 0S","20H 30M 0S")
rf.preds[,9:50]<-NULL