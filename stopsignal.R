library(psych)
# left=0,right,leftStop,rightStop
# failed stops (1-percentInhibition)
# meanGoRT (reaction time on correct Go trials) in ascending order
# Select the meanGoRT associated with the proportion of failed stops calculated in step 1
# Subtract the averageSSD from the meanGoRT selected in step 3. This is the SSRT
# 2 // stopping succeeded
# 1 // touched succeeded
# 0 // touched failed
# data$Inhibition[data$userResult == 0]<-"fail"
# data$Inhibition[data$userResult == 1]<-"go succeed"
# data$Inhibition[data$userResult == 2]<-"stop succeed"
# Note that for rounding off a 5, the IEC 60559 standard (see also ‘IEEE 754’)
# is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2. 
# However, this is dependent on OS services and on representation error (since e.g. 0.15 is not represented exactly, 
# the rounding rule applies to the represented number and not to the printed number, and so round(0.15, 1) could be either 0.1 or 0.2).



data<-read.table("~/Google Drive/stopsignal/Export_from_BevBits/4test-test-test-2-1.csv",
                 sep=",",
                 header=T)
data$type[data$trialType == 0]<-"Left Go"
data$type[data$trialType == 1]<-"Right Go"
data$type[data$trialType == 2]<-"Left Stop"
data$type[data$trialType == 3]<-"Right Stop"
head(data)

data$stopgo[data$trialType == 0]<-"Go"
data$stopgo[data$trialType == 1]<-"Go"
data$stopgo[data$trialType == 2]<-"Stop"
data$stopgo[data$trialType == 3]<-"Stop"

R1<-subset(data, data$roundId == 0)
R2<-subset(data, data$roundId == 1)
R3<-subset(data, data$roundId == 2)
dfList<-list(R1, R2,R3)

lapply(dfList, function(x){
  sum_userRe<-summary(as.factor(x$userResult))
  inhibition<-tail(sum_userRe, n=1)
  
  sum_stops<-summary(as.factor(x$stopgo))
  stops<-sum_stops[2]
  stops
  gos<-sum_stops[1]
  
  perStopSuccess<-inhibition/stops
  
  per_stop_fail<-1-perStopSuccess
  per_stop_fail
  
  index<-round(gos*per_stop_fail)
  index
  
  go_trials<-subset(x, stopgo=="Go")
  go_trials[order(go_trials$reactionTime),] 
  
  goRT<-go_trials$reactionTime[index]
  goRT
  
  
  SSD<-x$actualStopDelay[x$actualStopDelay >-1]
  length(SSD)
  SSD<-mean(SSD)
  
  SSRT<-goRT-SSD
  SSRT
  return(list(SSRT,SSD,goRT,index,per_stop_fail,stops))
})

