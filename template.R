# This is a simple template. You can add steps as you want

###########################################
# Config parameters
###########################################
# set working directory to ~/package/R/FlightDelay
# << fill code >>
setwd("~/package/R/FlightDelay")

###########################################
# library loading and predefined functions
###########################################
# Hint: you can use library rpart and rpart.plot to build decision tree model
# rpart manual: https://cran.r-project.org/web/packages/rpart/rpart.pdf
# Load libraries 
# << fill code >>
library(gtools) # for binsearch
library(rpart)


# Predefined functions
evaluation <- function(predicted_results,actual_results){
  evalCtree <- table(predicted_results,actual_results)
  print(evalCtree)
  print(paste("ACC : ",((evalCtree[1] + evalCtree[4]) / nrow(test_data))*100," %",sep=""))
  prec <- evalCtree[4] / (evalCtree[2] + evalCtree[4])
  recall <- evalCtree[4] / (evalCtree[3] + evalCtree[4])
  f1 <- 2*prec*recall/(prec+recall)
  print(paste("Prec : ",prec,sep=""))
  print(paste("Recall : ",recall,sep=""))
  print(paste("F1 : ",f1,sep=""))
}

# Additional predefined functions (optional)
# << fill code >>

totalFlightArriveBefore <- function(data,flight,periodLength){
  count <- 0
  for(i in 1:ncol(data)){
    row <- data[i,]
    if(row$CRS_ARR_TIME>=((arrival_time-15+2400)%%2400) 
       && row$CRS_ARR_TIME<flight$CRS_ARR_TIME 
       && row$DAY_OF_MONTH == flight$DAY_OF_MONTH){
      count = count + 1
    }
  }
}

lowerBound <- function(x,lo,hi,vec){
  while(lo<hi){
    mid = as.integer((lo+hi)/2);
    if(x>vec[mid]) lo = mid+1
    else hi = mid
  }
  return(lo)
}
upperBound <- function(x,lo,hi,vec){
  while(lo<hi){
    mid = as.integer((lo+hi)/2);
    if(x>=vec[mid]) lo = mid+1
    else hi = mid
  }
  return(lo)
}

setUniqueID <- function(data){
  id <- 1:nrow(data)
  data <- cbind(id=id, data)
  return(data)
}
getListOfDestDF <- function(data){
  dest = data$DEST_AIRPORT_ID;
  dest = unique(dest)
  dest_list <- list()
  str(data)
  for(i in 1:length(dest)){
    dest_list[[i]] <- data[data$DEST_AIRPORT_ID == dest[i],c('DAY_OF_MONTH','CRS_ARR_TIME','id')]
  }
  return(dest_list)
}
getArriveFrequency = function(data){
  arrivalTimes = data[,c('DAY_OF_MONTH','CRS_ARR_TIME','id')]
  arrivalTimes = arrivalTimes[ order(arrivalTimes[,1],arrivalTimes[,2]) ,]
  arrivalTimesInFloat = arrivalTimes$DAY_OF_MONTH + arrivalTimes$CRS_ARR_TIME/2400.0 
  arrivalTimesID = arrivalTimes$id;
  
  vvv <- vector(mode="numeric", length=0)
  for(i in 1:length(arrivalTimesInFloat)){
    l <- lowerBound(arrivalTimesInFloat[i]-(periodLength/2400.0),1,length(arrivalTimesInFloat),arrivalTimesInFloat)
    h <- upperBound(arrivalTimesInFloat[i],1,length(arrivalTimesInFloat),arrivalTimesInFloat)
    #print(i)
    vvv[i] <- h-l
  }
  d <- data.frame(freq = vvv,arrivalTimesID)
  return(d)
}

addFreqFeature <- function(target_df){
  target_df = setUniqueID(target_df)
  
  
  
  dest_list <- getListOfDestDF(target_df)
  
  df <- getArriveFrequency(dest_list[[1]])
  for(i in 2:length(dest_list)){
    cat(i ," of ",length(dest_list), "\n")
    df <- rbind(df,getArriveFrequency(dest_list[[i]]))
  }
  
  # merge output df together
  df <- rename(df,c("arrivalTimesID"="id"))
  df <- df[ order(df$id),]
  target_df <- cbind(arriveFreq=df$freq,target_df)
  return(target_df)
} 
###########################################
# Main
###########################################
#--------------------------------------
# Step1: read data
#--------------------------------------
# read train data from "2015_01_flight_delay.csv" in working directory and show first 6 examples
# << fill code >>
train_data <- read.csv("2015_01_flight_delay.csv");
head(train_set,6)

# read test data from "test_flight_delay.csv" in working directory and show first 6 examples
# << fill code >>
test_data <- read.csv("2015_02_flight_delay.csv");
head(test_set,6)

#--------------------------------------
# Step2: remove rows that have NA
#--------------------------------------
# remove rows that have NA and show number of remaining examples
# << fill code >>
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

print(paste("number of training data:",nrow(train_data))) #number of training data: 457013
print(paste("number of testing data:",nrow(test_data)))   #number of testing data: 407543

#--------------------------------------
# Step3: add your own features
#--------------------------------------
# add at least 1 new features
# << fill code >>

# add unique id 

periodLength = 15 # in minutes
train_data <- addFreqFeature(train_data)
test_data <- addFreqFeature(test_data)


#dest = train_data$DEST_AIRPORT_ID;
#v <- vector(mode="numeric", length=0)
#dest = unique(dest)
#c <- 0;
#dest_list <- list()
#for(i in 1:length(dest)){
#  c = c + nrow(train_data[train_data$DEST_AIRPORT_ID == dest[i],c('DAY_OF_MONTH','CRS_ARR_TIME')])
#  dest_list = list(dest_list,train_data[train_data$DEST_AIRPORT_ID == dest[i],c('DAY_OF_MONTH','CRS_ARR_TIME')])
#}

 


#--------------------------------------
# Step4: remove unused column and change column's data type
#--------------------------------------
# << fill code >>
removeFeature <- function(data){
  data$MONTH <- NULL
  data$YEAR <- NULL
  data$CANCELLED <- NULL
  return(data)
}
train_data <- removeFeature(train_data)
test_data <- removeFeature(test_data)

#--------------------------------------
# Step5: Build model
#--------------------------------------
# build model and see time used for building model
start.time <- Sys.time()

# << fill code >>
fit <- rpart(ARR_DEL15~.,data=train_data,method="class",control=rpart.control(minsplit=1, minbucket=1, cp=0.00001))
plot(fit)
text(fit)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("time used:",time.taken))

# visualion the model (if it's possible) (optional)
# << fill code >>

#--------------------------------------
# Step6: testing
#--------------------------------------
# predict whether testing data will delay or not
# << fill code >>
#
result <- predict(fit,newdata = test_data, type = "class")
table(result)
evaluation(result,test_data$ARR_DEL15)
#  evaluation function in part "library loading and predefined functions"