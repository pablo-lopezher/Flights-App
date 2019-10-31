
library(lubridate)
library(tidyverse)
library(dummies)
library(anchors)
library(neuralnet)
library(data.table)


data_prep <- function(df, n_data){
  
  train_df <- data.frame(matrix(ncol=4+n_data , nrow=nrow(df)-n_data))
  names <- c("dest","date","flights_date","week_day")
  
  for (i in 1:n_data){
    names <- c(names, paste0("flights_date_",i))
  }
  colnames(train_df) <- names
  
  train_df[,"date"] <- as.POSIXct(train_df[,"date"])
  
  for(i in (n_data+1):nrow(df)){
    
    date_flight <- df[[i,"date"]]
    train_df[i-n_data,"date"] <- date_flight
    train_df[i-n_data,"flights_date"] <- df[i,"count"]
    train_df[i-n_data,"dest"] <- df[i,"dest"]
    train_df[i-n_data,"week_day"] <- wday(date_flight[[1]])
    
    
    for (j in 1:n_data){
      
      pre_date <- df[[i-j,"date"]]
      
      if ( (as.numeric(date_flight-pre_date) ) != j){
        train_df[i-n_data, paste0("flights_date_",j)] <- 0
      }
      else {
        
        train_df[i-n_data, paste0("flights_date_",j)] <- df[i-j,"count"]
      }
      
    }
  }
  
  train_df <- dummy.data.frame(train_df, names=c("week_day"), sep="_")
  for (i in 1:7){
    train_df <- replace.value(train_df,c(paste0("week_day_",i)), 0, -1) 
  }
  
  #normalization 
  
  for ( i in 1:n_data){
    if(sd(train_df[[paste0("flights_date_",i)]])!=0) {
      train_df[[paste0("flights_date_",i)]] <- (train_df[[paste0("flights_date_",i)]] - mean(train_df[[paste0("flights_date_",i)]]))/sd(train_df[[paste0("flights_date_",i)]])
    }
  }
  return(train_df)
  
}



neural_nk <- function(df,desti){
  
  df <- df %>% filter(dest == desti)
  df <- data_prep(df,14)
  pred_df <- df %>% filter(date >= as.Date("2013-10-01"))
  train_df <- df %>% filter(date <= as.Date("2013-10-01"))
  res <- copy(pred_df)
  res$date <- ymd(as.Date(res$date))
  pred_df <- subset(pred_df,select = -c(dest,date))
  train_df <- subset(train_df,select = -c(dest,date))
  
  n <- names(train_df)
  rm <-c("flights_date")
  n <- setdiff(n,rm)
  f <- as.formula(paste('flights_date ~ ' ,paste(n,collapse='+')))
  
  nn <- neuralnet(f, data=train_df,hidden=25,rep=1)
  predict_nn <-compute(nn,pred_df)
  res <- subset(res,select = c(dest,date,flights_date))
  return(cbind(res,pred_flights= ifelse(predict_nn$net.result<0,0,round(predict_nn$net.result))))
  
}







