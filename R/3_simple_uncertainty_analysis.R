#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: USGS flow rating analysis 
# Coder: Sam Zipper and Nate Jones 
# Date: 5/29/2023
# Purpose: Explore categorical flow measurement ratings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup Environment ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#Load packages of interest
library(tidyverse) #join the cult
library(patchwork) #combine multiple plot objects
library(dataRetrieval) #download USGS data
library(parallel) #parallel processing

#Define dir of interest
output_dir <- "docs/"
data_dir   <- "data/"

#Load data
gages <- read_csv(paste0(data_dir, "gagesII.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Initially developed by Sam: https://github.com/samzipper/AIMS/blob/main/code/LowFlowAnalysis_GetGoodDaysFunction.R

#Min good function
min_good_fun <- function(gage_id, start_date = "1979-01-01", end_date = "2018-12-30"){
  # gage_id = USGS site number in "########"
  # start_date = first date in "YYYY-MM-DD" format
  # end_date = last date in "YYYY-MM-DD" format

    # read discharge data
  pCodes = c("00060") # discharge = 00060, stage = 00065
  daily_raw <- 
    dataRetrieval::readNWISdv(siteNumbers = gage_id, 
                              parameterCd = pCodes,
                              startDate = start_date,
                              endDate = end_date,
                              statCd = "00003")
  
  # get rating curve
  sw_meas <- 
    dataRetrieval::readNWISmeas(siteNumbers = gage_id) |> 
    subset(!is.na(measured_rating_diff)) |>
    subset(discharge_va > 0) |> 
    subset(measured_rating_diff %in% c("Good", "Excellent"))
  
  # find minimum good/excellent measurement
  min_good_q <- sw_meas$discharge_va[which.min(sw_meas$discharge_va)]
  
  # get percent of days with flow less than min_good_q
  prc_days_good <- sum(daily_raw$X_00060_00003 < min_good_q)/length(daily_raw$X_00060_00003)
  
  # make data frame to output
  df_out <- data.frame(
    gage_id,
    minGoodQ_cfs = min_good_q,
    daysSubGood_prc = prc_days_good)
  
  return(df_out)
  
}

#min fair function
min_fair_fun <- function(gage_id, start_date = "1979-01-01", end_date = "2018-12-30"){
  # gage_id = USGS site number in "########"
  # start_date = first date in "YYYY-MM-DD" format
  # end_date = last date in "YYYY-MM-DD" format
  
  # read discharge data
  pCodes = c("00060") # discharge = 00060, stage = 00065
  daily_raw <- 
    dataRetrieval::readNWISdv(siteNumbers = gage_id, 
                              parameterCd = pCodes,
                              startDate = start_date,
                              endDate = end_date,
                              statCd = "00003")
  
  # get rating curve
  sw_meas <- 
    dataRetrieval::readNWISmeas(siteNumbers = gage_id) |> 
    subset(!is.na(measured_rating_diff)) |>
    subset(discharge_va > 0) |>
    subset(discharge_va < max(daily_raw$X_00060_00003)) |>
    subset(measured_rating_diff %in% c("Poor","Fair"))
  
  # find minimum good/excellent measurement
  min_fair_q <- sw_meas$discharge_va[which.min(sw_meas$discharge_va)]
  
  # get percent of days with flow less than min_good_q
  prc_days_fair <- sum(daily_raw$X_00060_00003 < min_fair_q)/length(daily_raw$X_00060_00003)
  
  # make data frame to output
  df_out <- data.frame(
    gage_id,
    minFairQ_cfs = min_fair_q,
    daysSubFair_prc = prc_days_fair)
  
  return(df_out)
  
}

#min threshold function
min_threshold_fun <- function(gage_id, threshold, start_date = "1979-01-01", end_date = "2018-12-30"){
  # gage_id = USGS site number in "########"
  # start_date = first date in "YYYY-MM-DD" format
  # end_date = last date in "YYYY-MM-DD" format
  
  # read discharge data
  pCodes = c("00060") # discharge = 00060, stage = 00065
  daily_raw <- 
    dataRetrieval::readNWISdv(siteNumbers = gage_id, 
                              parameterCd = pCodes,
                              startDate = start_date,
                              endDate = end_date,
                              statCd = "00003")
  
  # get percent of days with flow less than min_good_q
  prc_days_fair <- sum(daily_raw$X_00060_00003 < threshold)/length(daily_raw$X_00060_00003)
  
  # make data frame to output
  df_out <- data.frame(
    gage_id,
    threshold,
    daysSubThreshold_prc = prc_days_fair)
  
  return(df_out)
  
}

#testing
fun <- function(gage_id){
  #run min good and max fair test
  output <- left_join(
    min_good_fun(gage_id),
    min_fair_fun(gage_id))
  
  #Estimate mean threshold
  threshold <- (output$minGoodQ_cfs  + output$minFairQ_cfs)/2
  
  #complete thre
  output <- left_join(
    output, 
    min_threshold_fun(gage_id, threshold)
  )
  
  #Export output
  output
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Apply Functin in Parallel --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#See Nate's tutorial: https://bamaecohydro.github.io/Methods/fun.html

#Create wrapper function 
error_fun<-function(n){
  tryCatch(
    expr = fun(gages$STAID[n]), 
    error = function(e)
      tibble(
        gage_id = gages$STAID[n],
        minGoodQ_cfs = NA,
        daysSubGood_prc = NA, 
        minFairQ_cfs = NA, 
        daysSubFair_prc = NA,
        threshold = NA,
        daysSubThreshold_prc = NA)
    )
}  

#Start timer
t0<-Sys.time()

#Determine number of processing cores available on your machine
n.cores<-detectCores()-1

#Create clusters
cl<-makeCluster(n.cores)

#Send libraries to cluster
clusterEvalQ(cl, {
  library(dataRetrieval)
  library(tidyverse)
  library(lubridate)
})

#Export data to cluter environments
clusterExport(cl, c("min_good_fun","min_fair_fun","min_threshold_fun", "fun", "gages"))

#Now run function
output<-parLapply(
  cl=cl,
  seq(1, nrow(gages)),
  error_fun)

#Now, bind rows from list output
output<-output %>% bind_rows()

#Stop the clusters
stopCluster(cl)

#End Time
tf<-Sys.time()
tf-t0 

#Export results ----------------
df<-output %>% drop_na()
write.csv(df, "data/Sub_MinGoodFairQ.csv")


