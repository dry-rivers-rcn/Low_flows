#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: USGS flow rating analysis 
# Coder: Sam Zipper and Nate Jones 
# Date: 10/12/2022
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

#Define output dir
output_dir<-"docs/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Initially developed by Sam
get_prc_subGood <- function(gage_id, start_date, end_date){
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
  df_out <- data.frame(minGoodQ_cfs = min_good_q,
                       daysSubGood_prc = prc_days_good)
  
  return(df_out)
  
}