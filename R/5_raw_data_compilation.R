#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Raw data wrangling
# Coder: Nate Jones 
# Date: 8/4/2023
# Purpose: Gather USGS gage data as requested by journal AE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Environment --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#Load packages of interest
library(tidyverse)     #join the cult
library(lubridate)     #date format handling
library(dataRetrieval) #downloading USGS data
library(parallel)

#Read and tidy spatial data
gages <- read_csv("data/gagesII.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Gather flow data  --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to download flow
fun <- function(gage_id, start_date = "1979-01-01", end_date = "2018-12-30"){
  # gage_id = USGS site number in "########"
  # start_date = first date in "YYYY-MM-DD" format
  # end_date = last date in "YYYY-MM-DD" format
  
  #Load packages of interest
  library(tidyverse)     #join the cult
  library(lubridate)     #date format handling
  library(dataRetrieval) #downloading USGS data
  
  # read discharge data
  pCodes = c("00060") # discharge = 00060, stage = 00065
  df <- dataRetrieval::readNWISdv(
    siteNumbers = gage_id, 
    parameterCd = pCodes,
    startDate = start_date,
    endDate = end_date,
    statCd = "00003")
  
  #Tidy gages
  df <- df %>% 
    select(
      gage_id = site_no,
      date  = Date,
      q_cfs = X_00060_00003) %>% 
    as_tibble()
  
  #Add gage id
  df$gage_id <- gage_id
  
  #export data
  df 
  
}

#Create wrapper function 
error_fun <- function(n){
  tryCatch(
    expr = fun(gages$STAID[n]), 
    error = function(e)
      tibble(
        gage_id = gages$STAID[n],
        date = NA,
        q_cfs = NA)
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
clusterExport(cl, c("fun","gages"))

#Now run function
output <- parLapply(
  cl=cl,
  seq(1, nrow(gages)), 
  error_fun)

#Now, bind rows from list output
output <- output %>% bind_rows()

#Stop the clusters
stopCluster(cl)

#End time
tf<-Sys.time()
tf-t0 

#write export
write_csv(output, "data//output.csv")
