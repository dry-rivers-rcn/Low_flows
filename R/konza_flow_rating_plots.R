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
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(patchwork)

#Define dir of interest
output_dir <- "docs/"

## download data
pCodes = c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "06879650"  # USGS 06879650 KINGS C NR MANHATTAN, KS
daily_raw <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = pCodes,
                            statCd = "00003") # daily mean

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Characterize flow below "good" measurement threshold -----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examine 
sw_meas <- readNWISmeas(siteNumbers = USGS_gage)
sw_meas$discharge_cfs_cut <- cut(sw_meas$discharge_va, c(0, 0.1, 1, 5, 10, 100, 10000), include.lowest = F)
sw_meas$measured_rating_diff <- factor(sw_meas$measured_rating_diff, levels = c("Poor", "Fair", "Good", "Excellent"))


# subset to water years only
daily_raw$WaterYear <- year(daily_raw$Date + days(92))
daily_raw$WYDOY <- yday(daily_raw$Date + days(92))
colnames(daily_raw)[colnames(daily_raw)=="X_00060_00003"] <- "discharge_cfs"

daily <- 
  daily_raw %>% 
  subset(WaterYear >= 1980 & WaterYear <= 2021)

# set "low flow is impossible" threshold
q_thres <- 0.22 # cfs

daily$toolow <- daily$discharge_cfs < q_thres

# sum to annual
annual <-
  daily %>% 
  group_by(WaterYear) %>% 
  summarize(toolow_days = sum(toolow),
            total_days = sum(is.finite(discharge_cfs)),
            toolow_prc = toolow_days/total_days,
            discharge_cfs_total = sum(discharge_cfs)*86400,
            discharge_cfs_toolow = sum(discharge_cfs[toolow])*86400,
            discharge_toolow_prc = discharge_cfs_toolow/discharge_cfs_total)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Characterize flow below "good" measurement threshold -----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual measurements
p_measurements <-
  sw_meas %>% 
  subset(!is.na(measured_rating_diff)) %>%
  subset(discharge_va > 0) %>% 
  ggplot(aes(x = discharge_cfs_cut, fill = measured_rating_diff)) +
  geom_bar() +
  scale_x_discrete(name = "Discharge [cfs]", 
                   labels = c("< 0.1", "0.1 - 1", "1 - 5", "5 - 10", "10 - 100", "> 100")) +
  scale_y_continuous(name = "Number of\nMeasurements",
                     expand = expansion(c(0, 0.025))) +
  scale_fill_viridis_d(name = "Measurement Rating") +
  theme(legend.position = "bottom")

p_ts_days <-
  ggplot(annual, aes(x = WaterYear, y = toolow_prc)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_y_continuous(name = "Percent of Days with\nDischarge < Lowest\nGood Measurement",
                     labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(name = "Water Year")

p_combo <-
  (p_measurements + p_ts_days) +
  plot_layout(ncol = 1) +
  plot_annotation(
    #title = "USGS 06879650, Kings Creek near Manhattan KS",
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")")

ggsave(paste0(output_dir, "konza.png"), width = 6, height = 4, units = "in")  


