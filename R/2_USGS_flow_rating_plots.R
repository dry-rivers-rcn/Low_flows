#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: USGS flow rating plots 
# Coder: Nate Jones 
# Date: 10/15/2022
# Purpose: Plots Plots Plots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup Environment ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#Load packages of interest
library(tidyverse)     #join the cult
library(lubridate)     #date format handling
library(dataRetrieval) #downloading USGS data
library(patchwork)     #combine multiple plot objects
library(scales)        #plotting
library(sf)            #spatial data

#Read and tidy spatial data
subGoodQ <- read_csv("data/SubGoodQ.csv")
gages    <- read_csv("data/gagesII.csv")
states   <- st_read("data/tl_2012_us_state.shp")  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Tidy data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Edit states shape
states <- states %>% 
  filter(
    NAME != 'Alaska', 
    NAME != 'Hawaii', 
    NAME != 'Guam', 
    NAME != 'Commonwealth of the Northern Mariana Islands', 
    NAME != 'American Samoa',
    NAME != 'Puerto Rico',
    NAME != 'United States Virgin Islands') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#Edit gages shape
gages <- gages %>% 
  #Add duration of streamflow under good readings
  left_join(., subGoodQ %>% rename(STAID = gage_id)) %>% 
  drop_na(minGoodQ_cfs) %>% 
  #Convert to simple feature
  st_as_sf(., 
         coords = c("LNG_GAGE", 'LAT_GAGE'), 
         crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#Limit gages to just areas within CONUS
gages <- gages[states,]

#Add xy coordinates
gages<-gages %>% 
  mutate(
    lat = st_coordinates(gages)[,1], 
    lon = st_coordinates(gages)[,2])

#Convert to percent
gages <- gages %>%
  mutate(
    daysSubGood_prop = daysSubGood_prc,
    daysSubGood_prc  = daysSubGood_prc*100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Gage location figure -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Crate map
map_fig <- gages %>%
  arrange(daysSubGood_prc) %>% 
  ggplot()+
    geom_sf(data = states, lwd=0.5) + 
    geom_point(
      aes(
        x=lat,
        y=lon,
        color = daysSubGood_prc, 
        size =  daysSubGood_prc, 
        alpha = daysSubGood_prc, 
        #shape= NP
        )) +
    scale_color_distiller(
      palette = 'Spectral',
      direction = -1, 
      breaks = seq(0,100,25)) +
    scale_size_continuous(
      range = c(0.5, 1.5), 
      breaks = seq(0,100,25)) +
    scale_alpha_continuous(
      range = c(0.5,.8) , 
      breaks = seq(0,100,25)) +
    #scale_shape(labels = c("Perennial", "Non-perennial")) +
    guides(
      color = guide_colorbar(
        title ="% Flow Record", 
        order = 1),
      size="none", 
      alpha = "none"
    ) +
    theme_bw() +
      theme(
        legend.position = 'right',
        legend.justification = "bottom") +
      xlab(NULL) +
      ylab(NULL) 
  
map_fig

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Characterize flow below "good" measurement threshold -----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## download data
pCodes = c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "06879650"  # USGS 06879650 KINGS C NR MANHATTAN, KS
daily_raw <-readNWISdv(
    siteNumbers = USGS_gage, 
    parameterCd = pCodes,
    statCd = "00003") # daily mean

# Examine quality of stage-discharge measurements
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

# sum to annual
tots <-
  daily  %>% 
  summarize(
    toolow_days = sum(toolow),
    total_days = sum(is.finite(discharge_cfs)),
    toolow_prc = toolow_days/total_days,
    discharge_cfs_total = sum(discharge_cfs)*86400,
    discharge_cfs_toolow = sum(discharge_cfs[toolow])*86400,
    discharge_toolow_prc = discharge_cfs_toolow/discharge_cfs_total)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Konza Plots! ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual measurements
p_measurements <-
  sw_meas %>% 
  subset(!is.na(measured_rating_diff)) %>%
  subset(discharge_va > 0) %>% 
  ggplot(aes(x = discharge_cfs_cut, fill = measured_rating_diff)) +
  geom_bar() +
  scale_x_discrete(name = "Flow [cfs]", 
                   labels = c("< 0.1", "0.1 - 1", "1 - 5", "5 - 10", "10 - 100", "> 100")) +
  scale_y_continuous(name = "Number of Flow\nMeasurements",
                     expand = expansion(c(0, 0.025))) +
  scale_fill_brewer(palette = 'Spectral') +
  theme_classic() +
  theme(
    legend.position=c(0.76,0.85), 
    legend.direction = "horizontal",
    legend.title = element_text(size=10),
    legend.text  = element_text(size=8),
  ) +
  guides(fill = guide_legend(title = NULL))
 

p_ts_days <-
  ggplot(annual, aes(x = WaterYear, y = toolow_prc)) +
  geom_line(
    color = "grey30", 
    lty=2, 
    lwd=1.2) +
  geom_point(
    pch = 21,
    fill = "red",
    col = "grey30", 
    cex = 3) +
  scale_y_continuous(
    name = "% Annual Flow\nBelow Good\nFlow Measurement", #with\nDischarge < Lowest\nGood Measurement
    labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(name = "Water Year") +
  theme_classic()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Combine plots --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Print individual plots and them put them together in powerpoint
ggsave(plot = map_fig,        file = "docs/map.png",            width = 6,   height = 3.75, units="in", dpi = 300)
ggsave(plot = b_plot,         file = "docs/b_plot.png",         width = 2.3, height = 1.75,  units="in", dpi = 300)
ggsave(plot = p_measurements, file = "docs/p_measurements.png", width = 6,   height = 1.75, units="in", dpi = 300)
ggsave(plot = p_ts_days,      file = "docs/p_ts_days.png",      width = 6,   height = 1.75, units="in", dpi = 300)

