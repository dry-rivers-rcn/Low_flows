#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: USGS flow rating plots 
# Coder: Nate Jones 
# Date: 10/15/2022
# Purpose: Plots Plots Plots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Environment ----------------------------------------------------
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
# 2.0 Tidy data ------------------------------------------------------------
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
# 3.0 Gage location figure -----------------------------------------------------
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
# 4.0 Annual analysis for Konza ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.1 Download and tidy data ---------------------------------------------------
#Define download parameters  
pCodes    <- c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "06879650" # USGS 06879650 KINGS C NR MANHATTAN, KS

# Download field measurement data
sw_meas <- readNWISmeas(siteNumbers = USGS_gage) #download field measurements
sw_meas$discharge_va <- sw_meas$discharge_va*0.0283168 #convert from cfs to cms
sw_meas$discharge_cfs_cut <- cut(sw_meas$discharge_va, c(0, .005, 0.05, 0.5, 5), include.lowest = F) #create flow categorices
sw_meas$measured_rating_diff <- factor(sw_meas$measured_rating_diff, levels = c("Poor", "Fair", "Good", "Excellent")) #populate categories by acuracy code

#download flow ts data
daily_flow <-
  readNWISdv(
    siteNumbers = USGS_gage, 
    parameterCd = pCodes,
    statCd = "00003") %>%  # daily mean
  rename(
    date  = Date, 
    Q_cfs = X_00060_00003) %>% 
  mutate(
    Q_cms = Q_cfs*0.0283168
  ) %>% 
  select(date, Q_cms)

#4.2 Plot of accuracy codes across flow ----------------------------------------
# Manual measurements
p_measurements <-
  sw_meas %>% 
  subset(!is.na(measured_rating_diff)) %>%
  subset(discharge_va > 0) %>% 
  ggplot(aes(x = discharge_cfs_cut, fill = measured_rating_diff)) +
  geom_bar() +
  scale_x_discrete(
    name = "Flow [cms]", 
    labels = c("<0.005", "0.005-0.05", "0.05-0.5", "0.5-5",">5")) +
  scale_y_continuous(
    name = "Number of Flow\nMeasurements",
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

p_measurements
 
# Figure of annual runoff vs % days bays below "good" --------------------------
# Add water year information
daily_flow <- daily_flow %>% 
  as_tibble() %>% 
  mutate(waterYear = year(date + days(92)))

#Determine lowest "good" flow measurement
minGoodQ_cms <- subGoodQ %>% 
  filter(gage_id == USGS_gage) %>% 
  mutate(minGoodQ_cms = minGoodQ_cfs*0.0283168) %>% 
  select(minGoodQ_cms) %>% pull()

#estimate duration below good
dur_below_good <- daily_flow %>% 
  mutate(below_good = if_else(Q_cms < minGoodQ_cms, 1, 0)) %>% 
  group_by(waterYear) %>% 
  summarise(
    below_good = sum(below_good),
    total      = n(),
    prop_below_good = below_good/total
  ) %>% 
  select(waterYear, prop_below_good)

#Obtain watershed area information
ws_area<- gages %>% 
  st_drop_geometry() %>% 
  filter(STAID == USGS_gage) %>% 
  select(DRAIN_SQKM) %>% 
  pull()

# Estimate Annual runoff depth [mm]
annual_flow <- daily_flow %>% 
  mutate(q_mm_day = Q_cms/ws_area/(1000^2)*1000*86400) %>% 
  group_by(waterYear) %>% 
  summarise(q_cm = sum(q_mm_day)/10) 

#Join annual runoff and proportion below good tibbles
annual_flow <- left_join(annual_flow, dur_below_good)

#plot
p_duration<-annual_flow %>% 
  ggplot() + 
    aes(x = q_cm, y = prop_below_good*100) +
    geom_point(
      pch=19, 
      cex=2.5, 
      col="grey30"
    ) +
    theme_classic() +
      theme(
        legend.title = element_text(size=10),
        legend.text  = element_text(size=8),
      ) + 
    xlab("Anual Runoff [cm]") + 
    ylab("Annual duration of flow below\n'Good' flow threshold [%]")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Combine plots --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Print individual plots and them put them together in powerpoint
ggsave(plot = map_fig,        file = "docs/map.png",            width = 6,   height = 3.75, units="in", dpi = 300)
ggsave(plot = p_measurements, file = "docs/p_measurements.png", width = 3,   height = 2, units="in", dpi = 300)
ggsave(plot = p_duration,      file = "docs/p_duration.png",      width = 3,   height = 2, units="in", dpi = 300)

