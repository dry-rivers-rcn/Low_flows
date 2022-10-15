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
library(tidyverse) #join the cult
library(patchwork) #combine multiple plot objects
library(sf)

#Read and tidy spatial data
subGoodQ <- read_csv("data/SubGoodQ.csv")
gages    <- read_csv("data/gagesII.csv")
p_class  <- read_csv("data/price_2021_np_streams.csv", col_types = c("ci")) 
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

#Tidy price classification data
p_class <- p_class %>% 
  #rename gage id for join below
  rename(STAID = gage) %>% 
  #Remove duplicates 
  distinct() %>% 
  #deal with 0 infront of gage no
  mutate(
    n.char = nchar(STAID),
    STAID = if_else(n.char==7, paste0("0", STAID), STAID), 
    STAID = if_else(n.char==9, paste0("0", STAID), STAID)) %>% 
  select(-n.char)
  
#Edit gages shape
gages <- gages %>% 
  #Add duration of streamflow under good readings
  left_join(., subGoodQ %>% rename(STAID = gage_id)) %>% 
  drop_na(minGoodQ_cfs) %>% 
  #Add Price non-perennial stream classification
  left_join(., p_class) %>% 
  mutate(NP = replace_na(NP, 0)) %>%
  mutate(NP = paste0(NP)) %>% 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Gage location figure -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Crate map
gages %>%
  mutate(
    daysSubGood_prop = daysSubGood_prc,
    daysSubGood_prc  = daysSubGood_prc*100) %>% 
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
        shape= NP)) +
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
    scale_shape(labels = c("Perennial", "Non-perennial")) +
    guides(
      shape = guide_legend(
        title = "Stream Type", 
        override.aes = list(size=3, color="grey30")),
      color = guide_colorbar(title="% Flow Record"),
      size="none", 
      alpha = "none") +
    theme_bw() +
      theme(legend.position = "right") +
      xlab(NULL) +
      ylab(NULL) 
  
      

