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
library(scales)    #ploting
library(sf)        #spatial data

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

#Convert to percent
gages <- gages %>%
  mutate(
    daysSubGood_prop = daysSubGood_prc,
    daysSubGood_prc  = daysSubGood_prc*100)

#Seprate gages in perennial and non-perennial
p_gages  <- gages %>% filter(NP == 0)
np_gages <- gages %>% filter(NP == 1)

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
      color = guide_colorbar(
        title ="% Flow Record", 
        order = 1),
      size="none", 
      alpha = "none",
      shape = guide_legend(
        title = "Stream Type", 
        override.aes = list(size=3, color=c("#4575b4","#d73027")), 
        order = 2)) +
    theme_bw() +
      theme(legend.position = "right") +
      xlab(NULL) +
      ylab(NULL) 
  
map_fig

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Duration of flow below "good" measurement ----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Make plot
daysSubGood_plot <- ggplot() + 
  geom_density(
    data = p_gages,
    aes(daysSubGood_prc), 
    fill = "#4575b4", 
    alpha = 0.99, 
    col = "grey30", 
    size = 0.5) +
  geom_density(
    data = np_gages,
    aes(daysSubGood_prc), 
    fill = "#d73027", 
    alpha = 0.90, 
    col = "grey30", 
    size = 0.5) +
  theme_bw() + 
    scale_x_log10(
      breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
      labels = c("0.001", "0.01","0.1", "1", "10", "100")
    ) +
    xlab("% Flow Record")

daysSubGood_plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Magnitude of flow below "good" measurement ----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Make plot
minGoodQ_plot <- ggplot() + 
  geom_density(
    data = p_gages,
    aes(minGoodQ_cfs), 
    fill = "#4575b4", 
    col  = "grey30", 
    size = 0.5,
    alpha = 0.99) +
  geom_density(
    data = np_gages,
    aes(minGoodQ_cfs), 
    fill = "#d73027", 
    col = "grey30",
    alpha = 0.90, 
    size = 0.5) +
  theme_bw() + 
  scale_x_log10(
    limits = c(10^-3, 10^4),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)))  +
  xlab("Flow [cfs]")

minGoodQ_plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Combine plots --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
map_fig / (minGoodQ_plot + daysSubGood_plot) + 
  plot_layout(
    guides = 'collect', 
    heights = c(3,1)) + 
  plot_annotation(tag_levels = "A", tag_suffix = ".")

ggsave("docs/USGS_flow_rating.png", width = 6, height =5, units ="in")
