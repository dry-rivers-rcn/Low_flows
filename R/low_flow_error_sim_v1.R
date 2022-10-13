#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Theoretical Low-flow Error Rates (V1)
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 10/11/2022
# Purpose: Explore theoretical error rates associated with low-flow estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup Environment ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#Load packages of interest
library(tidyverse) #join the cult
library(patchwork) #combine multiple plot objects
library(scales)

#Define output dir
output_dir<-"docs/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Examine impact of increasing low flow --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Define model variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Flow (cfs)
Qmin <- 0 
Qmax <- 10

#Proportion of Surface flow
Q_prop_min <- 0
Q_prop_max <- 1

# Define Error Rate
error <- tibble(
  flume  = 0.05,
  slug   = 0.06,
  adv    = 0.075,
  bucket = 0.20,
  orange = 0.14, 
  .name_repair = "minimal")

#Define subsurface measure (bianary)
subsurface <- tibble(
  flume  = 1,
  slug   = 1,
  adv    = 0,
  bucket = 0,
  orange = 0, 
  .name_repair = "minimal")

#2.2 Estimate Error ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create bounds of dataspace
df <- expand.grid(
  Q_actual = seq(Qmin,Qmax, 1), 
  Q_prop   = seq(0,1, 0.01))

# Estimate Q measured, Q percent error, and Q absolute error 
df <-  df %>% 
  # Estimate Q measured 
  mutate(Q_flume  = Q_actual*(1-error$flume), 
         Q_slug   = Q_actual*(1-error$slug),
         Q_adv    = Q_actual*Q_prop*(1-error$adv), 
         Q_bucket = Q_actual*Q_prop*(1-error$bucket), 
         Q_orange = Q_actual*Q_prop*(1-error$orange)) %>% 
  # Estimate absolute error 
  mutate(abs_error_flume = Q_actual-Q_flume,
         abs_error_slug  = Q_actual-Q_slug, 
         abs_error_adv = Q_actual-Q_adv,
         abs_error_bucket = Q_actual-Q_bucket,
         abs_error_orange = Q_actual-Q_orange) %>% 
  # Estimate percent error 
  mutate(per_error_flume = (Q_actual-Q_flume)/Q_actual,
         per_error_slug  = (Q_actual-Q_slug)/Q_actual, 
         per_error_adv =   (Q_actual-Q_adv)/Q_actual,
         per_error_bucket = (Q_actual-Q_bucket)/Q_actual,
         per_error_orange = (Q_actual-Q_orange)/Q_actual) 

#2.3 Plotting options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
axis_title_size <- 11
axis_font_size  <- 8 

#2.4 Individual plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Slug
abs_flume<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = abs_error_flume)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,2.5,5.0,7.5,10), 
    limits   = c(0, 10)) +  
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  ggtitle("Flume") + 
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

per_flume<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = per_error_flume)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,.25,.50,.75,.100), 
    limits   = c(0, 1)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

#Slug
abs_slug<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = abs_error_slug)) +
  geom_raster() +
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,2.5,5.0,7.5,10), 
    limits   = c(0, 10)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  ggtitle("Dillution Gauging") + 
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

per_slug<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = per_error_slug)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,.25,.50,.75,.100), 
    limits   = c(0, 1)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

#ADV
abs_adv<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = abs_error_adv)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,2.5,5.0,7.5,10), 
    limits   = c(0, 10)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  ggtitle("Acoustic Doppler Durrent Profiler") +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

per_adv<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = per_error_adv)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,.25,.50,.75,.100), 
    limits   = c(0, 1)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

#Bucket
abs_bucket<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = abs_error_bucket)) +
  geom_raster() +
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,2.5,5.0,7.5,10), 
    limits   = c(0, 10)) +theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  ggtitle("Bucket Test") +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

per_bucket<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = per_error_bucket)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,.25,.50,.75,.100), 
    limits   = c(0, 1)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

#orange
abs_orange<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = abs_error_orange)) +
  geom_raster()+
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"), 
    breaks   = c(0,2.5,5.0,7.5,10), 
    limits   = c(0, 10)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  ggtitle("Orange Test") +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

per_orange<-df %>% 
  ggplot(aes(Q_prop, Q_actual, fill = per_error_orange)) +
  geom_raster() +
  scale_fill_gradientn(
    colours=c("#0571b0","#92c5de",  "#f7f7f7","#f4a582","#ca0020"),
    breaks   = c(0,.25,.50,.75,.100),
    limits   = c(0, 1)) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.title = element_text(size = axis_title_size), 
    axis.text  = element_text(size = axis_font_size)) +
  xlab("Proportion of Surface Flow") + 
  ylab("Flow [cfs]") 

#2.5 Final Plots! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(abs_flume + per_flume)/(abs_slug + per_slug)/
  (abs_adv + per_adv)/(abs_bucket + per_bucket)/
  (abs_orange + per_orange)

ggsave(
  paste0(output_dir, "error_plot_V1.png"), 
  width = 7, height =9, units = "in", dpi = 300)

# (abs_slug + per_slug)/(abs_adv + per_adv)
# ggsave(
#   paste0(output_dir, "gw_driver_plot_small.png"), 
#   width = 7, height =4, units = "in", dpi = 300)

