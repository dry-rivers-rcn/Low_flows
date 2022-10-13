#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Theoretical Low-flow Error Rates (V2)
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
#Step 2: Focus on how error gets larger at flow -------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#Create master dataframe with flow conditions
df <- tibble(
  q_surf = seq(1,9,0.1), 
  q_sub  = 1,
  q_tot  = q_surf + q_sub
)

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

#Estimate methods flow measurement
df<-df %>% 
  mutate(
    q_flume   = (q_surf + q_sub*subsurface$flume)*(1-error$flume),
    q_slug    = (q_surf + q_sub*subsurface$slug)*(1-error$slug), 
    q_adv     = (q_surf  + q_sub*subsurface$adv )*(1-error$adv),
    q_bucket  = (q_surf  + q_sub*subsurface$bucket )*(1-error$bucket),
    q_orange  = (q_surf  + q_sub*subsurface$orange )*(1-error$orange))

#Estimate absolute error and percent error
df <- df %>% 
  mutate(
    abs_error_flume = q_tot - q_flume,
    abs_error_slug = q_tot - q_slug,
    abs_error_adv = q_tot - q_adv,
    abs_error_bucket = q_tot - q_bucket,
    abs_error_orange = q_tot - q_orange) %>% 
  mutate(
    per_error_flume = (q_tot - q_flume)/q_tot*100,
    per_error_slug  = (q_tot - q_slug)/q_tot*100,
    per_error_adv   = (q_tot - q_adv)/q_tot*100,
    per_error_bucket = (q_tot - q_bucket)/q_tot*100,
    per_error_orange = (q_tot - q_orange)/q_tot*100)

#Estimate methods flow measurement
abs_error_plot<-df %>% 
  ggplot() + 
  geom_line(aes(x=q_tot, y=abs_error_flume), col = "#e41a1c", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=abs_error_slug),  col = "#377eb8", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=abs_error_adv),   col = "#4daf4a", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=abs_error_bucket), col = "#984ea3", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=abs_error_orange), col = "#ff7f00", lwd = 2, lty=2) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)) +
  xlab("Flow [cfs]") + 
  ylab("Absolute Error [cfs]") 

per_error_plot<-df %>% 
  ggplot() + 
  geom_line(aes(x=q_tot, y=per_error_flume),  col = "#e41a1c", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=per_error_slug),   col = "#377eb8", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=per_error_adv),    col = "#4daf4a", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=per_error_bucket), col = "#984ea3", lwd = 2, lty=2) +
  geom_line(aes(x=q_tot, y=per_error_orange), col = "#ff7f00", lwd = 2, lty=2) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)) +
  xlab("Flow [cfs]") + 
  ylab("Percent Error") 

abs_error_plot + per_error_plot
ggsave(
  paste0(output_dir, "error_plot_v2.png"), 
  width = 7, height =3.5, units = "in", dpi = 300)
