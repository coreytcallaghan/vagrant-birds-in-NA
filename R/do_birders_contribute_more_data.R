## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Vagrant birds in birdwatching/vagrant-birds-in-NA")

## packages
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(data.table)
library(geosphere)

## load data
load("Data/observer_data.RData")
load("Data/ABA_analysis_results.RData")

## source data
source("R/global_functions.R")

## read in dataframe of rarities
vagrant_birds_2016 <- read_csv("Data/vagrant_birds_2016.csv")
ABA_vagrants <- vagrant_birds_2016 %>%
  filter(scale=='ABA') %>%
  rename(COMMON_NAME=species) %>%
  rename(STATE_PROVINCE=state_prov)

## add the bird lat/long to the dataframe of vagrants
ABA_vagrants <- ABA_analysis %>%
  select(COMMON_NAME, STATE_PROVINCE, bird_lat, bird_long) %>%
  filter(complete.cases(bird_lat)) %>%
  distinct(.keep_all=TRUE) %>%
  inner_join(., ABA_vagrants, by=c("COMMON_NAME", "STATE_PROVINCE"))

## add a bounding box of coordinates around the bird lat/long