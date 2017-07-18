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
load("Data/rarity_data.RData")

## source data
source("R/global_functions.R")

## read in dataframe of rarities
vagrant_birds_2016 <- read_csv("Data/vagrant_birds_2016.csv")
ABA_vagrants <- vagrant_birds_2016 %>%
  filter(scale=='ABA') %>%
  rename(COMMON_NAME=species) %>%
  rename(STATE_PROVINCE=state_prov)

## convert date to a date class
ABA_2016_rarities$OBSERVATION_DATE <- as.Date(ABA_2016_rarities$OBSERVATION_DATE, format="%Y-%m-%d")

## need to clean up Blue Bunting data
## the main reports come from January to April
## but there are two reports from December which
## don't necessarily pertain to the Blue Bunting that was
## 'advertised' on the ABA blog
## this will drop the two reports which don't pertain to the known Blue Bunting
ABA_2016_rarities <- ABA_2016_rarities %>%
  filter(SAMPLING_EVENT_IDENTIFIER != "S33220944") %>%
  filter(SAMPLING_EVENT_IDENTIFIER != "S33252432")

## summary of observational data for each vagrant
ABA_observation_summary <- ABA_2016_rarities %>%
  group_by(COMMON_NAME, STATE_PROVINCE) %>%
  arrange(OBSERVATION_DATE) %>%
  slice(1) %>%
  rename(first_ebird_date = OBSERVATION_DATE) %>%
  dplyr::select(COMMON_NAME, STATE_PROVINCE, first_ebird_date) %>%
  inner_join(., ABA_2016_rarities, by=c("COMMON_NAME", "STATE_PROVINCE")) %>%
  group_by(COMMON_NAME, STATE_PROVINCE) %>%
  arrange(OBSERVATION_DATE)%>%
  slice(n()) %>%
  rename(last_ebird_date = OBSERVATION_DATE) %>%
  dplyr::select(COMMON_NAME, STATE_PROVINCE, first_ebird_date, last_ebird_date) %>%
  inner_join(., ABA_2016_rarities, by=c("COMMON_NAME", "STATE_PROVINCE")) %>%
  mutate(time_in_days=as.numeric(difftime(last_ebird_date, first_ebird_date, units="days"))+1) %>%
  group_by(COMMON_NAME, STATE_PROVINCE) %>%
  summarise(Number_obs=length(OBSERVER_ID),
            Number_unique_obs=length(unique(OBSERVER_ID)),
            length_of_stay=mean(time_in_days),
            first_ebird_date=Mode(first_ebird_date),
            bird_lat=mean(LATITUDE),
            bird_long=mean(LONGITUDE)) %>%
  mutate(first_observer_date=as.Date(first_ebird_date)-180) %>%
  inner_join(., ABA_2016_rarities, by=c("COMMON_NAME", "STATE_PROVINCE")) %>%
  group_by(COMMON_NAME, STATE_PROVINCE) %>%
  mutate(observers=list(OBSERVER_ID)) %>%
  dplyr::select(COMMON_NAME, STATE_PROVINCE, length_of_stay, Number_obs, Number_unique_obs,
         first_ebird_date, first_observer_date, observers, bird_lat, bird_long) %>%
  distinct(.keep_all = TRUE) %>%
  inner_join(., ABA_vagrants, by=c("COMMON_NAME", "STATE_PROVINCE")) 

## adding an index. Can't seem to put this in the dplyr pipe above...
ABA_observation_summary$Index <- 1:nrow(ABA_observation_summary)

## This returns the 'home' coordinates for each observer who saw a given bird
## calls a function: "return_observer_data"
observer_coords <- ABA_observation_summary %>%
  group_by(Index) %>%
  do(lists=return_observer_coords(.)) %>%
  unnest(., lists)

## calculate the number of times a birder saw a specific bird
number_obs_birder <- ABA_2016_rarities %>%
  group_by(COMMON_NAME, STATE_PROVINCE, OBSERVER_ID) %>%
  summarise(number_visits_per_individual=length(SAMPLING_EVENT_IDENTIFIER))

## merge the above three dataframes into one dataframe
## I think that the observers who have NA filled in are overseas birders
## Can probably drop them from analysis
ABA_analysis <- merge(observer_coords, ABA_observation_summary, by="Index")
ABA_analysis <- merge(ABA_analysis, number_obs_birder, 
                      by=c("OBSERVER_ID", "COMMON_NAME", "STATE_PROVINCE"), all=TRUE)


## calculate approximate distance each individual travelled
setDT(ABA_analysis)[, distance_travelled.km := distGeo(matrix(c(bird_long, bird_lat), ncol=2),
                                                           matrix(c(obsr_long, obsr_lat), ncol=2))/1000]

## clean up dataframe
ABA_analysis$Index <- NULL
ABA_analysis$observers <- NULL
ABA_analysis$first_observer_date <- NULL
ABA_analysis$scale <- NULL

## remove all object besides dataframes necessary
rm(list=setdiff(ls(), c("ABA_analysis")))

## save df as Rdata file
save.image("Data/ABA_analysis_results.RData")









  