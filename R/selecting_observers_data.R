## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Vagrant birds in birdwatching/vagrant-birds-in-NA")

### Packages
library(dplyr)
library(RSQLite)
library(tictoc)



### Read in RData
load("Data/rarity_data.RData")


###################################################
##### Get the observer information necessary ######
###################################################

## needs to be a character string to select from another database
ABA_rare_observers <- ABA_2016_rarities %>%
  dplyr::select(OBSERVER_ID) %>%
  distinct() %>%
  .$OBSERVER_ID

state_rare_observers <- state_2016_rarities %>%
  dplyr::select(OBSERVER_ID) %>%
  distinct() %>%
  .$OBSERVER_ID


### connect to all ebird sqlite file db
### this needs to be a hard connection
ebird_db <- src_sqlite("D:/All eBird Data/ebird.sqlite", create=FALSE)
all_ebird <- tbl(ebird_db, "ebird")

### Now can select and subset to necessary information for the observers subsetted above
tic(ABA_observers <- all_ebird %>%
  dplyr::select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER,
         ALL_SPECIES_REPORTED, DURATION_MINUTES, EFFORT_DISTANCE_KM, LATITUDE, LONGITUDE,
         COUNTRY, COUNTY, STATE, PROTOCOL_TYPE) %>%
  distinct() %>%
  filter(OBSERVATION_DATE > "2015-06-01") %>%
  filter(OBSERVER_ID %in% ABA_rare_observers) %>%
  collect(n=Inf))
toc()

tic(state_observers <- all_ebird %>%
  dplyr::select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER,
         ALL_SPECIES_REPORTED, DURATION_MINUTES, EFFORT_DISTANCE_KM, LATITUDE, LONGITUDE,
         COUNTRY, COUNTY, STATE, PROTOCOL_TYPE) %>%
  distinct() %>%
  filter(OBSERVATION_DATE > "2015-06-01") %>%
  filter(OBSERVER_ID %in% state_rare_observers) %>%
  collect(n=Inf))
toc()


## remove all object besides dataframes necessary
rm(list=setdiff(ls(), c("state_observers", "ABA_observers")))

## save dfs as Rdata file
save.image("Data/observer_data.RData")







