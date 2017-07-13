## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Vagrant birds in birdwatching/vagrant-birds-in-NA")

library(RSQLite)
library(lubridate)
library(dplyr)
library(readr)
library(tictoc)

### connect to the two separate sqlite dbs
can_db <- src_sqlite("Data/canada-2016.sqlite", create=FALSE)
can <- tbl(can_db, "canada-2016")

usa_db <- src_sqlite("Data/usa-2016.sqlite", create=FALSE)
usa <- tbl(usa_db, "usa-2016")

### read in file of vagrant birds to select from sqlite dbs
vagrant_birds_2016 <- read_csv("Data/vagrant_birds_2016.csv")


### select the vagrant birds seen
## first, for USA
vagrants_USA_ABA.selection <- vagrant_birds_2016 %>%
  filter(country=="United States") %>%
  filter(scale=="ABA")

vagrants_USA_ABA <- usa %>%
  select(GLOBAL_UNIQUE_IDENTIFIER, COMMON_NAME, OBSERVER_ID, LOCALITY_ID, COUNTRY,
         LATITUDE, LONGITUDE, STATE_PROVINCE, COUNTY, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  filter(STATE_PROVINCE %in% vagrants_USA_ABA.selection$state_prov) %>%
  filter(COUNTY %in% vagrants_USA_ABA.selection$county) %>%
  filter(COMMON_NAME %in% vagrants_USA_ABA.selection$species) %>%
  collect(n=Inf)

## Then, for Canada
vagrants_CAN_ABA.selection <- vagrant_birds_2016 %>%
  filter(country=="Canada") %>%
  filter(scale=="ABA")

vagrants_CAN_ABA <- can %>%
  select(GLOBAL_UNIQUE_IDENTIFIER, COMMON_NAME, OBSERVER_ID, LOCALITY_ID, COUNTRY,
         LATITUDE, LONGITUDE, STATE_PROVINCE, COUNTY, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  filter(STATE_PROVINCE %in% vagrants_CAN_ABA.selection$state_prov) %>%
  filter(COMMON_NAME %in% vagrants_CAN_ABA.selection$species) %>%
  collect(n=Inf)

## merge the two dfs together
ABA_2016_rarities <- rbind(vagrants_CAN_ABA, vagrants_USA_ABA)


#############################################################################
#############################################################################
####### create dataframe for a local level analysis of vagrant birds ########
#############################################################################
#############################################################################

## create a selection df
state_level_vagrants_CAN <- vagrant_birds_2016 %>%
  filter(scale=="State") %>%
  filter(country=="Canada") %>%
  filter(species != "Ancient Murrelet") %>%
  filter(species != "European Storm-Petrel") %>%
  filter(species != "Least Auklet") %>%
  filter(species != "Audubon's Shearwater")

vagrants_CAN_state <- can %>%
  select(GLOBAL_UNIQUE_IDENTIFIER, COMMON_NAME, OBSERVER_ID, LOCALITY_ID, COUNTRY,
         LATITUDE, LONGITUDE, STATE_PROVINCE, COUNTY, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  filter(STATE_PROVINCE %in% state_level_vagrants_CAN$state_prov) %>%
  filter(COMMON_NAME %in% state_level_vagrants_CAN$species) %>%
  collect(n=Inf)

state_level_vagrants_USA <-  vagrant_birds_2016 %>%
  filter(scale=="State") %>%
  filter(country=="United States") %>%
  filter(species != "Ancient Murrelet") %>% # 'seabird'
  filter(species != "European Storm-Petrel") %>% # 'seabird'
  filter(species != "Least Auklet") %>% # 'seabird'
  filter(species != "Audubon's Shearwater") %>% # 'seabird'
  filter(species != "Sandhill Crane") %>% # Washington D.C. record
  filter(state_prov != "Alaska") %>% # Alaska records
  mutate(spatial_filter=paste(county, state_prov, species, sep="_"))


vagrants_USA_state <- usa %>%
  select(GLOBAL_UNIQUE_IDENTIFIER, COMMON_NAME, OBSERVER_ID, LOCALITY_ID, COUNTRY,
         LATITUDE, LONGITUDE, STATE_PROVINCE, COUNTY, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  filter(STATE_PROVINCE %in% state_level_vagrants_USA$state_prov) %>%
  filter(COUNTY %in% state_level_vagrants_USA$county) %>%
  filter(COMMON_NAME %in% state_level_vagrants_USA$species) %>%
  collect(n=Inf) %>%
  mutate(spatial_filter=paste(COUNTY, STATE_PROVINCE, COMMON_NAME, sep="_")) %>%
  filter(spatial_filter %in% state_level_vagrants_USA$spatial_filter) %>%
  dplyr::select(-spatial_filter)


## merge the two dfs together
state_2016_rarities <- rbind(vagrants_CAN_state, vagrants_USA_state)

## remove all object besides dataframes necessary
rm(list=setdiff(ls(), c("state_2016_rarities", "ABA_2016_rarities")))

## save dfs as Rdata file
save.image("Data/rarity_data.RData")




