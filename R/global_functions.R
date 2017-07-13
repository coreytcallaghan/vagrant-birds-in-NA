
## Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



## finding the 'home coordinates' for each observer in a given index
return_observer_coords <- function(data) {
  
  dat <- unlist(unname(data[, 'observers']))
  
  first_date <- data[, 'first_observer_date'][[1,1,]]
  second_date <- data[, 'first_ebird_date'][[1,1,]]
  
  dat.2 <- ABA_observers %>%
    filter(OBSERVER_ID %in% dat) %>%
    filter(OBSERVATION_DATE >= first_date & OBSERVATION_DATE <= second_date) %>%
    filter(COUNTRY %in% c("United States", "Canada")) %>%
    unite(county_state, COUNTY, STATE, sep="_", remove=TRUE) %>%
    group_by(OBSERVER_ID) %>%
    summarise(home_county_state=Mode(county_state),
              home_country=Mode(COUNTRY)) %>%
    separate(home_county_state, c("COUNTY", "STATE"), sep="_") %>%
    inner_join(., ABA_observers, by=c("OBSERVER_ID", "COUNTY", "STATE")) %>%
    group_by(OBSERVER_ID) %>%
    summarise(obsr_lat=mean(LATITUDE),
              obsr_long=mean(LONGITUDE))
  
  return(dat.2)
}
