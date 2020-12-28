library(ggmap)
library(tidyverse)
# for reverse geocoding to gather zipcode
# library(revgeo)

###  Geocoding ###

# googleApiKey = "ENTER_YOUR_API_KEY_HERE_IF_YA_WANT"
# register_google(key=googleApiKey)#



# only used if CURL error HTTP2 occurs while trying to connect to ggmap api
# library(httr)
# httr::set_config(httr::config(http_version = 0))


# Enter most up to date UOF data that already has lat/long data
UOF_latest_with_lat_long.df <- readr::read_csv("../../WranglingRawData/CleanedDatasets/UOF/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv") 
# Enter newest wrangled UOF here
UOF_newest.df <-
  readr::read_csv("../../WranglingRawData/CleanedDatasets/UOF/wrangledUOF_2020_Dec_27.csv")


latLong.df <- UOF_latest_with_lat_long.df %>% select(c(INCNUM, latitude, longitude)) %>% distinct()
View(latLong.df)

latest.df <- UOF_newest.df
View(latest.df)


joined.df <- latest.df %>% left_join(latLong.df, by= "INCNUM")
View(joined.df)



# with only unique addresses to keep cost down
justMissingLatLon.df <- joined.df %>% filter(is.na(longitude)) %>% dplyr::distinct(FULL_ADD, .keep_all = TRUE) %>%  select(-c(latitude, longitude))
# View(justMissingLatLon.df)




# this runs the actual ggmaps api command, don't run unless necessary
# latestGeocoded <- justMissingLatLon.df %>%
  # mutate_geocode(FULL_ADD)




latestGeocoded <-
  read_csv("geocoded_2020_12_27.csv")
# View(latestGeocoded)


# TODO
# the issue is the joining of tables, i have the datasets and want to join them on the INCNUM and populate any lat/long that isn't there

x <- latestGeocoded %>% rename(latitude = lat, longitude = lon) %>%  select(c(INCNUM, latitude, longitude))
View(x)


z <- joined.df %>% distinct(INCNUM) 

allTogetherNow <- z %>% left_join(x, KEEP = TRUE)
View(allTogetherNow)
