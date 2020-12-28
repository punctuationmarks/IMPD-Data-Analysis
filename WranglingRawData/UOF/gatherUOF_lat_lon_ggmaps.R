# dataset found here
# http://data.indy.gov


# mapping
library(ggmap)
library(tidyverse)
# for reverse geocoding to gather zipcode
library(revgeo)

# only used if CURL error HTTP2 occurs while trying to connect to ggmap api
# library(httr)
# httr::set_config(httr::config(http_version = 0))


# googleApiKey = "ENTER_YOUR_API_KEY_HERE_IF_YA_WANT"
# register_google(key=googleApiKey)#


# ENTER NEWEST UOF HERE
useOfForce.df <- readr::read_csv("../../WranglingRawData/CleanedDatasets/UOF/wrangledUOF_2020_Dec_27.csv")
# View(useOfForce.df)

# joining all of the lat, lon, and incnums
# lat_lon_inc_0 <- readr::read_csv("Datasets/UOF_lat_lon_incnum_up_to_july_2019.csv")
# lat_lon_inc_1 <- readr::read_csv("Datasets/UOF_lat_lon_incnum_07_19_to_12_19.csv")


# View(lat_lon_inc_0)
# View(lat_lon_inc_1)
# lat_lon_inc <- full_join(lat_lon_inc_0, lat_lon_inc_1)

# View(lat_lon_inc)


# full join leaves NA values for lat and lon in the original dataset
new_UOF_with_most_current_lat_lon <- full_join(useOfForce.df, lat_lon_inc)
# View(new_UOF_with_most_current_lat_lon)

# write_csv(new_UOF_with_most_current_lat_lon, "UOF_all___with_lat_lon_up_to_dec_2019.csv")
# write_csv(new_UOF_with_most_current_lat_lon, "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/Mapping/UOF_all___with_lat_lon_up_to_dec_2019.csv")
# write_csv(new_UOF_with_most_current_lat_lon, "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/UOF_all___with_lat_lon_up_to_dec_2019.csv")
# write_csv(new_UOF_with_most_current_lat_lon, "../CleanData/UOF/UOF_all___with_lat_lon_up_to_dec_2019.csv")



# making a copy of the data, so we're not importing it
# multiple times and slowing down the analysis
# also nice to have a "reset button" in case you get down a rabbit hole and want to see what's up
geoUOF.df <- new_UOF_with_most_current_lat_lon


###
###
###
### IF YOU WANTED TO ADD MORE INFORMTION TO THIS, E.G. UPDATED DATA WITH NEW LAT/LON INFORMATION THROUGH GGMAPS
## THIS WOULD BE A GOOD WAY OF PROCEEDING
#
# just_missing_lat_lons <- UOF_WITH_lat_lon.df %>%
#        filter(is.na(lat) & is.na(lon))
#
# # making new address column for google maps
# just_missing_lat_lons <-
#   just_missing_lat_lons %>%
#   unite(STREET_ADD, STREET_N, STREET_G, STREET, STREET_T,
#         sep = " ") %>%
#   unite(FULL_ADD, STREET_ADD,
#         CITY, STATE, sep = ", ") %>%
#   distinct(INCNUM, .keep_all = TRUE)
#
#
# googleApiKey = "SECRET_KEY"
# register_google(key=googleApiKey)
#
# most_current_lat_lon_after_july_2019 <-
#   just_missing_lat_lons %>%
#   mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, INCNUM)
#


# dataset found herE
# http://data.indy.gov

# importing officer related shootings


library(tidyverse)
library(ggmap)
library(leaflet)
officerInvolvedShootings <- read_csv("../RawData/IMPD_Officer_Involved_Shootings.csv")

# View(officerInvolvedShootings)




# Note: all missing numeric values will default to 999999 so we're not losing any data
wrangledUOF.df <- officerInvolvedShootings %>%
  # renming variables
  dplyr::rename(
    CIT_SEX = SEX,
    CIT_RACE = RACE,
    OFF_NUM = OFFNUM,
    CIT_NUM = CITNUM
  ) %>%
  # mutating all of the strings for ease of MANIPULATION
  mutate_if(is.character, str_to_upper) %>%
  mutate_if(is.character, str_squish) %>%
  
  # cleaning up some manually entered data
  dplyr::mutate(CIT_SEX = ifelse((is.na(CIT_SEX) |
                                    CIT_SEX == "UNNOWN"), "UNREPORTED", CIT_SEX)) %>%
  mutate(CIT_SEX = if_else(CIT_SEX == "M", "MALE", CIT_SEX)) %>%
  mutate(CIT_RACE = ifelse((is.na(CIT_RACE) |
                              CIT_RACE == "UNNOWN" |
                              CIT_RACE == "NA"),
                           "UNREPORTED",
                           CIT_RACE
  )) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "B", "BLACK", CIT_RACE)) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "NATIVE AMER", "NATIVE AMERICAN", CIT_RACE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE < 18, 999999, OFF_AGE)) %>% # having a placeholder for the innacurate data for the officer age
  
  # dealing with basics of addresses
  # regex matching all of the mispelling of INDIANAPOLIS
  mutate(CITY = if_else(
    stringr::str_detect(CITY, "(IND|IIN|IND|INI|IDI|INJ)"),
    "INDIANAPOLIS",
    CITY
  )) %>%
  mutate(STATE = "IN") %>%
  # replace_na(OCCURRED_DT = 00:00:00:000) %>% 	
  # dealing with time:
  unite(YMD_TM,
        OCCURRED_DT,
        OCCURRED_TM,
        sep = " ",
        remove = FALSE) %>% # need to combine these to parse them
  mutate(YMD_TM = as.POSIXct(YMD_TM, format = "%Y-%m-%d %H:%M:%S")) %>%  # need to change the date and time to POSIXct
  mutate(YMD_TM = lubridate::ymd_hms(YMD_TM)) %>% # formatting the time again to ymd_hms for ease of lubridate's viewing
  mutate(OCCURRED_WEEK_DAY = lubridate::wday(OCCURRED_DT, label = TRUE)) %>%  # day of the week might be interesting
  mutate(OCCURRED_HOUR = lubridate::hour(YMD_TM)) %>%
  mutate(OCCURRED_QUARTER = lubridate::quarter(YMD_TM)) %>% # quarter might show some good trends
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "UNKNOWN" | CIT_RACE == "UNREPORTED", "UNREPORTED RACE", CIT_RACE)) %>%
  # removing unecessary strings in CIT_WEAPON
  mutate(CIT_WEAPON = str_remove(CIT_WEAPON, "SUSPECT - ")) %>% 
  # dealing with missing values
  replace_na(
    list(
      # OCCURRED_YEAR = "UNREPORTED",
      CITY = "INDIANAPOLIS",
      # defaulting to INDIANAPOLIS for an NA values
      UDTEXT24A = "UNREPORTED",
      UDTEXT24B = "UNREPORTED",
      UDTEXT24C = "UNREPORTED",
      UDTEXT24D = "UNREPORTED",
      DISPOSITION = "UNREPORTED",
      UOF_FORCE_TYPE = "UNREPORTED",
      UOF_REASON = "UNREPORTED",
      SERVICE_TYPE = "UNREPORTED",
      # leaving this blank for Google Maps
      STREET_G = " ",
      STREET_T = " ",
      CIT_NUM = "999999",
      CIT_ARRESTED = "UNREPORTED",
      CITCHARGE_TYPE = "UNREPORTED",
      CIT_WEAPON_TYPE = "UNREPORTED",
      CIT_INJURED = "UNREPORTED",
      CIT_HOSPITAL = "UNREPORTED",
      CIT_COND_TYPE = "UNREPORTED",
      CIT_RACE = "UNREPORTED RACE",
      CIT_SEX = "UNREPORTED",
      CIT_AGE = "999999",
      CIT_WEAPON = "UNREPORTED",
      OFF_WEAPON = "UNREPORTED",
      OFF_AGE = "999999",
      OFF_SEX = "UNREPORTED",
      OFF_RACE  = "UNREPORTED RACE",
      OFF_YR_EMPLOY = "999999",
      # This is technically already there, but R turns this into a NA value, needs to be made explicit
      OFF_COND_TYPE = "UNREPORTED",
      OFF_INJURED = "UNREPORTED",
      OFF_HOSPITAL = "UNREPORTED",
      OFF_NUM = "999999"
    )
  )


# View(wrangledUOF.df)


###  MAPPING ###

# only used if CURL error HTTP2 occurs while trying to connect to ggmap api
# library(httr)
# httr::set_config(httr::config(http_version = 0))


# googleApiKey = "ENTER_YOUR_API_KEY_HERE_IF_YA_WANT"
# register_google(key=googleApiKey)#


geoUOF.df <- wrangledUOF.df   

geoUOF.df <- geoUOF.df %>%  
  # filtering only the unique incnum since there is no reason we need to spend more $$
  # on addresses through google api than needed
  distinct(INCNUM, .keep_all = TRUE) %>%
  # removing the na that will be produced when the street_g is comibined into full address
  mutate(STREET_G = ifelse(is.na(STREET_G), " ", STREET_G))%>% 
  unite(STREET_ADD, STREET_N, STREET_G, STREET, STREET_T,
        sep = " ") %>%
  unite(FULL_ADD, STREET_ADD,
        CITY, STATE, sep = ", ") %>% 
  distinct(FULL_ADD, .keep_all = TRUE)

# View(geoUOF.df)




# geoCodedAddresses_officer_involved_shootings <-
#   geoUOF.df %>%
#   # google maps api, return lat/lon from address
#   mutate_geocode(FULL_ADD) %>%
#   # the lat/lon variables will be added via ggmaps
# rename(latitude = lat,
#    longitude = lon)

geoUOF.df <- geoUOF.df %>%  
  # filtering only the unique incnum since there is no reason we need to spend more $$
  # on addresses through google api than needed
  distinct(INCNUM, .keep_all = TRUE) %>%
  # removing the na that will be produced when the street_g is comibined into full address
  mutate(STREET_G = ifelse(is.na(STREET_G), " ", STREET_G))%>% 
  unite(STREET_ADD, STREET_N, STREET_G, STREET, STREET_T,
        sep = " ") %>%
  unite(FULL_ADD, STREET_ADD,
        CITY, STATE, sep = ", ") %>% 
  distinct(FULL_ADD, .keep_all = TRUE) %>% 
  filter(is.na(lat) & is.na(lon))
  
# View(geoUOF.df)







geoCodedAddressesUOF <-
  geoUOF.df %>%
  # dropping the NA lat and lon since ggmaps will add a new variable called lat1 and lon1 if we don't drop these here
  select(-c(lat, lon)) %>% 
  # google maps api, return lat/lon from address
  mutate_geocode(FULL_ADD) %>%
  # the lat/lon variables will be added via ggmaps
  select(lon, lat, everything())


  
# write_csv(geoCodedAddressesUOF, "UOF_lat_lon_incnum_07_19_to_12_19.csv")





## Getting the zip codes with revgeo



### so this works, but it's very slow, better concept might be to split it up by the 1k observersations
# IT MIGHT NOT BE FINISHED IN TIME, BEEN RUNNING 2.75 HOURS and it started to throttle (much earlier)
uof_cleaned_with_lat_lon <- read_csv("Datasets/UOF_all___with_lat_lon_up_to_dec_2019.csv")

lat_lon_for_reverse_geo <- uof_cleaned_with_lat_lon %>% 
  # making lat and lon more explcit for revgeo
  rename(latitude = lat, longitude = lon) %>% 
  select(latitude, longitude, INCNUM) %>% 
  unique() %>%
  # we're using index as a joiner between revgeo's zipcodes
  mutate(index=row_number())
# View(lat_lon_for_reverse_geo)

# View(lat_lon_for_reverse_geo)

lat_lon_incnum_zip <- revgeo(lat_lon_for_reverse_geo$longitude, lat_lon_for_reverse_geo$latitude, provider =  'photon', output = 'frame') %>% 
  mutate(index = row_number()) %>%
  select(index, zip) %>% 
  left_join(lat_lon_for_reverse_geo, by="index") %>% 
  # dropping the index column
  select(-index)
# View(lat_lon_incnum_zip)

write_csv(lat_lon_incnum_zip, "ScratchArea/testing_lat_lon_zip_UOF.csv")
x <- read_csv("ScratchArea/testing_lat_lon_zip_UOF.csv")
View(x)

# small batch due to throttling of the reverse geo api
# batch_1 <- lat_lon_for_reverse_geo[1:1000, ]
batch_2 <- lat_lon_for_reverse_geo[1001:1002, ]
# View(batch_2)
# batch_3 <- lat_lon_for_reverse_geo[2001:3000, ]
# batch_4 <- lat_lon_for_reverse_geo[3001:4000, ]
# batch_5 <- lat_lon_for_reverse_geo[4001:5000, ]
# batch_6 <- lat_lon_for_reverse_geo[5001:6000, ]
# # to the end
# batch_6 <- lat_lon_for_reverse_geo[6001:6568, ]
# 
# lat_lon_incnum_zip_1 <- revgeo(batch_1$longitude, batch_1$latitude, provider =  'photon', output = 'frame') %>% 
#   mutate(index = row_number()) %>%
#   select(index, zip) %>% 
#   left_join(batch_1, by="index") %>% 
#   # dropping the index column
#   select(-index) %>% 
#   write_csv("ScratchArea/uof_lat_lon_incnum_zip_1.csv")
# 
lat_lon_incnum_zip_2 <- revgeo(batch_2$longitude, batch_2$latitude, provider =  'photon', output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_2, by="index") %>%
  # dropping the index column
  select(-index)

write_csv(lat_lon_incnum_zip_2, "ScratchArea/uof_lat_lon_incnum_zip_2.csv")

# View(lat_lon_incnum_zip_2)
# 
# 
# 
# lat_lon_incnum_zip_3 <- revgeo(batch_3$longitude, batch_3$latitude, provider =  'photon', output = 'frame') %>% 
#   mutate(index = row_number()) %>%
#   select(index, zip) %>% 
#   left_join(batch_3, by="index") %>% 
#   # dropping the index column
#   select(-index) %>% 
#   write_csv("ScratchArea/uof_lat_lon_incnum_zip_3.csv")
# 
# 
# 
# 
# lat_lon_incnum_zip_4 <- revgeo(batch_4$longitude, batch_4$latitude, provider =  'photon', output = 'frame') %>% 
#   mutate(index = row_number()) %>%
#   select(index, zip) %>% 
#   left_join(batch_4, by="index") %>% 
#   # dropping the index column
#   select(-index) %>% 
#   write_csv("ScratchArea/uof_lat_lon_incnum_zip_4.csv")
# 
# 
# 
# 
# uof_lat_lon_incnum_zip_5 <- revgeo(batch_5$longitude, batch_5$latitude, provider =  'photon', output = 'frame') %>% 
#   mutate(index = row_number()) %>%
#   select(index, zip) %>% 
#   left_join(batch_5, by="index") %>% 
#   # dropping the index column
#   select(-index) %>% 
#   write_csv("ScratchArea/uof_lat_lon_incnum_zip_5.csv")
# 
# 
# 
# 
# uof_lat_lon_incnum_zip_6 <- revgeo(batch_6$longitude, batch_6$latitude, provider =  'photon', output = 'frame') %>% 
#   mutate(index = row_number()) %>%
#   select(index, zip) %>% 
#   left_join(batch_6, by="index") %>% 
#   # dropping the index column
#   select(-index) %>% 
#   write_csv("ScratchArea/uof_lat_lon_incnum_zip_6.csv")
# 
# 
# 
# zip_1 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_1.csv")
# zip_2 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_2.csv")
# zip_3 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_3.csv")
# zip_4 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_4.csv")
# zip_5 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_5.csv")
# zip_6 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_6.csv")
# 
# View(zip_1)
# View(zip_2)
# View(zip_4)
# zip_joined_1 = full_join(zip_1, zip_2)
# View(zip_joined_1)




# joining the full cleaned data with the lat/lon and zip data 
# joined_officer_involved_shootngs_lat_lon_zip <- full_join(lat_lon_incnum_zip, y)
# View(joined_officer_involved_shootngs_lat_lon_zip)
# write_csv(joined_officer_involved_shootngs_lat_lon_zip, "Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
# testing_write <- read_csv("Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
# View(testing_write)




##### NOTICE!!! ######

# Some example errors from Google maps API,
# "3600   Kline Driv..." not uniquely geocoded, using "3600 kline n dr, indianapolis, in 46226, usa"
# "10 S Delaware Str..." not uniquely geocoded, using "10 delaware st, indianapolis, in 46204, usa"
# "5200 S Madison Av..." not uniquely geocoded, using "5200 madison ave, indianapolis, in 46227, usa"
# "2900 N Kessler Bl..." not uniquely geocoded, using "2900 w kessler blvd n dr, indianapolis, in 46222, usa"


# cleaning whatever ggmaps couldn't find:
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "3300 Watergate Drive, Indianapolis, IN")] = -86.250571
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "3300 Watergate Drive, Indianapolis, IN")] = 39.816205
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "101 1/2 S Washington Street, Danville, IN")] = -86.52355
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "101 1/2 S Washington Street, Danville, IN")] = 39.75974
# 
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "5310 Melbourne NA, Indianapolis, IN")] = -86.220436
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "5310 Melbourne NA, Indianapolis, IN")] = 39.847923
# 
# 
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "11230 State Road 67 NA, Moorseville, IN")] = -86.37799
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "11230 State Road 67 NA, Moorseville, IN")] = 39.59091
# 
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "N/A 34th And Massachusetts NA, Indianapolis, IN")] = -86.06463
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "N/A 34th And Massachusetts NA, Indianapolis, IN")] = 39.81837
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "E 34th St & Massachusetts Ave, Indianapolis, IN")] = -86.06463
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "E 34th St & Massachusetts Ave, Indianapolis, IN")] = 39.81837
# moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "N/A 34th And Massachusetts NA, Indianapolis, IN")] = "E 34th St & Massachusetts Ave, Indianapolis, IN"
# 
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "7001 In37 Highway, Martinsville, IN")] = -86.28636
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "7001 In37 Highway, Martinsville, IN")] = 39.5289
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "7001 Ruel W Steele Hwy, Martinsville, IN")] = -86.28636
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "7001 Ruel W Steele Hwy, Martinsville, IN")] = 39.5289
# moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "7001 In37 Highway, Martinsville, IN")] = "7001 Ruel W Steele Hwy, Martinsville, IN"
# 
# moreCleaning$lon[which(moreCleaning$FULL_ADD == "1700 S Beulah Street, Indianapolis, IN")] = -86.246367
# moreCleaning$lat[which(moreCleaning$FULL_ADD == "1700 S Beulah Street, Indianapolis, IN")] = 39.742822
