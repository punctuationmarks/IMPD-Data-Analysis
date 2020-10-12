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

# write_csv(geoCodedAddresses_officer_involved_shootings, "geoCodedAddresses_officer_involved_shootings_12_2019.csv")
# write_csv(geoCodedAddresses_officer_involved_shootings, "Datasets/lat_lon_incnum_officer_involved_shootings_12_2019.csv")




# once there are more shootings reported, then you can combine them here:
# (same proceedure as UOF)

# joining all of the lat, lon, and incnums
# lat_lon_inc_0 <- readr::read_csv("")
# lat_lon_inc_1 <- readr::read_csv("")


# View(lat_lon_inc_0)
# View(lat_lon_inc_1)
# lat_lon_inc <- full_join(lat_lon_inc_0, lat_lon_inc_1)

lat_lon_inc <- read_csv("Datasets/lat_lon_incnum_officer_involved_shootings_12_2019.csv")
# View(lat_lon_inc)
# 39.764702, -86.116329




# full join leaves NA values for lat and lon in the original dataset
officer_involved_shootings_with_most_current_lat_lon <- full_join(wrangledUOF.df, lat_lon_inc)
# View(new_UOF_with_most_current_lat_lon)




# THIS IS USED TO MANUALLY FIX SOME OF GGMAPS MISSED LAT/LON
# THIS WILL BE INTERGRADED IN SCRIPT ABOVE ONCE MORE REPORTS ARE FILLED
# View(officer_involved_shootings_with_most_current_lat_lon %>% filter(is.na(lat)))

x <- officer_involved_shootings_with_most_current_lat_lon %>% 
  mutate(latitude = ifelse(STREET == "NEWTON" & STREET_N == 2800, 39.764702, latitude)) %>%
  mutate(longitude = ifelse(STREET == "NEWTON" & STREET_N == 2800, -86.116329, longitude)) %>% 
  mutate(latitude = ifelse(STREET == "AQUEDUCT" & STREET_N == 2300, 39.799762, latitude)) %>%
  mutate(longitude = ifelse(STREET == "AQUEDUCT" & STREET_N == 2300, -86.178805, longitude))

# View(x)



# write_csv(x, "Datasets/cleaned_officer_involved_shootings_with_lat_lon_12_2019.csv")
y <- read_csv("Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
View(y)

# for reverse geocoding to gather zipcode
library(revgeo)


lat_long_for_reverse_geo <- y %>% 
  select(latitude, longitude, INCNUM) %>% 
  unique() %>%
  # we're using index as a joiner between revgeo's zipcodes
  mutate(index=row_number())

lat_long_incnum_zip <- revgeo(lat_long_for_reverse_geo$longitude, lat_long_for_reverse_geo$latitude, provider =  'photon', output = 'frame') %>% 
  mutate(index = row_number()) %>%
  select(index, zip) %>% 
  left_join(lat_long_for_reverse_geo, by="index") %>% 
  # dropping the index column
  select(-index)

# View(lat_long_incnum_zip)


joined_officer_involved_shootngs_lat_lon_zip <- full_join(lat_long_incnum_zip, y)
# View(joined_officer_involved_shootngs_lat_lon_zip)
# write_csv(joined_officer_involved_shootngs_lat_lon_zip, "Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
# testing_write <- read_csv("Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
# View(testing_write)