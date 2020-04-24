# playing with some visual mapping in leaflet and ggmaps

# more info on the datset
# https://xmaps.indy.gov/arcgis/rest/services/OpenData/OpenData_NonSpatial/MapServer/14
# dataset found here:
# http://data.indy.gov


# mapping
library(ggmap)
library(tidyverse)


#
# googleApiKey = "ENTER_YOUR_API_KEY_HERE_IF_YA_WANT"
# register_google(key=googleApiKey)


useOfForce.df <- readr::read_csv("../CleanData/cleanedUOF_data.csv")



# viewing raw dataset
# View(useOfForce.df)

# grabbing quick info
# glimpse(useOfForce.df)


# making a copy of the data, so we're not importing it
# multiple times and slowing down the analysis
geoUOF.df <- useOfForce.df

# just verifying the copy works
# View(geoUOF.df)


# removing the na that will be produced when the street_g is comibined into full address
geoUOF.df$STREET_G[which(is.na(geoUOF.df$STREET_G))] = " "




# making new address column for google maps
geoUOF.df <-
  geoUOF.df %>%
  unite(STREET_ADD, STREET_N, STREET_G, STREET, STREET_T,
        sep = " ") %>%
  unite(FULL_ADD, STREET_ADD,
        CITY, STATE, sep = ", ")

# View(geoUOF.df)


# grabbing just the unique location data
# for ease of google maps API


# getting all of the actual longitudes and latitudes from google maps
# for use from leaflet
# View(geoUOF.df)


## breaking these up because google maps api crashes due to curl memory on the whole thing

# already made and saved
# geoCodedAddressesUOF_1_10000 <-
#   geoUOF.df[1:10000, ] %>%
#   mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, everything()) %>%
#   write_csv("geoCodedAddressesUOF_1_10000.csv")
#
# geoCodedAddressesUOF_10001_20000 <-
#   geoUOF.df[10001:20000, ] %>% mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, everything()) %>%
#   write_csv("geoCodedAddressesUOF_10001_20000.csv")
#
#
#
# geoCodedAddressesUOF_20001_30000 <-
#   geoUOF.df[20001:30000, ] %>% mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, everything()) %>%
#   write_csv("geoCodedAddressesUOF_20001_30000.csv")
#

# geoCodedAddressesUOF_30001_40000 <-
#   geoUOF.df[30001:40000, ] %>% mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, everything()) %>%
#   write_csv("geoCodedAddressesUOF_30001_40000.csv")

# geoCodedAddressesUOF_40001_50000 <-
#   geoUOF.df[40001:50000, ] %>% mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, everything()) %>%
#   write_csv("geoCodedAddressesUOF_40001_50000.csv")

# geoCodedAddressesUOF_50001_63413 <-
#   geoUOF.df[50001:63413, ] %>% mutate_geocode(FULL_ADD) %>%
#   select(lon, lat, everything()) %>%
#   write_csv("geoCodedAddressesUOF_50001_63413.csv")


# Some errors from Google maps API,
# The errors observed seemed trivial and fixed in Google Maps
# eveything *should* be alright, due to the cleaning process.

# "3600   Kline Driv..." not uniquely geocoded, using "3600 kline n dr, indianapolis, in 46226, usa"
# "10 S Delaware Str..." not uniquely geocoded, using "10 delaware st, indianapolis, in 46204, usa"
# "5200 S Madison Av..." not uniquely geocoded, using "5200 madison ave, indianapolis, in 46227, usa"
# "2900 N Kessler Bl..." not uniquely geocoded, using "2900 w kessler blvd n dr, indianapolis, in 46222, usa"



# then combine all of these with join_full()
# have since moved the saved files for organization, hense the path differences
geo_1 <- read_csv("../CleanData/UOF/geoCodedAddressesUOF_1_10000.csv")
geo_2 <- read_csv("../CleanData/UOF/geoCodedAddressesUOF_10001_20000.csv")
geo_3 <- read_csv("../CleanData/UOF/geoCodedAddressesUOF_20001_30000.csv")
geo_4 <- read_csv("../CleanData/UOF/geoCodedAddressesUOF_30001_40000.csv")
geo_5 <- read_csv("../CleanData/UOF/geoCodedAddressesUOF_40001_50000.csv")
geo_6 <- read_csv("../CleanData/UOF/geoCodedAddressesUOF_50001_63413.csv")


# fastest way I've seen without having a nested, hard to read mess (but would love a better way)
join_1 <- full_join(geo_1, geo_2)
join_2 <- full_join(join_1, geo_3)
join_3 <- full_join(join_2, geo_4)
join_4 <- full_join(join_3, geo_5)

final_join <- full_join(join_4, geo_6) %>%
  write_csv("cleanedUOF_withGeoLocation.csv")


just_lat_and_lon <- final_join %>%
  select(lat, lon, OBJECTID) %>%
  write_csv("UOF_lat_lon_objectid.csv")
# verifying the joins worked
# View(final_join)


# fixing some outliers that came from weird lat/lon
moreCleaning <- read_csv("../CleanData/UOF/cleanedUOF_withGeoLocation.csv")
View(moreCleaning)




moreCleaning$lon[which(moreCleaning$FULL_ADD == "3300   Watergate Drive, Indianapolis, IN")] = -86.250571
moreCleaning$lat[which(moreCleaning$FULL_ADD == "3300   Watergate Drive, Indianapolis, IN")] = 39.816205

moreCleaning$lon[which(moreCleaning$FULL_ADD == "101 1/2 S Washington Street, Danville, IN")] = -86.52355
moreCleaning$lat[which(moreCleaning$FULL_ADD == "101 1/2 S Washington Street, Danville, IN")] = 39.75974


moreCleaning$lon[which(moreCleaning$FULL_ADD == "5310   Melbourne NA, Indianapolis, IN")] = -86.220436
moreCleaning$lat[which(moreCleaning$FULL_ADD == "5310   Melbourne NA, Indianapolis, IN")] = 39.847923



moreCleaning$lon[which(moreCleaning$FULL_ADD == "11230   State Road 67 NA, Moorseville, IN")] = -86.37799
moreCleaning$lat[which(moreCleaning$FULL_ADD == "11230   State Road 67 NA, Moorseville, IN")] = 39.59091


moreCleaning$lon[which(moreCleaning$FULL_ADD == "N/A   34th And Massachusetts NA, Indianapolis, IN")] = -86.06463
moreCleaning$lat[which(moreCleaning$FULL_ADD == "N/A   34th And Massachusetts NA, Indianapolis, IN")] = 39.81837
moreCleaning$lon[which(moreCleaning$FULL_ADD == "E 34th St & Massachusetts Ave, Indianapolis, IN")] = -86.06463
moreCleaning$lat[which(moreCleaning$FULL_ADD == "E 34th St & Massachusetts Ave, Indianapolis, IN")] = 39.81837
moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "N/A   34th And Massachusetts NA, Indianapolis, IN")] = "E 34th St & Massachusetts Ave, Indianapolis, IN"


moreCleaning$lon[which(moreCleaning$FULL_ADD == "7001   In37 Highway, Martinsville, IN")] = -86.28636
moreCleaning$lat[which(moreCleaning$FULL_ADD == "7001   In37 Highway, Martinsville, IN")] = 39.5289

moreCleaning$lon[which(moreCleaning$FULL_ADD == "7001 Ruel W Steele Hwy, Martinsville, IN")] = -86.28636
moreCleaning$lat[which(moreCleaning$FULL_ADD == "7001 Ruel W Steele Hwy, Martinsville, IN")] = 39.5289
moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "7001   In37 Highway, Martinsville, IN")] = "7001 Ruel W Steele Hwy, Martinsville, IN"

moreCleaning$lon[which(moreCleaning$FULL_ADD == "1700 S Beulah Street, Indianapolis, IN")] = -86.246367
moreCleaning$lat[which(moreCleaning$FULL_ADD == "1700 S Beulah Street, Indianapolis, IN")] = 39.742822



# defaulting some to be the DOT downtown location
moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "73 W I-70 W NA, Indianapolis, IN")] = "100 N Pennsylvania NA, Indianapolis, IN"

moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "5000 W Interstate 74 Wb Highway, Indianapolis, IN")] = "100 N Pennsylvania St, Indianapolis, IN"

moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "3500 S Interstate 65 S Highway, Indianapolis, IN")] = "100 N Pennsylvania St, Indianapolis, IN"

moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "SR 13 N Interstate 69 Highway, Lapel, IN")] = "100 N Pennsylvania St, Indianapolis, IN"

moreCleaning$FULL_ADD[which(moreCleaning$FULL_ADD == "100 N Pennsylvania NA, Indianapolis, IN")] = "100 N Pennsylvania St, Indianapolis, IN"

moreCleaning$lon[which(moreCleaning$FULL_ADD == "100 N Pennsylvania St, Indianapolis, IN")] = -86.15646
moreCleaning$lat[which(moreCleaning$FULL_ADD == "100 N Pennsylvania St, Indianapolis, IN")] = 39.76852

write_csv(moreCleaning, "../CleanData/UOF/cleanedUOF_withGeoLocation.csv")
