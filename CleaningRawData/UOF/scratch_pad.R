install.packages(c("tidyverse", "revgeo"))
library(tidyverse)
# for reverse geocoding to gather zipcode
library(revgeo)

### so this works, but it's very slow, better concept might be to split it up by the 1k observersations
# IT MIGHT NOT BE FINISHED IN TIME, BEEN RUNNING 2.75 HOURS and it started to throttle (much earlier)
uof_cleaned_with_lat_lon <-
  read_csv("Datasets/UOF_all___with_lat_lon_up_to_dec_2019.csv")

lat_lon_for_reverse_geo <- uof_cleaned_with_lat_lon %>%
  # making lat and lon more explcit for revgeo
  rename(latitude = lat, longitude = lon) %>%
  select(latitude, longitude, INCNUM) %>%
  unique()

# View(lat_lon_for_reverse_geo)


# lat_lon_incnum_zip <- revgeo(lat_lon_for_reverse_geo$longitude, lat_lon_for_reverse_geo$latitude, provider =  'photon', output = 'frame') %>%
#   mutate(index = row_number()) %>%
#   select(index, zip) %>%
#   left_join(lat_lon_for_reverse_geo, by="index") %>%
#   # dropping the index column
#   select(-index)
# # View(lat_lon_incnum_zip)

# write_csv(lat_lon_incnum_zip, "ScratchArea/testing_lat_lon_zip_UOF.csv")
# x <- read_csv("ScratchArea/testing_lat_lon_zip_UOF.csv")
# View(x)

# small batch due to throttling of the reverse geo api
# we're using index column as a joiner between revgeo's zipcodes, doing it here since we're basing the index off of row number
# this will be deleted after
batch_1 <-
  lat_lon_for_reverse_geo[1:1000, ] %>% mutate(index = row_number())
batch_2 <-
  lat_lon_for_reverse_geo[1001:2000, ] %>% mutate(index = row_number())
# View(batch_2)
batch_3 <-
  lat_lon_for_reverse_geo[2001:3000, ] %>% mutate(index = row_number())
batch_4 <-
  lat_lon_for_reverse_geo[3001:4000, ] %>% mutate(index = row_number())
batch_5 <-
  lat_lon_for_reverse_geo[4001:5000, ] %>% mutate(index = row_number())
batch_6 <-
  lat_lon_for_reverse_geo[5001:6000, ] %>% mutate(index = row_number())
# View(batch_6)
# # to the end
batch_7 <-
  lat_lon_for_reverse_geo[6001:6568, ] %>% mutate(index = row_number())



lat_lon_incnum_zip_1 <-
  revgeo(batch_1$longitude,
         batch_1$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_1, by = "index") %>%
  # dropping the index column
  select(-index)

write_csv(lat_lon_incnum_zip_1, "ScratchArea/uof_lat_lon_incnum_zip_1.csv")

# View(lat_lon_incnum_zip_1)

lat_lon_incnum_zip_2 <-
  revgeo(batch_2$longitude,
         batch_2$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>% 
  select(index, zip) %>%
  left_join(batch_2, by = "index") %>%
  # dropping the index column
  select(-index)


write_csv(lat_lon_incnum_zip_2,
          "ScratchArea/uof_lat_lon_incnum_zip_2.csv")
# View(lat_lon_incnum_zip_2)



#
#
#
lat_lon_incnum_zip_3 <-
  revgeo(batch_3$longitude,
         batch_3$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_3, by = "index") %>%
  # dropping the index column
  select(-index)


write_csv(lat_lon_incnum_zip_3,
          "ScratchArea/uof_lat_lon_incnum_zip_3.csv")

#
#
#
lat_lon_incnum_zip_4 <-
  revgeo(batch_4$longitude,
         batch_4$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_4, by = "index") %>%
  # dropping the index column
  select(-index)
write_csv(lat_lon_incnum_zip_4,
          "ScratchArea/uof_lat_lon_incnum_zip_4.csv")
#
#
#
#
lat_lon_incnum_zip_5 <-
  revgeo(batch_5$longitude,
         batch_5$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_5, by = "index") %>%
  # dropping the index column
  select(-index)
write_csv(lat_lon_incnum_zip_5,
          "ScratchArea/uof_lat_lon_incnum_zip_5.csv")
#
#
#
#
lat_lon_incnum_zip_6 <-
  revgeo(batch_6$longitude,
         batch_6$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_6, by = "index") %>%
  # dropping the index column
  select(-index)
write_csv(lat_lon_incnum_zip_6,
          "ScratchArea/uof_lat_lon_incnum_zip_6.csv")

lat_lon_incnum_zip_7 <-
  revgeo(batch_7$longitude,
         batch_7$latitude,
         provider =  'photon',
         output = 'frame') %>%
  mutate(index = row_number()) %>%
  select(index, zip) %>%
  left_join(batch_7, by = "index") %>%
  # dropping the index column
  select(-index)
write_csv(lat_lon_incnum_zip_7,
          "ScratchArea/uof_lat_lon_incnum_zip_7.csv")
#
#

zip_1 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_1.csv")
zip_2 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_2.csv")
zip_3 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_3.csv")
zip_4 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_4.csv")
zip_5 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_5.csv")
zip_6 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_6.csv")
zip_7 = read_csv("ScratchArea/uof_lat_lon_incnum_zip_7.csv")

# joining two at a time
zip_joined_1 = full_join(zip_1, zip_2)
zip_joined_2 = full_join(zip_3, zip_4)
zip_joined_3 = full_join(zip_5, zip_6)

# joining combined joins with each other
zip_joined_4 = full_join(zip_joined_1, zip_7)
zip_joined_5 = full_join(zip_joined_4, zip_joined_2)
zip_joined_5 = full_join(zip_joined_5, zip_joined_3)

View(zip_joined_5)

# retuning all of the zip codes that are longer than 5 digits
# View(zip_joined_5 %>%  filter(stringr::str_length(zip_joined_5$zip) > 5))
# View(zip_joined_5 %>%  filter(zip == "Postcode Not Found"))

uof_with_zip_lat_lon <- zip_joined_5 %>%
  # changing revgeo's NA value
  mutate(zip = ifelse(zip == "Postcode Not Found", NA, zip)) %>%
  mutate(zip = ifelse(
    # removing any extra specific zip codes (for fomatting)
    nchar(zip, allowNA = TRUE, keepNA = TRUE) > 5,
    # forcing the zip code to be a max of 5 characters
    nchar(zip, allowNA = TRUE, keepNA = TRUE) == 5,
    zip
  ))

# View(uof_with_zip_lat_lon)
# View(uof_with_zip_lat_lon %>%  filter(stringr::str_length(uof_with_zip_lat_lon$zip) > 5))
# View(uof_with_zip_lat_lon %>%  filter(is.na(zip)))
# # could also try to use this for a placeholder instead of NA (issue is R reduces this down to a single 0)
# # and might make it more convoluted
# View(uof_with_zip_lat_lon %>%  filter(zip == 00000))




# combining the lat, lon and zip with the cleaned UOF data
# View(uof_with_zip_lat_lon)
write_csv(uof_with_zip_lat_lon, "ScratchArea/uof_lat_lon_incnum_zip_12_2019.csv")

x <- read_csv("ScratchArea/uof_lat_lon_incnum_zip_12_2019.csv")
y <- read_csv("Datasets/UOF_all___with_lat_lon_up_to_dec_2019.csv")

z <- full_join(x, y) 




# View(z)
write_csv(z, "Datasets/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv")


a <- read_csv("Datasets/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv") 

View(a)


# joining the full cleaned data with the lat/lon and zip data
# joined_officer_involved_shootngs_lat_lon_zip <- full_join(lat_lon_incnum_zip, y)
# View(joined_officer_involved_shootngs_lat_lon_zip)
# write_csv(joined_officer_involved_shootngs_lat_lon_zip, "Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
# testing_write <- read_csv("Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv")
# View(testing_write)
