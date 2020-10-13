# dataset found here
# http://data.indy.gov

# combing cleaned and organized UOF from cleaningIMPD_UOF.R with newest lat/lon data from gatherUOF_lat_lon_ggmaps.R 
# lat/lon files being saved in Datasets/UOF_lat_lon_incnum....csv
library(tidyverse)

# only used if CURL error HTTP2 occurs while trying to connect to ggmap api
# library(httr)
# httr::set_config(httr::config(http_version = 0))


# googleApiKey = "ENTER_YOUR_API_KEY_HERE_IF_YA_WANT"
# register_google(key=googleApiKey)#


useOfForce.df <- readr::read_csv("Datasets/extremely_simplified_force_type_and_reason_and_arrest_12_2019.csv")

# View(useOfForce.df)

# joining all of the lat, lon, and incnums
lat_lon_inc_0 <- readr::read_csv("Datasets/UOF_lat_lon_incnum_up_to_july_2019.csv")
lat_lon_inc_1 <- readr::read_csv("Datasets/UOF_lat_lon_incnum_07_19_to_12_19.csv")

# View(lat_lon_inc_0)
# View(lat_lon_inc_1)
lat_lon_inc <- full_join(lat_lon_inc_0, lat_lon_inc_1)

# View(lat_lon_inc)

# full join leaves NA values for lat and lon in the original dataset
new_UOF_with_most_current_lat_lon <- full_join(useOfForce.df, lat_lon_inc)

# glimpse(new_UOF_with_most_current_lat_lon)
# View(new_UOF_with_most_current_lat_lon)

#### SAVING THE CLEANED DATA BEFORE ANY SIMPLIFICATIONS

# Saving for local development
write_csv(new_UOF_with_most_current_lat_lon,
          "Datasets/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv")

write_csv(new_UOF_with_most_current_lat_lon,
          "../CleanData/UOF/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv")

# Saving for Shiny development
write_csv(
  new_UOF_with_most_current_lat_lon,
  "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv"
)

# testing write
x <- read_csv("Datasets/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv")
x <- read_csv("../CleanData/UOF/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv")
z <- read_csv("../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv")

# View(x)
# View(z)
