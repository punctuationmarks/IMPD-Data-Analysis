
# Libraries ---------------------------------------------------------------

# install.packages(c("shiny", "tidyverse", "leaflet", "DT", "shinycssloaders", "shinythemes"))
library(tidyverse)
library(shiny)
library(tidyverse)
library(leaflet) # maps
library(DT) # for table rendering/prettifying
library(shinycssloaders) # for having a loading page to keep the audience entertained
library(shinythemes) # for styling overall app

# Dataframes ---------------------------------------------------------------

# raw data for tables, wrangled and renamed below with the convention "name.df"
UOF_csv <-
  read_csv("./Datasets/UOF/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv")

complaints_csv <-
  read_csv("./Datasets/Complaints/cleanedComplaint_data.csv")

UCR_csv <-
  read_csv("./Datasets/UCR/simplied_crime_categories_2014_2019.csv")





# Wrangling and naming conventions ----------------------------------------


UOF.df <-
  UOF_csv %>%  mutate(CIT_RACE = if_else(CIT_RACE == "BLACE", "BLACK", CIT_RACE)) %>% select(-c(lat, lon))

UCR.df <- UCR_csv

complaints.df <- (
  complaints_csv %>%
    select(
      ALG_CLASS,
      FINDING,
      ALLEGATION,
      INCIDENT_TYPE,
      SERVICE_TYPE,
      OFF_YR_EMPLOY,
      OFF_RACE,
      OFF_SEX,
      OFF_AGE,
      CIT_RACE,
      CIT_SEX,
      CIT_AGE,
      OCCURRED_DATE_AND_TIME,
      OCCURRED_YEAR,
      OCCURRED_MONTH,
      everything()
    ) %>%
    arrange(ALG_CLASS, FINDING)
)


UCR_with_year.df <- UCR.df %>%
  select(YEAR, CRIME) %>%
  group_by(YEAR, CRIME) %>%
  filter(YEAR >= 2014) %>%
  count(CRIME) %>%
  rename("NUM_OF_OCCURANCES" = n)


# UI input options --------------------------------------------------------


### dataframes -----------------------------------------------------------


# unique zipcodes in the data, for the input choices
zipCodes <-
  UOF.df %>% filter(zip < 60000 &
                      zip > 40000) %>%  distinct(zip) %>% arrange(zip)

citizenRaces <-
  UOF.df %>% mutate(CIT_RACE = if_else(CIT_RACE == "BLACE", "BLACK", CIT_RACE)) %>%  distinct(CIT_RACE) %>% drop_na() %>%  arrange(CIT_RACE)

officerRaces <-
  UOF.df %>%  distinct(OFF_RACE) %>% drop_na() %>%  arrange(OFF_RACE)



# Global options ----------------------------------------------------------


# options with this library: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
overallTheme <- shinytheme("sandstone")

# Options for spinners while the page loads
options(
  spinner.color.background = "#F5F5F5",
  spinner.color = "#000000",
  spinner.type = 3
)


### lists for inputs -----------------------------------------------------


inputCitizenRaces <- c("All", citizenRaces)
inputOfficerRaces <- c("All", officerRaces)

inputSexes <- list("All", "FEMALE", "MALE", "UNREPORTED")

inputYears <- list(
  "All years (2014-2019)" = "All",
  2014 ,
  2015 ,
  2016 ,
  2017 ,
  2018 ,
  2019 ,
  "UNREPORTED"
)


inputQuarters <- list(
  "All",
  "QT1" = 1,
  "QT2" = 2,
  "QT3" = 3,
  "QT4" = 4
)

inputHours <- list(
  "All day" = "All",
  '00:00' = 0,
  '01:00' = 1,
  '02:00' = 2,
  '03:00' = 3,
  '04:00' = 4,
  '05:00' = 5,
  '06:00' = 6,
  '07:00' = 7,
  '08:00' = 8,
  '10:00' = 10,
  '11:00' = 11,
  '12:00' = 12,
  '13:00' = 13,
  '14:00' = 14,
  '15:00' = 15,
  '16:00' = 16,
  '17:00' = 17,
  '18:00' = 18,
  '19:00' = 19,
  '20:00' = 20,
  '21:00' = 21,
  '22:00' = 22,
  '23:00' = 23,
  '24:00' = 24
)


inputZipCodes <- c("All", zipCodes)

inputGraphAxis <- names(UOF.df)



### Palettes for mapping ----------------------------------------------------

random_hex_colors <- c(
  '#7b71b3', '#540564', '#0a41ba', '#4a83d8',
  '#b02092', '#b78642', '#923a26', '#4976a5',
  '#2e9100', '#baecee', '#68deab', '#9e6ad5',
  '#42b971', '#b10eba', '#b7d348', '#81b807',
  '#e41b40', '#541020', '#3d56b9', '#7a3d34',
  '#6333a7', '#bd5a55', '#2636c9', '#33e7e4',
  '#ed5611', '#1ad5aa', '#ad3502', '#82a2e9',
  '#184279', '#a33dab', '#8b576b', '#e2e7b7',
  '#752ce3', '#51e018', '#a7adc3', '#ae2189',
  '#6b51c9', '#b015d9', '#7978ee', '#47b3c0',
  '#a20ac6', '#854833', '#b20042', '#b72b71',
  '#2d9a03', '#ba8041', '#5284a5', '#63ed31',
  '#b43a0a', '#52c26a', '#5de930', '#bd77d5',
  '#7a69dc', '#bccd60', '#224779', '#686763',
  '#578dea', '#67ad28', '#b6bd4c', '#22e797'
)



# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# RColorBrewer::display.brewer.all()


# # Pass the palette function a data vector to get the corresponding colors
# reason for random hex values is due to the qty of zip codes
palPaired_byZip <- colorFactor(palette = random_hex_colors,
                               domain = UOF.df$zip,
                               ordered = TRUE)


palPaired_byCitzenRace <- colorFactor(palette = "Set1",
                                      domain = UOF.df$CIT_RACE,
                                      ordered = TRUE)

palPaired_byCitzenSex <-
  colorFactor(palette = "Set1",
              domain = UOF.df$CIT_SEX)


palPaired_byYear <- colorFactor(palette = "Set1",
                                domain = UOF.df$OCCURRED_YEAR)



