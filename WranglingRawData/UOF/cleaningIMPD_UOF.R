# install.packages("tidyverse")
library(tidyverse)
# library(ggmap) # getting the latlon of the addresses, note this cost $$ using the API


# # The downloaded UOF from IMPD comes in a large csv file, just named "IMPD_Use_of_Force.csv", it gets updated
# # # frequently, but due to it not having the updated dates in the title, might be best practice to download them and change the file name


# enter the newest downloaded UOF here,
newest_UOF_csv <-
  read_csv("../../RawData/UOF/IMPD_Use_Of_Force_downloaded_2020_10_11.csv")

# View(newest_UOF_csv)


# View(newest_UOF_csv %>% filter(is.na(STREET), is.na(STREET)))
# making a copy of the raw data
wrangledUOF.df <- newest_UOF_csv



# # # Interesting insights while cleaning the data:
# # #  there is a clear difference between user hand-typed data and
# # #   a pre-filed-choice for entering data (e.g. possibly a drop down box
# # #   for certain variables)
# # # For instance, I had to clean up the capitalization of STREET, CITY, UDTEXT24C
# # # But did not need to clean up the capitalization of UOF_REASON, UOF_TYPE, CITCHARGE_TYPE
# # #
# # # Also there were some that seemed so common there was a pre-filed-choice for things like
# # # CIT_CONDITION, OFF_CONDITION, OFF_RACE, and CIT_RACE. These seemed to only need to be made uniform
# # # once there was custom entered data
# # #
# # # Somehow there are UNKNOWN officer numbers but not UNKNOWN incident numbers, needs more indepth research.



# Notes on data:
# All missing numeric values will default to 999999 so we're not losing any observations just a few individual data cells
wrangledUOF.df <- wrangledUOF.df %>%
  # renaming variables to be more explicit
  dplyr::rename(
    CIT_SEX = SEX,
    CIT_RACE = RACE,
    OFF_NUM = OFFNUM,
    CIT_NUM = CITNUM
  ) %>%
  # mutating all of the strings for ease of wrangling
  mutate_if(is.character, str_to_upper) %>%
  mutate_if(is.character, str_squish) %>%
  
  # Renaming observations to be more explicit
  mutate(CIT_RACE = if_else(CIT_RACE == "B", "BLACK", CIT_RACE)) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "BLACE", "BLACK", CIT_RACE)) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "NATIVE AMER", "NATIVE AMERICAN", CIT_RACE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE < 18, 999999, OFF_AGE)) %>%
  
  # dealing with basics of addresses
  # assuming all of IMPD's work is only done in Indianapolis, IN
  mutate(CITY = if_else(
    stringr::str_detect(CITY, "(IND|IIN|IND|INI|IDI|INJ)"),
    "INDIANAPOLIS",
    CITY
  )) %>%
  mutate(STATE = "IN") %>%
  
  # Dealing with time
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
  
  
  # Replacing empty or missing data
  dplyr::mutate(CIT_SEX = ifelse((is.na(CIT_SEX) |
                                    CIT_SEX == "UNKNOWN"), "UNREPORTED", CIT_SEX)) %>%
  mutate(CIT_SEX = if_else(CIT_SEX == "M", "MALE", CIT_SEX)) %>%
  mutate(CIT_RACE = ifelse((is.na(CIT_RACE) |
                              CIT_RACE == "UNKNOWN" |
                              CIT_RACE == "NA"),
                           "UNREPORTED",
                           CIT_RACE
  )) %>%
  
  # Replacing any missing attribute values
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

View(wrangledUOF.df)



## ADDRESSES
## Manual cleaning

#### Skeleton of the variables for renaming address observations:
# NOTE THAT IF WE ARE USING THE STREET TO FILTER OUR RESULTS BEFORE MUTATING,
# MAKE SURE TO MUTATE STREET (OR WHICHEVER VARIABLE CHOSEN) LAST!

# wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "")] = ""
# wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "")] = ""
# wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "")] = ""
# wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "")] = ""


## Grouping all of the UOF happenings on the highway and setting them to the DOT
## This is for costs and time, since finding the actual GPS location (lat/lon)
## of the incidents would be too high at this time (but with more funding, it is possible!)

wrangledUOF.df <-
  wrangledUOF.df %>%  dplyr::mutate(STREET = ifelse(
    grepl(
      'SR37|I65|I69|I70|I465|SR-37|I-65|I-69|I-70|I-465|SR 37|I 65|I 69|I 70|I 465|INTERSTATE',
      STREET
    ),
    "INDOT",
    STREET
  )) %>%
  dplyr::mutate(STREET_N = ifelse(
    grepl(
      'SR37|I65|I69|I70|I465|SR-37|I-65|I-69|I-70|I-465|SR 37|I 65|I 69|I 70|I 465|INTERSTATE',
      STREET_N
    ),
    "INDOT",
    STREET_N
  ))



# Cleaning outliers/mispellings on street names
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10TH / REAR PARKING LOT")] = "10TH"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10TH STREET")] = "10TH"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10TH ST")] = "10TH"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == 10)] = "10TH"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10")] = "10TH"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "30TH STREET")] = "STREET"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "30TH STREET")] = "30TH"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "W 38TH")] = "38TH"

# STREET Number is repeated
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "1437 IRON TRAIL E")] = "IRON TRAIL"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "2405 MADISON AVE")] = "MADISON"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "8505 FRONGATE")] = "FRONGATE"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "8524 ADLINGTON")] = "ADLINGTON"


# Cleaning duplicate data, splitting data or ambiguos street addresses
wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "2100 N MONTCALM")] = "2100"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "2100 N MONTCALM")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "2100 N MONTCALM")] = "ST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "2100 N MONTCALM")] = "MONTCALM"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "10TH AND DEARBORN")] = "3200"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "10TH AND DEARBORN")] = "E"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "10TH AND DEARBORN")] = "ST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10TH AND DEARBORN")] = "10TH"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "PATTON DR/SUBURBAN DR")] = "3546"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "PATTON DR/SUBURBAN DR")] = "DR"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "PATTON DR/SUBURBAN DR")] = "PATTON"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "HILL VALLEY/SOUTH MERIDIAN")] = "8298"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "HILL VALLEY/SOUTH MERIDIAN")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "HILL VALLEY/SOUTH MERIDIAN")] = "ST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "HILL VALLEY/SOUTH MERIDIAN")] = "MERIDIAN"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "ROCKVILLE ROAD/MICKLEY AVE")] = "28-2"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "ROCKVILLE ROAD/MICKLEY AVE")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "ROCKVILLE ROAD/MICKLEY AVE")] = "AVE"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "ROCKVILLE ROAD/MICKLEY AVE")] = "MICKLEY"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA Wy")] = "1790"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA Wy")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA Wy")] = "AVE"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA Wy")] = "KENTUCKY"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "W KENTUCKY AV/S HARDING ST")] = "1790"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "W KENTUCKY AV/S HARDING ST")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "W KENTUCKY AV/S HARDING ST")] = "AVE"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "W KENTUCKY AV/S HARDING ST")] = "KENTUCKY"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "JACKSON ST/SOUTH MERIDIAN")] = "250"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "JACKSON ST/SOUTH MERIDIAN")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "JACKSON ST/SOUTH MERIDIAN")] = "ST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "JACKSON ST/SOUTH MERIDIAN")] = "MERIDIAN"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "JACKSON PL. & MERIDIAN ST")] = "250"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "JACKSON PL. & MERIDIAN ST")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "JACKSON PL. & MERIDIAN ST")] = "ST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "JACKSON PL. & MERIDIAN ST")] = "MERIDIAN"


wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "LAFAYETTE RD/GEORGETOWN RD")] = "4320"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "LAFAYETTE RD/GEORGETOWN RD")] = ""
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "LAFAYETTE RD/GEORGETOWN RD")] = "RD"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "LAFAYETTE RD/GEORGETOWN RD")] = "Layfayette"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "1437 IRON TRAIL E")] = "1437"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "1437 IRON TRAIL E")] = "E"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "1437 IRON TRAIL E")] = "TRAIL"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "1437 IRON TRAIL E")] = "IRON"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "34TH AND MASSACHUSETTS")] = "5991"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "34TH AND MASSACHUSETTS")] = ""
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "34TH AND MASSACHUSETTS")] = "AVE"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "34TH AND MASSACHUSETTS")] = "MASSACHUSETTS"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "10TH STREET/LASALLE STREET")] = "3301"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "10TH STREET/LASALLE STREET")] = "EAST"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "10TH STREET/LASALLE STREET")] = "STREET"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10TH STREET/LASALLE STREET")] = "10TH"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA WY")] = "1806"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA WY")] = ""
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA WY")] = "AVE"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "KENTUCKY AVE/MINNESOTA WY")] = "KENTUCKY"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "E. 75TH ST. AND JOHNSON RD.")] = "6801"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "E. 75TH ST. AND JOHNSON RD.")] = "E"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "E. 75TH ST. AND JOHNSON RD.")] = "ST"


# using the INCNUM for more precision on these outliers, when it doesn't have a repeating pattern this is an option
# issues, but since magic numbers are horrifying, we won't do that

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "IRON TRAILS W." &
                                wrangledUOF.df$STREET_N == 1409)] = "1409"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "IRON TRAILS W." &
                                wrangledUOF.df$STREET_N == 1409)] = "W"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "IRON TRAILS W." &
                                wrangledUOF.df$STREET_N == 1409)] = "TRAIL"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "IRON TRAILS W." &
                              wrangledUOF.df$STREET_N == 1409)] = "IRON"


wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "BEL DAR" &
                                wrangledUOF.df$STREET_N == 5850)] = "WEST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "BEL DAR" &
                              wrangledUOF.df$STREET_N == 5850)] = "BAR DEL"

# INCNUM == 37819 == 71ST	WOODLAND	DRIVE	W (which I assume means around 71st and Woodland Dr) ~= 5941 w 71s
wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "WOODLAND" &
                                wrangledUOF.df$STREET_N == "71ST")] = "5941"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "WOODLAND" &
                                wrangledUOF.df$STREET_N == "71ST")] = "WEST"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "WOODLAND" &
                                wrangledUOF.df$STREET_N == "71ST")] = "STREET"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "WOODLAND" &
                              wrangledUOF.df$STREET_N == "71ST")] = "71ST"

wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "WEST 36TH" &
                                wrangledUOF.df$STREET_N == 1049)] = "WEST"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "WEST 36TH" &
                              wrangledUOF.df$STREET_N == 1049)] = "36TH"




## !! IMPORTANT !! ##
# Any addresses that are not reported will be defaulted to
# the downtown district building of IMPD (39 Jackson Place)
# AND all highways will be the DOT (100 N Senate AVE)

wrangledUOF.df$STREET[which(is.na(wrangledUOF.df$STREET_N) &
                              is.na(wrangledUOF.df$STREET))] = "IMPD"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "")] = "IMPD"
wrangledUOF.df$STREET[which(is.na(wrangledUOF.df$STREET))] = "IMPD"

wrangledUOF.df$STREET[which((wrangledUOF.df$STREET_N == 465) &
                              (wrangledUOF.df$STREET == "ALLISONVILLE"))] = "INDOT"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "MILE MARKER 84")] = "INDOT"



wrangledUOF.df <- wrangledUOF.df %>%
  # changing the INDOT placeholder to be the actual downtown INDOT location
  mutate(
    STREET_N = ifelse(STREET == "INDOT", "100", STREET_N),
    STREET_G = ifelse(STREET == "INDOT", "N", STREET_G),
    STREET_T = ifelse(STREET == "INDOT", "Ave", STREET_T),
    CITY = ifelse(STREET == "INDOT", "INDIANAPOLIS", CITY),
    STREET = ifelse(STREET == "INDOT", "Senate", STREET)
  ) %>%  mutate(
    STREET_N = ifelse(STREET_N == "INDOT", "100", STREET_N),
    STREET_G = ifelse(STREET_N == "INDOT", "N", STREET_G),
    STREET_T = ifelse(STREET_N == "INDOT", "Ave", STREET_T),
    CITY = ifelse(STREET_N == "INDOT", "INDIANAPOLIS", CITY),
    STREET = ifelse(STREET_N == "INDOT", "Senate", STREET)
  ) %>%
  # changing the IMPD placeholder to be the actual downtown IMPD location
  mutate(
    STREET_N = ifelse(STREET == "IMPD", "39", STREET_N),
    STREET_G = ifelse(STREET == "IMPD", "W", STREET_G),
    STREET_T = ifelse(STREET == "IMPD", "Place", STREET_T),
    CITY = ifelse(STREET == "IMPD", "INDIANAPOLIS", CITY),
    STREET = ifelse(STREET == "IMPD", "Jackson", STREET)
  ) %>%
  mutate(
    STREET_N = ifelse(STREET_N == "IMPD", "39", STREET_N),
    STREET_G = ifelse(STREET_N == "IMPD", "W", STREET_G),
    STREET_T = ifelse(STREET_N == "IMPD", "Place", STREET_T),
    CITY = ifelse(STREET_N == "IMPD", "INDIANAPOLIS", CITY),
    STREET = ifelse(STREET_N == "IMPD", "Jackson", STREET)
  )


wrangledUOF.df <- wrangledUOF.df %>%
  # removing the na that will be produced when the street_g is comibined into full address
  mutate(STREET_G = ifelse(is.na(STREET_G), " ", STREET_G)) %>%
  unite(STREET_ADD, STREET_N, STREET_G, STREET, STREET_T,
        sep = " ") %>%
  unite(FULL_ADD, STREET_ADD,
        CITY, STATE, sep = ", ") %>%
  ### ORGANIZING THE DATA FOR EASE OF VIEWING
  select(
    INCNUM,
    OFF_NUM,
    UOF_FORCE_TYPE,
    UOF_REASON,
    CIT_ARRESTED,
    CITCHARGE_TYPE,
    DISPOSITION,
    CIT_WEAPON_TYPE,
    OFF_SEX,
    CIT_SEX,
    OFF_RACE,
    CIT_SEX,
    OFF_AGE,
    CIT_AGE,
    YMD_TM,
    everything()
  )

write_csv(wrangledUOF.df,
          "../../CleaningRawData/CleanedDatasets/UOF/wrangledUOF_2020_October.csv")
write_csv(
  wrangledUOF.df,
  "../../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/wrangledUOF_2020_October.csv"
)

write_csv(
  wrangledUOF.df,
  "../../ShinyProjects/TestingDeploy/Datasets/UOF/wrangledUOF_2020_October.csv"
)
