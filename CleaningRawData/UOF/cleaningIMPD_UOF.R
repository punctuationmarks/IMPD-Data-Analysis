library(tidyverse)
# library(ggmap) # getting the latlon of the addresses, note this cost $$ using the API


# # # The downloaded UOF from IMPD comes in a large csv file, just named "IMPD_Use_of_Force.csv", it gets updated
# # # frequently, but due to it not having the updated dates in the title, might be best practice to download them and change the file name


# enter the newest downloaded UOF here,
newest_UOF_csv <-
  read_csv("../../RawData/UOF/IMPD_UOF_downloaded_2020_04_24.csv")

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
# # # Somehow there are UNNOWN officer numbers but not UNNOWN incident numbers, needs more indepth research.



# Note: all missing numeric values will default to 999999 so we're not losing any data
wrangledUOF.df <- wrangledUOF.df %>%
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
  # dealing WITH time:
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
  mutate(
    CIT_RACE = if_else(
      CIT_RACE == "UNKNOWN" |
        CIT_RACE == "UNREPORTED",
      "UNREPORTED RACE",
      CIT_RACE
    )
  ) %>%
  # dealing WITH missing values
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
## of the incidents would be too high at this time (but WITH more funding, it is possible!)

# script to filter to your desires
# so you can grab anything from the select variable
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
  # Changing the INDOT and IMPD placeholders to be theaddresses to INDOT and IMPD
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



#### SIMPLIFYING THE FORCE TYPE AND UOF REASON
simplifed_force_type_and_reason_and_arrest <-
  wrangledUOF.df %>%
  # using regex in str_detect() to combine ufo type categories for ease of graphing and understanding
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(FOGGER)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(CS/CO)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-clearout OC)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-burning CS)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-CS GRENADE)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(FLASH BANG)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(PEPPER BALL)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(PEPPERBALL)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-BPS GAS)"),
      "SMOKE GRENADE/TEAR GAS/FLASH BANG",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-TASER)"),
    "Taser",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(HANDCUFFING)"),
    "HANDCUFFING",
    UOF_FORCE_TYPE
  )) %>%  # make sure HANDCUFFING comes first (since some are "PHYSICAL HANDCUFFING")
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(BODY WEIGTH LEVERAGE)"),
    "PHYSICAL",
    UOF_FORCE_TYPE
  )) %>%  # make sure HANDCUFFING comes first (since some are "PHYSICAL HANDCUFFING")
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(PHYSICAL)"),
    "PHYSICAL",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-LEG SWEEP)"),
    "PHYSICAL",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(HANDS, FIST, FEET)"),
    "PHYSICAL",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Joint MANIPULATION)"),
    "PHYSICAL",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-BEAN BAG)"),
    "BEAN BAG",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-BATON)"),
    "BATON",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(LESS LETHAL-OTHER)"),
    "OTHER",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(OTHER)"),
    "OTHER",
    UOF_FORCE_TYPE
  )) %>%
  # GROUPING THE TYPE OF CHARGE FOR SPARKING THE UOF
  mutate(CITCHARGE_TYPE = stringr::str_squish(CITCHARGE_TYPE)) %>%
  # removing punctuation for ease of replacing WITH regex
  mutate(CITCHARGE_TYPE = str_replace_all(CITCHARGE_TYPE, "[:punct:]", "")) %>%
  # syntax for string detect is to have the entire phrase in "()" if you're trying to regex entire observation
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(DEALING IN A SCHEDULE OR CONTROLLED SUBSTANCE)"),
      "DEALING SCHEDULE/CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(DEALING MARIJUANA)"),
      "DEALING SCHEDULE/CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(DEALING in METHAMPHETAMINE)"),
      "DEALING SCHEDULE/CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(DEALING COCOAINE)"),
      "DEALING SCHEDULE/CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF CONTROLLED SUBSTANCE)"),
      "POSSESSION OF CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF COCOAINE F)"),
      "POSSESSION OF CONTROLLED SUBSTANCE F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF METHAMPHETAMINE)"),
      "POSSESSION OF CONTROLLED SUBSTANCE F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF MARIJUANAHASH F)"),
      "POSSESSION OF CONTROLLED SUBSTANCE F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF MARIJUANAHASH M)"),
      "POSSESSION OF CONTROLLED SUBSTANCE M",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(LEAVING THE SCENE OF A PD CRASH)"),
      "LEAVING THE SCENE OF A PD/PI CRASH",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(LEAVING THE SCENE OF A PI CRASH)"),
      "LEAVING THE SCENE OF A PD/PI CRASH",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(OPERATING A VEHICLE WHILE INTOXICATED F)"),
    "OWI F",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(OPERATING A VEHICLE WHILE INTOXICATED M)"),
    "OWI M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(OPERATING A VEHICLE WITH A BAC 08 to 15)"),
    "OWI M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(OPERATING A VEHICLE WITH A BAC 15 OR Higher)"),
    "OWI M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(BATTERY M)"),
    "BATTERY M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(CRIMINAL MISCHIEF M)"),
    "CRIMINAL MISCHIEF M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Cruelty to a Police Service Animal)"),
    "ANIMAL CRUELTY",
    CITCHARGE_TYPE
  )) %>% # this is an interesting observation, cruel to a cop's animal is different that animal cruelty?
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(TORTURING OR MUTILATING A VERTEBRATE ANIMAL)"),
    "ANIMAL CRUELTY",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(INTERFERING WITH A FIREFIGHTER)"),
      "INTERFERING WITH A FIREFIGHTER/REPORTING Crime",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(INTERFERING WITH REPORTING a Crime)"),
      "INTERFERING WITH A FIREFIGHTER/REPORTING Crime",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(RESISTING LAW ENFORCEMENT M)"),
      "RESISTING LAW ENFORCEMENT M",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(RESISTING LAW ENFORCEMENT F)"),
      "RESISTING LAW ENFORCEMENT F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(PUBLIC INTOXICATION MB)"),
    "PUBLIC INTOXICATION",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(POINTING A Firearm F)"),
    "POSSESSION OF FIREARM",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(POINTING A Firearm M)"),
    "POSSESSION OF FIREARM",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF A HANDGUN F)"),
    "POSSESSION OF FIREARM",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF A HANDGUN M)"),
    "POSSESSION OF FIREARM",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(THEFTRECEIVING STOLEN PROPERTY)"),
      "THEFT/RECEIVING STOLEN PROPERTY",
      CITCHARGE_TYPE
    )
  ) %>%
  # the term common nuisance means an area that is a "known" criminal area
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(VISITING A COMMON NUISANCE)"),
    "COMMON NUISANCE",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(MAINTAINING A COMMON NUISANCE)"),
    "COMMON NUISANCE",
    CITCHARGE_TYPE
  ))



# View(simplifed_force_type_and_reason_and_arrest)

extremely_simplified_force_type_and_reason_and_arrest <-
  simplifed_force_type_and_reason_and_arrest %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(BATTERY)"),
    "ASSAULT/BATTERY",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(ASSAULT)"),
    "ASSAULT/BATTERY",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(STRANGULATION)"),
    "ASSAULT/BATTERY",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(DOMESTIC BATTERY)"),
    "DOMESTIC BATTERY",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(CIMINAL)"),
    "CRIMINAL BAHAVIOR",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(CRIMINAL)"),
    "CRIMINAL BAHAVIOR",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF CONTROLLED SUBSTANCE)"),
      "POSSESSION OF CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Prostitution)"),
    "Prostitution",
    CITCHARGE_TYPE
  )) %>%
  
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(ROBBERY)"),
    "ROBBERY",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(THEFT)"),
    "THEFT",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(BURGLARY)"),
    "BURGLARY",
    CITCHARGE_TYPE
  )) %>%
  
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(VIOLATION OF PROTECTIVE ORDER)"),
      "VIOLATION OF PROTECTIVE ORDER",
      CITCHARGE_TYPE
    )
  )  %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(OWI)"),
      "OPERATING WHILE INTOXICATED",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(MENTAL WRIT)"),
    "INTIMIDATION/MENTAL WRIT",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(INTIMIDATION)"),
    "INTIMIDATION/MENTAL WRIT",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(TRESSPASS)"),
    "TRESSPASSING",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(RESIDENTIAL ENTRY)"),
    "TRESSPASSING",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(RESISTING LAW ENFORCEMENT)"),
    "RESISTING/ESCAPE",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(IMMEDIATE DETENTION)"),
    "RESISTING/ESCAPE",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(ESCAPE)"),
    "RESISTING/ESCAPE",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(INVASION OF PRIVACY)"),
      "INVASION OF PRIVACY/STALKING",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(STALKING)"),
      "INVASION OF PRIVACY/STALKING",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(TRAFFIC)"),
    "TRAFFIC VIOLATION",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(DRIVING)"),
    "TRAFFIC VIOLATION",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(JOYRIDING)"),
    "TRAFFIC VIOLATION",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(FALSE REPORTING)"),
      "FALSE TESTIMONY/FRAUD/FORGERY",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(FRAUD)"),
      "FALSE TESTIMONY/FRAUD/FORGERY",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(FORGERY)"),
      "FALSE TESTIMONY/FRAUD/FORGERY",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF PARAPHERNALIA)"),
      "POSSESSION OF PARAPHERNALIA/CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(POSSESSION OF CONTROLLED)"),
      "POSSESSION OF PARAPHERNALIA/CONTROLLED SUBSTANCE",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(STOLEN VEHICLE)"),
    "GRAND THEFT AUTO",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(CARJACKING)"),
    "GRAND THEFT AUTO",
    CITCHARGE_TYPE
  )) %>%
  
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



# extra wrangling to keep the datasets clean
# x <-
#   extremely_simplified_force_type_and_reason_and_arrest %>%
#   # mutating street to character to skip any outliers, would have to manually wrangle these
#   mutate_if(is.integer, is.character)


extremely_simplified_force_type_and_reason_and_arrest <-
  extremely_simplified_force_type_and_reason_and_arrest %>%
  # removing the na that will be produced when the street_g is comibined into full address
  mutate(STREET_G = ifelse(is.na(STREET_G), " ", STREET_G)) %>%
  unite(STREET_ADD, STREET_N, STREET_G, STREET, STREET_T,
        sep = " ") %>%
  unite(FULL_ADD, STREET_ADD,
        CITY, STATE, sep = ", ")


View(extremely_simplified_force_type_and_reason_and_arrest)

# write_csv(
#   extremely_simplified_force_type_and_reason_and_arrest,
#   "Datasets/extremely_simplified_force_type_and_reason_and_arrest_12_2019.csv"
# )
#
# write_csv(
#   extremely_simplified_force_type_and_reason_and_arrest,
#   "../CleanData/UOF/extremely_simplified_force_type_and_reason_and_arrest_12_2019.csv"
# )
# write_csv(
#   extremely_simplified_force_type_and_reason_and_arrest,
#   "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/extremely_simplified_force_type_and_reason_and_arrest_12_2019.csv"
# )
#








#### Idea for having the ages grouped together: not sure if I want to use this ###
#
# grouped_by_age_extremely_simple  <-
#   extremely_simplified_force_type_and_reason_and_arrest %>%
#   mutate(CIT_AGE = as.character(OFF_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE < 18, "< 18", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 17 &
#                              CIT_AGE < 26, "18-25", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 25 &
#                              CIT_AGE < 36, "26-35", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 35 &
#                              CIT_AGE < 46, "36-45", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 45 &
#                              CIT_AGE < 56, "46-55", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 55 &
#                              CIT_AGE < 66, "56-65", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 65 &
#                              CIT_AGE < 76, "66-75", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 75 &
#                              CIT_AGE < 86, "76-85", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 85 &
#                              CIT_AGE < 96, "86-95", CIT_AGE)) %>%
#   mutate(CIT_AGE = if_else(CIT_AGE > 95, "UNREPORTED", CIT_AGE)) %>%
#   # converting officer age and grouping
#   mutate(OFF_AGE = as.character(OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE < 18, "UNREPORTED", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 17 &
#                              OFF_AGE < 26, "18-25", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 25 &
#                              OFF_AGE < 36, "26-35", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 35 &
#                              OFF_AGE < 46, "36-45", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 45 &
#                              OFF_AGE < 56, "46-55", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 55 &
#                              OFF_AGE < 66, "56-65", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 65 &
#                              OFF_AGE < 76, "66-75", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 75 &
#                              OFF_AGE < 86, "76-85", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 85 &
#                              OFF_AGE < 96, "86-95", OFF_AGE)) %>%
#   mutate(OFF_AGE = if_else(OFF_AGE > 95, "UNREPORTED", OFF_AGE))



##### SAVING THE CLEANED DATA BEFORE ANY SIMPLIFICATIONS
#
# # Saving for local development
# write_csv(wrangledUOF.df,
#           "../CleanData/UOF/cleaned_UOF_17_12_2019.csv")
# write_csv(
#   simplifed_force_type_and_reason_and_arrest,
#   "../CleanData/UOF/cleaned_UOF_simplified_17_12_2019.csv"
# )
# write_csv(
#   extremely_simplified_force_type_and_reason_and_arrest,
#   "../CleanData/UOF/cleaned_UOF__extremely_simplified_17_12_2019.csv"
# )
# write_csv(
#   grouped_by_age_extremely_simple,
#   "../CleanData/UOF/cleaned_UOF_extremely_simplified_grouped_by_age_17_12_2019.csv"
# )
#
# # Saving for Shiny development
# write_csv(
#   wrangledUOF.df,
#   "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/cleaned_UOF_17_12_2019.csv"
# )
# write_csv(
#   simplifed_force_type_and_reason_and_arrest,
#   "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/cleaned_UOF_simplified_17_12_2019.csv"
# )
# write_csv(
#   extremely_simplified_force_type_and_reason_and_arrest,
#   "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/cleaned_UOF__extremely_simplified_17_12_2019.csv"
# )
# write_csv(
#   grouped_by_age_extremely_simple,
#   "../ShinyProjects/IMPD_Data_2014_2019/Datasets/UOF/cleaned_UOF_extremely_simplified_grouped_by_age_17_12_2019.csv"
# )
