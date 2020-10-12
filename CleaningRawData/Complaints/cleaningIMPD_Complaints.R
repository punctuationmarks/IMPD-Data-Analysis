# dataset found here:
# http://data.indy.gov


library(tidyverse)
library(stringr)
# library(assertthat)


complaints <-
  readr::read_csv("../RawData/IMPD_complaints_Dec_2019.csv")




# cleaning the data

# make a copy of the dataframe, this way you can always go back to the original, quickly
wrangledComplaints.df <- complaints


wrangledComplaints.df <- wrangledComplaints.df %>%
  # renaming the variable/column names to be more explicit
  dplyr::rename(CIT_SEX = SEX,
                CIT_RACE = RACE,
                OFF_NUM = OFFNUM) %>%
  # mutating all of the strings for ease of manipulation
  mutate_if(is.character, str_to_upper) %>%
  mutate_if(is.character, str_squish) %>%
  ### Cleaning SEX data ###
  mutate(
    CIT_SEX = if_else(CIT_SEX == "M", "MALE", CIT_SEX),
    CIT_SEX = if_else(CIT_SEX == "F", "FEMALE", CIT_SEX),
    # using | "or" logic to match rename SEX observations
    CIT_SEX = if_else((is.na(CIT_SEX) |
                         CIT_SEX == " " | CIT_SEX == "UNKNOWN"),
                      "UNREPORTED",
                      CIT_SEX
    ),
  ) %>%
  # removing missing officer sex data
  mutate(OFF_SEX = if_else((is.na(OFF_SEX) |
                              OFF_SEX == " "), "UNREPORTED", OFF_SEX)) %>%
  ### Cleaning Race ###
  mutate(
    CIT_RACE = if_else((CIT_RACE == "B" |
                          CIT_RACE == "Blace"), "Black", CIT_RACE),
    CIT_RACE = if_else(CIT_RACE == "W", "White", CIT_RACE),
    CIT_RACE = if_else((is.na(CIT_RACE) |
                          CIT_RACE == "UNKNOWN" | CIT_RACE == " "),
                       "UNREPORTED",
                       CIT_RACE
    )
  ) %>%
  # cleaning up missing officer race values
  mutate(OFF_RACE = if_else((is.na(OFF_RACE) |
                               OFF_RACE == " "), "UNREPORTED", OFF_RACE)) %>%
  ### Cleaing up findings and allegation of investigation of complaint ###
  mutate(FINDING = ifelse((is.na(FINDING) |
                             FINDING == " "), "UNREPORTED", FINDING)) %>%
  # Cleaing up findings of investigation of complaint
  mutate(ALLEGATION = ifelse((is.na(ALLEGATION) |
                                ALLEGATION == " "),
                             "UNREPORTED",
                             ALLEGATION)) %>%
  ### Cleaning Age ###
  # # we'll just say this is 99999 to ensure keeping the observation and have the number be a ridiculous number
  # (and for consistency, since this data cluster has a small sample size)
  mutate(CIT_AGE = ifelse((is.na(CIT_AGE) | CIT_AGE < 0),
                          99999,
                          CIT_AGE)) %>%
  # # Cops can't be minors, right?
  # # we'll just say this is 99999 to ensure keeping the observation and have the number be a ridiculous number
  # # (if the sample size was much larger that 3, then we might want to differeniate these)
  mutate(OFF_AGE = ifelse((is.na(OFF_AGE) | OFF_AGE < 18),
                          99999,
                          OFF_AGE)) %>%
  ### Dealing with time ###
  # cleaning up the time, assuming that the midnight hour is correct (since the complaint could have happened after the incident,
  # thus being an estimated time), we'll change the empty values to "01:02:03:004"
  # since there are no observations with that time stamp
  mutate(OCCURRED_TM = ifelse((is.na(OCCURRED_TM) |
                                 OCCURRED_TM == " "),
                              "01:02:03:004",
                              OCCURRED_TM)) %>%
  # splitting the Time and Date up to be used by lubridate since there are more options with the lubridate package than
  # vanilla R, we'll keep the original data if we ever need anything with that as well
  tidyr::separate(
    OCCURRED_TM,
    into = c(
      "OCCURRED_HOUR",
      "OCCURRED_MIN",
      "OCCURRED_SEC",
      "OCCURRED_MILLISEC"
    ),
    sep = ":",
    remove = FALSE
  ) %>%
  tidyr::separate(
    OCCURRED_DT,
    into = c("OCCURRED_YEAR", "OCCURRED_MONTH", "OCCURRED_DAY"),
    sep = "-",
    remove = FALSE
  )  %>%
  # converting all of the date data to numeric (since lubridate processes only numeric/integers)
  mutate(
    OCCURRED_HOUR = as.numeric(OCCURRED_HOUR),
    OCCURRED_MIN = as.numeric(OCCURRED_MIN),
    OCCURRED_SEC = as.numeric(OCCURRED_SEC),
    OCCURRED_MILLISEC = as.numeric(OCCURRED_MILLISEC)
  ) %>%
  mutate(
    OCCURRED_YEAR = as.numeric(OCCURRED_YEAR),
    OCCURRED_MONTH = as.numeric(OCCURRED_MONTH),
    OCCURRED_DAY = as.numeric(OCCURRED_DAY)
  ) %>%
  # making the new variable for the lubridate date time
  mutate(
    OCCURRED_DATE_AND_TIME = lubridate::make_datetime(
      year = OCCURRED_YEAR,
      month = OCCURRED_MONTH,
      day = OCCURRED_DAY,
      hour = OCCURRED_HOUR,
      min = OCCURRED_MIN,
      sec = OCCURRED_SEC,
      tz = "America/New_York"
    )
  ) %>%
  # Removing missing service types
  mutate(SERVICE_TYPE = if_else((is.na(SERVICE_TYPE) |
                                   SERVICE_TYPE == " " | 
                                   SERVICE_TYPE == ""),
                                "UNREPORTED",
                                SERVICE_TYPE)) %>%
  # assuming all of the empty cities are INDIANAPOLIS
  mutate(CITY = if_else((is.na(CITY) |
                           CITY == " " |
                           CITY == ""), "INDIANAPOLIS", CITY)) %>%
  # Removing missing values for accused officer's time of employment, notice use of ifelse since we're going from integer to string
  mutate(OFF_YR_EMPLOY = ifelse(is.na(OFF_YR_EMPLOY), 99999, OFF_YR_EMPLOY)) %>%
  # cleaning up alegation class
  mutate(ALG_CLASS = if_else((is.na(ALG_CLASS) |
                                ALG_CLASS == " " | ALG_CLASS == ""),
                             "UNREPORTED",
                             ALG_CLASS)) %>%
  # mutating all of the rest of the NA values
  mutate(UDTEXT24A = if_else((is.na(UDTEXT24A) |
                                UDTEXT24A == " " |
                                UDTEXT24A == ""),
                             "UNREPORTED",
                             UDTEXT24A
  )) %>%
  
  mutate(UDTEXT24B = if_else((is.na(UDTEXT24B) |
                                UDTEXT24B == " " |
                                UDTEXT24B == ""),
                             "UNREPORTED",
                             UDTEXT24B
  )) %>%
  
  mutate(UDTEXT24C = if_else((is.na(UDTEXT24C) |
                                UDTEXT24C == " " |
                                UDTEXT24C == ""),
                             "UNREPORTED",
                             UDTEXT24C
  )) %>%
  
  mutate(UDTEXT24D = if_else((is.na(UDTEXT24D) |
                                UDTEXT24D == " " |
                                UDTEXT24D == ""),
                             "UNREPORTED",
                             UDTEXT24D
  )) 


wrangledComplaints.df <- wrangledComplaints.df %>%
  # dropping addresses and some redundant variables that could easily be reconstructed
  select(-c(STREET, STREET_T, STREET_G, STREET_N, OCCURRED_MIN, OCCURRED_SEC, OCCURRED_MILLISEC))

View(wrangledComplaints.df)

# if wanting in the same dir/
readr::write_csv(wrangledComplaints.df, "cleanedComplaint_data.csv")
readr::write_csv(wrangledComplaints.df,
                 "../CleanData/cleanedComplaint_data.csv")
readr::write_csv(wrangledComplaints.df,
                 "~/Projects/IMPD/ShinyProjects/IMPD_Data_2014_2019/Datasets/Complaints/cleanedComplaint_data.csv")


checkingWritingAndReading.complaintCSV <-
  readr::read_csv("cleanedComplaint_data.csv")
View(checkingWritingAndReading.complaintCSV)




# View(wrangledComplaints.df)












# This seems unnecessary since I don't envision mapping these oberservations plus it might get expensive
# paying for google maps
## Addresses of Complaints ##




### Exploring the nitty gritty details of address

# this is just a small script to test specific street names so check what city, street type, ect
# testingSpecificStreets <-
# wrangledComplaints.df %>%  filter(grepl('US 40 & Center St', STREET))
# glimpse(testingSpecificStreets)




# inspecting some of the intersections written down by officers,
# since these are individual incidents, we'll need to clean them individually and not in any batch form
# interSectionStreets <- wrangledComplaints.df %>%
#   arrange(STREET, STREET_N, STREET_T, STREET_G) %>%
#   # grepl is a very handy regex finder, to filter out what we want to use, but it is loose, meaning Anderson will also appear
#   filter(grepl('/|and|And|&|@', STREET))

# glimpse(interSectionStreets)


# # NOTE: Because we are searching these by the street name AND changing the street name
# # we need to change the street name as the last change, since if we don't,
# # the type, geo, or number will most likely not link up

### Skeleton of the variables for renaming address observations:
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "")] = ""
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "")] = ""
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "")] = ""
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "34th Keystone and Millersville")] = "3400"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "34th Keystone and Millersville")] = "N"
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "34th Keystone and Millersville")] = "Ave"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "34th Keystone and Millersville")] = "Keystone"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Binford/56th")] = "4101"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Binford/56th")] = "E"
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Binford/56th")] = "St"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Binford/56th")] = "56th"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Conner @ Cumberland")] = "2599"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Conner @ Cumberland")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Conner @ Cumberland")] = ""
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Conner @ Cumberland")] = "IN-32"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Fallcreek/Binford")] = "4410"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Fallcreek/Binford")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Fallcreek/Binford")] = "Blvd"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Fallcreek/Binford")] = "Binford"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "US 40 & Center St")] = "195"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "US 40 & Center St")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "US 40 & Center St")] = ""
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "US 40 & Center St")] = "US-40"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Washington & Mitthoeffer")] = "9999"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Washington & Mitthoeffer")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Washington & Mitthoeffer")] = ""
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Washington & Mitthoeffer")] = "US-40"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Emerson Ave & Thompson Rd")] = "4957"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Emerson Ave & Thompson Rd")] = "s"
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Emerson Ave & Thompson Rd")] = "Ave"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Emerson Ave & Thompson Rd")] = "Emerson"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Fall Creek & Hague")] = "7700"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Fall Creek & Hague")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Fall Creek & Hague")] = "Rd"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Fall Creek & Hague")] = "Fall Creek"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "East & Hanna")] = "4000"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "East & Hanna")] = "S"
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "East & Hanna")] = "St"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "East & Hanna")] = "East"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Bridge @ Meridian Broadripple")] = "6622"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Bridge @ Meridian Broadripple")] = "N"
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Bridge @ Meridian Broadripple")] = "St"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Bridge @ Meridian Broadripple")] = "Meridian"
#
# wrangledComplaints.df$STREET_N[which(wrangledComplaints.df$STREET == "Raymond @ Emerson")] = "2143"
# wrangledComplaints.df$STREET_G[which(wrangledComplaints.df$STREET == "Raymond @ Emerson")] = ""
# wrangledComplaints.df$STREET_T[which(wrangledComplaints.df$STREET == "Raymond @ Emerson")] = "Ave"
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "Raymond @ Emerson")] = "Emerson"
#
#
#
#


## !! IMPORTANT !! ##
# Any addresses that are not reported will be defaulted to
# the downtown district building of IMPD (39 Jackson Place)
# AND all highways will be the IMPD (100 N Senate Ave), for brevity and accuracy

# Everything on highways will default to INDOT (since current google maps aren't equipt to accept mile markers and highways
# and a decent amount of this data is less than GPS accurate)
# we'll set a placeholder for the highway names so we can just batch rename the full street address later
# wrangledComplaints.df$STREET[grep(
#   'Interstate|interstate|I65|I70|I465|i65|i70|i465|I-65|I-70|I-465|i-65|i-70|i-465|465',
#   wrangledComplaints.df$STREET
# )] = 'INDOT'
#
#
# # Some outliers on highways
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "70" &
#                                      wrangledComplaints.df$STREET_T == "Highway")] = 'INDOT'
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "65" &
#                                      wrangledComplaints.df$STREET_T == "Highway")] = 'INDOT'
# wrangledComplaints.df$STREET[which(
#   wrangledComplaints.df$STREET == "65 MM108" &
#     wrangledComplaints.df$STREET_T == "Highway"
# )] = 'INDOT'
#
# wrangledComplaints.df$STREET[which(wrangledComplaints.df$STREET == "36" &
#                                      wrangledComplaints.df$STREET_T == "Highway")] = 'INDOT'
#
#
#
#
# # Formatting all phone call and online complaints to be together (since their sample is so small, 7 entrees)
# wrangledComplaints.df$STREET[grep('online|phone|Online|Phone', wrangledComplaints.df$STREET)] = 'Online/Phone'
#
#

# impd for any missing data, using a placeholder to not miss any values (or accidentally overwrite something)
# doing this at the end, since we already cleaned the rest of the data, so relying on the STREET to be empty
# only for those that haven't been ruled out as a highway, some intersection, or an online/phone complant
#
#
# wrangledComplaints.df <-
#   wrangledComplaints.df %>% mutate(STREET = if_else((is.na(STREET) |
#                                                        STREET == " "), "IMPD", STREET))
#
#
# wrangledComplaints.df <- wrangledComplaints.df %>%
#   # Changing the INDOT placeholder to be the actual address to INDOT
#   mutate(
#     STREET_N = ifelse(STREET == "INDOT", "100", STREET_N),
#     STREET_G = ifelse(STREET == "INDOT", "N", STREET_G),
#     STREET_T = ifelse(STREET == "INDOT", "Ave", STREET_T),
#     CITY = ifelse(STREET == "INDOT", "INDIANAPOLIS", CITY),
#     STREET = ifelse(STREET == "INDOT", "Senate", STREET)
#   ) %>%
#   # changing the IMPD placeholder to be the actual downtown IMPD location
#   mutate(
#     STREET_N = ifelse(STREET == "IMPD", "39", STREET_N),
#     STREET_G = ifelse(STREET == "IMPD", "W", STREET_G),
#     STREET_T = ifelse(STREET == "IMPD", "Place", STREET_T),
#     CITY = ifelse(STREET == "IMPD", "INDIANAPOLIS", CITY),
#     STREET = ifelse(STREET == "IMPD", "Jackson", STREET)
#   ) %>%
#   # making all of the Online/Phone address variables the same, so it's easier to grasp on glimpse or graphing
# mutate(
#   STREET_N = ifelse(STREET == "Online/Phone", "Online/Phone", STREET_N),
#   STREET_G = ifelse(STREET == "Online/Phone", "Online/Phone", STREET_G),
#   STREET_T = ifelse(STREET == "Online/Phone", "Online/Phone", STREET_T),
#   CITY = ifelse(STREET == "Online/Phone", "Online/Phone", CITY),
#   STREET = ifelse(STREET == "Online/Phone", "Online/Phone", STREET)
# )


# View(wrangledComplaints.df)




# colnames(wrangledComplaints.df)

# # organizing some of the data
# need to orgnaize how the data is displayed, mainly due to the date and time issues,
# possibly keep everything in line and then you'll just need to put the lubridate:: date
# and then all the pieces (organized) in the back of the rest of the variables (since they're extra data)









# # fun stuff to do later?
# # just grabbing the text from the allegations
# # to be used for a word cloud
#
# allegationsText <- wrangledComplaints.df %>%
#   select(ALLEGATION)


# class(allegationsText)


# View(allegationsText)
# write_lines(allegationsText, "complaintAllegations_justText.txt", sep =
#               ",")
# write_delim(allegationsText, "complaintAllegations_justText.txt", delim =",")


# checkingWritingAndReading.complaintCSV <- read.csv2("complaintAllegations_justText.txt")
# View(checkingWritingAndReading.complaintCSV)
