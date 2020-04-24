library(tidyverse)

## some base processing of the dates which might be fairly universal for analysis

UOF.df <-
  read_csv("../CleanData/UOF/cleanedUOF_withGeoLocation.csv")




# Making the date more understandable, allowing the graphing of year, quarter (for a rough way of seeing the seasons for the citizens (not far off
# since the midwest's seasons are almost quarterly'), but also important for the
# business cycle of the police force as well as the individual officer's lives), day of the week (Mon, Fri, ect)
separatingDates.df <- UOF.df %>%
  unite(YMD_TM,
        OCCURRED_DT,
        OCCURRED_TM,
        sep = " ",
        remove = FALSE) %>% # need to combine these to parse them
  mutate(YMD_TM = as.POSIXct(YMD_TM, format = "%Y-%m-%d %H:%M:%S")) %>%  # need to change the date and time to POSIXct
  mutate(YMD_TM = lubridate::ymd_hms(YMD_TM)) %>% # formatting the time again to ymd_hms for ease of lubridate's viewing
  dplyr::mutate(OCCURRED_WEEK_DAY = lubridate::wday(OCCURRED_DT, label = TRUE)) %>%  # day of the week might be interesting
  mutate(OCCURRED_QUARTER = lubridate::quarter(YMD_TM)) %>% # quarter might show some good trends
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM))



cleaning_part_1 <- separatingDates.df %>%
  rename(CIT_SEX = SEX) %>%
  rename(CIT_RACE = RACE) %>%
  rename(OFF_NUM = OFFNUM) %>%
  mutate(CIT_SEX = ifelse((is.na(CIT_SEX) |
                             CIT_SEX == "Unknown"), "Unreported", CIT_SEX)) %>%
  mutate(CIT_SEX = if_else(CIT_SEX == "M", "Male", CIT_SEX)) %>%
  mutate(CIT_RACE = ifelse((is.na(CIT_RACE) |
                              CIT_RACE == "Unknown" |
                              CIT_RACE == "NA"),
                           "Unreported",
                           CIT_RACE
  )) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "B", "Black", CIT_RACE)) %>%
  mutate(CIT_RACE = if_else(CIT_RACE == "Native Amer", "Native American", CIT_RACE))



colnames(cleaning_part_1)


cleaning_part_2 <- cleaning_part_1 %>%
  replace_na(list(CIT_AGE = "Unreported")) %>%
  replace_na(list(CIT_SEX = "Unreported")) %>%
  replace_na(list(OFF_SEX = "Unreported")) %>%
  replace_na(list(OFF_AGE = "Unreported"))  %>%
  arrange(
    INCNUM,
    DISPOSITION,
    OFF_NUM,
    CITNUM,
    OFF_RACE,
    CIT_RACE,
    OFF_SEX,
    CIT_SEX,
    OFF_AGE,
    CIT_AGE,
    OFF_YR_EMPLOY,
    OFF_COND_TYPE,
    CIT_COND_TYPE,
    CITCHARGE_TYPE,
    CIT_ARRESTED,
    UOF_FORCE_TYPE,
    UOF_REASON,
    SERVICE_TYPE,
    CIT_WEAPON_TYPE,
    UDTEXT24A,
    UDTEXT24B,
    UDTEXT24C,
    UDTEXT24D,
    OBJECTID,
    OCCURRED_DT,
    everything()
  )

# View(cleaning_part_2)


write_csv(
  cleaning_part_2,
  "../CleanData/UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv"
)
