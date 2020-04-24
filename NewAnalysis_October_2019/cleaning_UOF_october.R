library(tidyverse)

# quick cleaning
# cleaning new use of force, but without having lat/lon or full address info due to costs (if needs to be done, that's doable)
uof_october_2019 <- read_csv("IMPD_Use_Of_Force_2019_10_09.csv")


# View(uof_october_2019)
colnames(uof_october_2019)




cleaned_oct_2019_uof <- uof_october_2019 %>% 
  # renaming variables
  dplyr::rename(CIT_SEX = SEX) %>% 
  rename(CIT_RACE = RACE) %>% 
  rename(OFF_NUM = OFFNUM) %>% 
  # cleaning up some manually entered data
  dplyr::mutate(CIT_SEX = ifelse((is.na(CIT_SEX) | CIT_SEX == "Unknown" ), "Unreported", CIT_SEX)) %>% 
  mutate(CIT_SEX = if_else(CIT_SEX == "M", "Male", CIT_SEX)) %>%
  mutate(CIT_RACE = ifelse((is.na(CIT_RACE) | CIT_RACE == "Unknown" | CIT_RACE == "NA"), "Unreported", CIT_RACE)) %>% 
  mutate(CIT_RACE = if_else(CIT_RACE == "B", "Black", CIT_RACE)) %>% 
  mutate(CIT_RACE = if_else(CIT_RACE == "Native Amer", "Native American", CIT_RACE)) %>% 
  mutate(OFF_AGE = if_else(OFF_AGE < 18, 0, OFF_AGE)) %>% # having a placeholder for the innacurate data for the officer age 
  
  # dealing with basics of addresses, not going too detailed due to time constraints
  mutate(CITY = stringr::str_to_lower(CITY)) %>% # making the city the same case to ease regex 
  mutate(CITY = stringr::str_squish(CITY)) %>% # removing any excess white space
  mutate(CITY = if_else(stringr::str_detect(CITY, "(ind|iin|ind|ini|idi|inj)"), "indianapolis", CITY)) %>%  # regex matching all of the mispelling of indianapolis

  # dealing with time:
  unite(YMD_TM, OCCURRED_DT, OCCURRED_TM, sep=" ", remove = FALSE) %>% # need to combine these to parse them
  mutate(YMD_TM = as.POSIXct(YMD_TM, format = "%Y-%m-%d %H:%M:%S")) %>%  # need to change the date and time to POSIXct
  mutate(YMD_TM = lubridate::ymd_hms(YMD_TM)) %>% # formatting the time again to ymd_hms for ease of lubridate's viewing
  mutate(OCCURRED_WEEK_DAY = lubridate::wday(OCCURRED_DT, label= TRUE)) %>%  # day of the week might be interesting
  mutate(OCCURRED_HOUR = lubridate::hour(YMD_TM)) %>% 
  mutate(OCCURRED_QUARTER = lubridate::quarter(YMD_TM)) %>% # quarter might show some good trends
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>% 
  # dealing with missing values
  replace_na(
    list(
      OCCURRED_YEAR = "Unreported",
      UDTEXT24A = "Unreported",
      UDTEXT24B = "Unreported",
      UDTEXT24C = "Unreported",
      UDTEXT24D = "Unreported",
      DISPOSITION = "Unreported",
      UOF_FORCE_TYPE = "Unreported",
      UOF_REASON = "Unreported",
      SERVICE_TYPE = "Unreported",
      CIT_ARRESTED = "Unreported",
      CITCHARGE_TYPE = "Unreported",
      CIT_WEAPON_TYPE = "Unreported",
      CIT_INJURED = "Unreported",
      CIT_HOSPITAL = "Unreported",
      CIT_COND_TYPE = "Unreported",
      OFF_INJURED = "Unreported",
      OFF_HOSPITAL = "Unreported",
      CITNUM = "Unreported",
      CIT_RACE = "Unreported",
      CIT_SEX = "Unreported",
      OFF_AGE = "Unreported",
      OFF_SEX = "Unreported",
      OFF_RACE  = "Unreported",
      OFF_YR_EMPLOY = "Unreported",
      OFF_COND_TYPE = "Unreported"
    )
  ) %>% 
  # Cleaning up some grammatical issues
  mutate(CIT_COND_TYPE = stringr::str_to_sentence(CIT_COND_TYPE)) %>% 
  mutate(OFF_COND_TYPE = str_to_sentence(OFF_COND_TYPE)) %>% 
  mutate(UDTEXT24A = stringr::str_to_upper(UDTEXT24A)) %>% 
  mutate(UDTEXT24B = str_to_upper(UDTEXT24B)) %>% 
  mutate(UDTEXT24C = str_to_upper(UDTEXT24C)) %>% 
  mutate(UDTEXT24D = str_to_upper(UDTEXT24D)) %>% 
  mutate(STREET = str_squish(STREET)) %>% # removing any excess white space
  mutate(STREET = str_to_title(STREET)) %>%
  mutate(STREET_T = str_to_title(STREET_T)) %>%
  mutate(CITY = str_to_title(CITY))

View(cleaned_oct_2019_uof)

    



write_csv(cleaned_oct_2019_uof, "cleaned_oct_2019_uof.csv")
w  
  