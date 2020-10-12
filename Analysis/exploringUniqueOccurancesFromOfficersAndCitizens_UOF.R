# seeing how many unique human occurances of UOF, meaning if 5 officers used UOF on 1 individual 
# I want to see how many individual "citizens" (since I don't know if they are citizens, we'll leave that in quotes)
# since the raw data is the literal use of force types per situation (e.g. hands + car on one individual means 2 indicidents recorded)

# library(tidyverse)

# uof_csv <- read_csv("../CleanData/UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv")
uof_csv <- read_csv("../CleanData/UOF/extremely_simplified_force_type_and_reason_and_arrest.csv")


View(uof_csv)


separated_year_uof <- uof_csv %>% 
  tidyr::replace_na(list(lon = -86.15646, lat = 39.76852)) %>%
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  mutate(OCCURRED_YEAR = ifelse(
    OCCURRED_YEAR == 2000 |
      is.na(OCCURRED_YEAR),
    "Unreported",
    OCCURRED_YEAR
  ))


distinct_incnum <- separated_year_uof %>% 
  distinct(INCNUM, .keep_all = TRUE)

View(distinct_incnum)

distinct_citnum <- separated_year_uof %>% 
  distinct(CITNUM, .keep_all = TRUE)

View(distinct_citnum)

test <- separated_year_uof %>% 
  filter(OFF_NUM == 8)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        

view(test)


# viewing unique INCNUM per year
unique_2014 <- separated_year_uof %>% 
  filter(OCCURRED_YEAR == 2014) %>%
  distinct(INCNUM, .keep_all = TRUE)

unique_2015 <- separated_year_uof %>% 
  filter(OCCURRED_YEAR == 2015) %>%
  # distinct()
  distinct(INCNUM, .keep_all = TRUE)


View(unique_2015)
unique_2016 <- separated_year_uof %>% 
  filter(OCCURRED_YEAR == 2016) %>%
  distinct(INCNUM, .keep_all = TRUE)


