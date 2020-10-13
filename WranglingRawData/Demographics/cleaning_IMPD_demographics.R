# data for demographics taken from Governing.com
# https://www.governing.com/gov-data/safety-justice/police-department-officer-demographics-minority-representation.html

library(tidyverse)

demographicsIMPD <- tribble(
  ~officer_race, ~num_officers_2013,
  "white",1326,
  "hispanic",28,
  "bi-racial",0,
  "black",218,
  "asian",0,
  "native american",0,
  "hawaiian",0,
  "unknown",15
)


demographicsIMPD <- demographicsIMPD %>% 
  mutate(officer_race = stringr::str_to_sentence(officer_race))

# View(demographicsIMPD)
write_csv(demographicsIMPD, "cleaned_demographics_IMPD_as_of_2013.csv")
write_csv(demographicsIMPD, "../CleanData/Demographics/cleaned_demographics_IMPD_as_of_2013.csv")
