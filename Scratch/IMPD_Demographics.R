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


View(demographicsIMPD)
write_csv(demographicsIMPD, "demographicsIMPD_as_of_2013.csv")
