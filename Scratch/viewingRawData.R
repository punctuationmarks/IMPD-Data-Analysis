library(tidyverse)



raw_IMPD_complaints <- readr::read_csv("IMPD_COMPLAINTS.csv")

# 
# View(raw_IMPD_complaints)


raw_officer_involved_shootings <- readr::read_csv("IMPD_Officer_Involved_Shootings.csv")

# View(raw_officer_involved_shootings)


raw_IMPD_UCR <- readr::read_csv("IMPD_UCR_2018_Data.csv")

# View(raw_IMPD_UCR)


raw_IMPD_UOF <- readr::read_csv("IMPD_Use_Of_Force.csv")

View(raw_IMPD_UOF)


raw_up_to_september_2019 <- readr::read_csv("uof_up_to_date_sept_2019.csv")
View(raw_up_to_september_2019)