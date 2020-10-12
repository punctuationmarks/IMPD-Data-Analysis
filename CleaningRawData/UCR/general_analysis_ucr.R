library(tidyverse)
library(stringr)

# More detailed cleaning and exploration is done in jupyternotebook with Python

### THINGS TO RESEARCH ###
## 1. ALL THE CRIME YEARS HAVE CRIMES FROM PREVIOUS YEARS (SOMETIMES THESE ARE A DECADE OLD),
##### IS THIS FROM SOLVED CRIME OR IS THIS NEWLY REPORTED CRIME.
## 2. THE PREVIOUS QUESTION LEADS ME TO ASK, IS THE GENERAL DATA SOLVED CRIME OR JUST REPORTED CRIME?
## 1A. PRELIMINARY RESEARCH DID NOT RETURN RESULTS, PERHAPS A PHONE CALL TO IMPD IS NEEDED


ucr_2019 <- read_csv("../RawData/IMPD_UCR_Current_Year.csv")
ucr_2018 <- read_csv("../RawData/IMPD_UCR_2018_Data.csv")
ucr_2017 <- read_csv("../RawData/IMPD_UCR_2017_Data.csv")
ucr_2016 <- read_csv("../RawData/IMPD_UCR_2016_Data.csv")
ucr_2015 <- read_csv("../RawData/IMPD_UCR_2015_Data.csv")
ucr_2014 <- read_csv("../RawData/IMPD_UCR_2014_Data.csv")


# all of the joined UCR data from 2014 - 2019
first_join <- full_join(ucr_2014, ucr_2015)
second_join <- full_join(first_join, ucr_2016)
third_join <- full_join(second_join, ucr_2017)
fourth_join <- full_join(third_join, ucr_2018)
all_ucr_data_from_2014_2019 <- full_join(fourth_join, ucr_2019)



# 17111
View(ucr_2019)
# 47,464
View(ucr_2018)
# 50152
View(ucr_2017)
# 53957
View(ucr_2016)
# 53017
View(ucr_2015)
# 52713
View(ucr_2014)





counts_of_crime_2019 <- ucr_2019 %>% 
  count(CRIME, sort=TRUE) %>% 
  rename("NUM_OCCURRANCES_IN_2019" = n) 


counts_of_crime_2018 <- ucr_2018 %>% 
  count(CRIME, sort=TRUE) %>% 
  rename("NUM_OCCURRANCES_IN_2018" = n) 


counts_of_crime_2017 <- ucr_2017 %>% 
  count(CRIME, sort=TRUE) %>% 
  rename("NUM_OCCURRANCES_IN_2017" = n) 


counts_of_crime_2016 <- ucr_2016 %>% 
  count(CRIME, sort=TRUE) %>% 
  rename("NUM_OCCURRANCES_IN_2016" = n) 


counts_of_crime_2015 <- ucr_2015 %>% 
  count(CRIME, sort=TRUE) %>% 
  rename("NUM_OCCURRANCES_IN_2015" = n) 

counts_of_crime_2014 <- ucr_2014 %>% 
  count(CRIME, sort=TRUE) %>% 
  rename("NUM_OCCURRANCES_IN_2014" = n) 






#### NARROW CRIME SKELETON, NARROWS CRIMES INTO SMALLER SECTIONS ####
NARROW_CRIME_2019 <- ucr_2019 %>% 
  mutate(CRIME = if_else(stringr::str_detect(ucr_2019$CRIME, "(LARCENY)"), "LARCENY", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(ucr_2019$CRIME, "(AGGRAVATED ASSAULT)"), "AGGRAVATED ASSAULT", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(ucr_2019$CRIME, "(ROBBERY)"), "ROBBERY", CRIME)) %>%  
  mutate(CRIME = if_else(stringr::str_detect(ucr_2019$CRIME, "(BURG)"), "BURGLARY", CRIME)) %>% 
  count(CRIME, sort=TRUE) %>%
  rename("NUM_OCCURRANCES_IN_2019" = n)

View(NARROW_CRIME_2019)

NARROW_CRIME_ALL_UCR <- all_ucr_data_from_2014_2019 %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(LARCENY)"), "LARCENY", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(AGGRAVATED ASSAULT)"), "AGGRAVATED ASSAULT", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(ROBBERY)"), "ROBBERY", CRIME)) %>%  
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(BURG)"), "BURGLARY", CRIME)) %>% 
  # group_by(CRIME, DATE_) %>% 
  count(CRIME, sort=TRUE) %>%
  rename("NUM_OCCURRANCES" = n)

View(NARROW_CRIME_ALL_UCR)










plot_crime_ucr_2019 <- NARROW_CRIME_ALL_UCR %>% 
  ggplot() + 
  geom_point(aes(x = CRIME, y = NUM_OCCURRANCES)) +
  labs(title="All UCR reported for 2014-2019 ") +  
  theme(axis.text.x = element_text(vjust = 0.5, angle=45)) # +
#   scale_x_discrete(name ="Types of crimes reported") +
#   scale_y_discrete(name="Number of crimes reported in 2019")

plot_crime_ucr_2019



#### CRIMES PER DATE OF THE GIVEN UCR YEAR, INTERESTING DATA
## for instance, there 

### SKELETON FOR TOTAL NUMBER OF CRIMES PER DAY IN DATA (INCLUDING PREVIOUS YEARS REPORTED IN SELECTED YEAR'S DATA)
crimes_per_date_2019 <- ucr_2019 %>% 
  count(DATE_, sort =TRUE) %>% 
  rename("Crimes per date" = n)
crimes_per_date_2019

### SKELETON FOR TOTAL NUMBER OF CRIMES PER DAY IN SELECTED YEAR'S DATA (EXCLUDING PREVIOUS YEARS REPORTED)
crimes_per_date_2018 <- ucr_2018 %>% 
  mutate(DATE_ = lubridate::ymd(DATE_))%>%
  mutate(YEAR = lubridate::year(DATE_)) %>% 
  filter(YEAR == 2018) %>% 
  count(DATE_, sort =TRUE) %>% 
  rename("Crimes per date" = n) 

View(crimes_per_date_2018)

  