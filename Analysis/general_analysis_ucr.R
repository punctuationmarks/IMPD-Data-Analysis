install.packages(c("tidyverse", "ggrepel"))
library(tidyverse)
library(ggrepel)


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
final_join <- full_join(fourth_join, ucr_2019)

all_ucr_data_from_2014_2019 <- final_join %>% 
  select(-ID) %>%
  replace_na(list(DATE_ = NA, TIME = NA)) %>% 
  mutate(DATE_ = lubridate::ymd(DATE_)) %>% 
  # mutate(TIME = lubridate::hms(TIME)) %>% 
  mutate(YEAR = lubridate::year(DATE_)) %>% 
  mutate(QUARTER = lubridate::quarter(DATE_)) %>% 
  mutate(WEEK_DAY = lubridate::wday(DATE_))

View(all_ucr_data_from_2014_2019)
# 
  # # 17111
# View(ucr_2019)
# # 47,464
# View(ucr_2018)
# # 50152
# View(ucr_2017)
# # 53957
# View(ucr_2016)
# # 53017
# View(ucr_2015)
# # 52713
# View(ucr_2014)





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

# View(NARROW_CRIME_2019)
# write_csv(NARROW_CRIME_2019, "../CleanData/UCR/all_ucr_simplified_crime_categories_2019.csv")




NARROW_CRIME_ALL_UCR <- all_ucr_data_from_2014_2019 %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(LARCENY)"), "LARCENY", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(AGGRAVATED ASSAULT)"), "AGGRAVATED ASSAULT", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(ROBBERY)"), "ROBBERY", CRIME)) %>%  
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(BURG)"), "BURGLARY", CRIME)) %>% 
  # group_by(CRIME, DATE_) %>% 
  count(CRIME, sort=TRUE) %>%
  rename("NUM_OCCURRANCES_2014_2019" = n)

# View(NARROW_CRIME_ALL_UCR)
# write_csv(NARROW_CRIME_ALL_UCR, "../CleanData/UCR/all_ucr_simplified_crime_categories_2014_2019_without_date.csv")



NARROW_CRIME_ALL_UCR_ALL_YEARS <- all_ucr_data_from_2014_2019 %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(LARCENY)"), "LARCENY", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(VEHICLE)"), "AUTO THEFT", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(RAPE)"), "RAPE", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(AGGRAVATED ASSAULT)"), "ASSAULT", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(ROBBER)"), "ROBBERY", CRIME)) %>%  
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(BURG)"), "BURGLARY", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(HOMOCIDE)"), "HOMOCIDE", CRIME)) %>% 
  mutate(CRIME = if_else(stringr::str_detect(CRIME, "(MANSLAUGHTER)"), "MANSLAUGHTER", CRIME))  

# View(NARROW_CRIME_ALL_UCR_ALL_YEARS)
# write_csv(NARROW_CRIME_ALL_UCR_ALL_YEARS, "../CleanData/UCR/simplied_crime_categories_2014_2019.csv")
write_csv(NARROW_CRIME_ALL_UCR_ALL_YEARS, "../ShinyProjects/TestingDeploy/UCR/simplied_crime_categories_2014_2019.csv")








### some analysis ###

plot_crime_ucr_all_years <- NARROW_CRIME_ALL_UCR_ALL_YEARS %>%
  select(YEAR, CRIME) %>%
  group_by(YEAR, CRIME) %>% 
  filter(YEAR >= 2014) %>% 
  count(CRIME) %>% 
  rename("NUM_OF_OCCURANCES" = n) %>% 
  ggplot(aes(x = CRIME, y = NUM_OF_OCCURANCES)) + 
  geom_point() +
  facet_wrap(. ~ YEAR) +
  coord_flip() +
  ggrepel::geom_label_repel(aes(label = NUM_OF_OCCURANCES), label.size = 0.15)+
  labs(title="All IMPD UCR reported for 2014-2019", 
       subtitle = paste0("Simplified crime categories,\nfor instance attempted and committed crime is combined.
                         \nAlso 2019 data was taken up until July")) +  
  theme(axis.text.x = element_text(vjust = 0.75, angle=45, lineheight = 1)) # +

plot_crime_ucr_all_years


plot_crime_ucr_all_years

# ggplot2::ggsave("../MediaOfFindings/UCR/plot_crime_ucr_2014_2019.png",  plot_crime_ucr_all_years)





plot_crime_ucr_2019 <- NARROW_CRIME_ALL_UCR %>% 
  ggplot() + 
  geom_point(aes(x = CRIME, y = NUM_OCCURRANCES)) +
  labs(title="All UCR reported for 2014-2019 ") +  
  theme(axis.text.x = element_text(vjust = 0.5, angle=45)) # +
#   scale_x_discrete(name ="Types of crimes reported") +
#   scale_y_discrete(name="Number of crimes reported in 2019")

# plot_crime_ucr_2019



#### CRIMES PER DATE OF THE GIVEN UCR YEAR, INTERESTING DATA
## for instance, there 

### SKELETON FOR TOTAL NUMBER OF CRIMES PER DAY IN DATA (INCLUDING PREVIOUS YEARS REPORTED IN SELECTED YEAR'S DATA)
crimes_per_date_2019 <- ucr_2019 %>% 
  count(DATE_, sort =TRUE) %>% 
  rename("CRIMES.2019" = n)
# crimes_per_date_2019

### SKELETON FOR TOTAL NUMBER OF CRIMES PER DAY IN SELECTED YEAR'S DATA (EXCLUDING PREVIOUS YEARS REPORTED)
crimes_per_date_2018 <- ucr_2018 %>% 
  mutate(DATE_ = lubridate::ymd(DATE_))%>%
  mutate(YEAR = lubridate::year(DATE_)) %>% 
  filter(YEAR == 2018) %>% 
  count(DATE_, sort =TRUE) %>% 
  rename("CRIMES.2018" = n) 

# View(crimes_per_date_2018)

  