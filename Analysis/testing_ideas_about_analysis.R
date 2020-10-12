# testing the idea of having unique individual UOF on citizens

library(tidyverse)
library(ggrepel) # for some ggplot visual improvements


testing_uof <- read_csv("../CleanData/UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv")


View(testing_uof)

# colnames(testing_uof)

uof_ <- testing_uof %>%
  group_by(
    lon,
    lat,
    OBJECTID,
    INCNUM,
    YMD_TM,
    OCCURRED_DT,
    OCCURRED_TM,
    UDTEXT24A,
    UDTEXT24B,
    UDTEXT24C,
    UDTEXT24D,
    DISPOSITION,
    FULL_ADD,
    UOF_FORCE_TYPE,
    UOF_REASON ,
    SERVICE_TYPE,
    CIT_ARRESTED,
    CITCHARGE_TYPE,
    CIT_WEAPON_TYPE,
    CIT_INJURED,
    CIT_HOSPITAL,
    OFF_INJURED,
    OFF_HOSPITAL,
    CITNUM,
    CIT_RACE,
    CIT_SEX,
    CIT_AGE,
    CIT_COND_TYPE,
    OFF_NUM,
    OFF_RACE,
    OFF_SEX,
    OFF_AGE,
    OFF_YR_EMPLOY,
    OFF_COND_TYPE,
    OCCURRED_WEEK_DAY,
    OCCURRED_QUARTER
  ) %>%
  # group_by(INCNUM, FULL_ADD, UOF_FORCE_TYPE, UOF_REASON) %>%
  distinct(INCNUM) %>% 
  ungroup()
View(uof_)


# if else statement of if the INCNUM is the same then combine the officer's numbers and the uof type
# or swtich statement? something to make sure we can combine all of the information, that way it won't be lost on the distint INCNUM

blah <- testing_uof %>% 
  mutate(UOF_FORCE_TYPE = ifelse(INCNUM == 5098, "BILLY OAT", UOF_FORCE_TYPE))


View(blah)

unique()

# just a thought process of using which statements
# blah$UOF_FORCE_TYPE[which(
#   !distint(blah$INCNUM) == "Pennsylvania" &
#     wrangledUOF.df$STREET_N == 2900 &
#     wrangledUOF.df$STREET_G == "S"
# )] = "St"



officerOccurances <- testing_uof %>% 
  count(OFF_NUM)

View(officerOccurances)







citizenOccurances <- testing_uof %>% 
  count(CITNUM)

View(citizenOccurances)



testing  <- testing_uof %>% 
  filter(CITNUM == 11890) %>% 
  select(UOF_FORCE_TYPE, UOF_REASON, OCCURRED_DT, CIT_SEX, CIT_AGE, CIT_COND_TYPE, OFF_COND_TYPE)
View(testing)




# THIS DATA MAKES LITTLE SENSE, HIGHEST UOF ON SINGLE CITIZEN IS 900+, 
# AND SOME COPS HAVE ~500 UOF OVER THE LAST 4 YEARS
# THIS SHIT IS WILD, MIGHT NEED TO LOOK AT THIS, MIGHT LITERALLY BE A FEW BAD APPLES

count_of_cit_num <- testing_uof %>% 
  group_by(OCCURRED_YEAR, UOF_FORCE_TYPE, UOF_REASON, OFF_NUM) %>% 
    dplyr::count(CITNUM) 

count_of_off_num <- testing_uof %>% 
  group_by(OCCURRED_YEAR, UOF_FORCE_TYPE, UOF_REASON, CITNUM) %>% 
  dplyr::count(OFF_NUM) 


# View(count_of_cit_num)
# View(count_of_off_num)




View(counting_cit_off_nums)


# how to graph this? maybe look at how you did UCR data? 
testingGraphOnIncNum <- count_of_off_num %>%
  mutate(OFF_NUM == factor(OFF_NUM)) %>% 
  ggplot(aes(x = OFF_NUM, fill = OCCURRED_YEAR)) +
  geom_bar()


testingGraphOnIncNum








testingOff_Num <- testing_uof %>% 
  filter(OFF_NUM == 1814) 

View(testingOff_Num)


blah <- testing_uof %>% 
  group_by(CITNUM, OFF_NUM, UOF_REASON) %>% 
  summarise(UOF_FORCE_TYPE = paste0(UOF_FORCE_TYPE, collapse = " ")) %>%
  ungroup()
  
  
View(blah)


graphing.1814 <- testingOff_Num %>% 
  ggplot(aes(x = CIT_RACE)) +
  geom_bar()

graphing.1814









resistingArrest <- testing_uof %>%
  filter(UOF_REASON == "Resisting Arrest") %>% 
  group_by(UOF_REASON, UOF_FORCE_TYPE, INCNUM) %>% 
  # mutate(UOF_FORCE_TYPE == ) %>% 
  distinct(INCNUM)


View(resistingArrest)

TESTING_ <- testing_uof %>%
  filter(INCNUM == 5098)

View(TESTING_)




# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# RColorBrewer::display.brewer.all()

# palette_by_year <- RColorBrewer::brewer.pal(n = 8, name = "Paired")


View(grDevices::colors())

color_palette_by_years_2014_to_unreported <- c("indianred", "wheat", "cornflowerblue", "seagreen2", "chocolate2", "purple", "darkorange4")


groupingByNumberOfINC <- 
  testing_uof %>% 
  arrange(desc(OCCURRED_YEAR)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR)) %>% # was used as a placeholder
  replace_na(list(OCCURRED_YEAR = "Unreported")) %>%
  group_by(OCCURRED_YEAR, UOF_FORCE_TYPE, UOF_REASON) %>% 
  mutate(OCCURRED_YEAR == as.factor(OCCURRED_YEAR)) %>% 
  count(INCNUM) %>% 
  rename("NUM.OF.OCCURANCES.INCNUM" = n) %>%  
  # filter(NUM.OF.OCCURANCES.INCNUM > 50) %>% 
  ggplot(aes(x = INCNUM, y = NUM.OF.OCCURANCES.INCNUM, color = OCCURRED_YEAR)) +
  geom_point() + 
  labs(title = "Number of Occurances of specific Incident Numbers", 
       subtitle =  "(meaning, how many times use of force was used in one incident)",
       caption = paste0("Measuring all of the INCNUMs together,\nwe can assume the 'Unreported' years are \ngrouped together with their respective year.")) +
  scale_color_manual(values=color_palette_by_years_2014_to_unreported) +
  # changing the color fill of the OCCURRED_YEAR
  # theme_bw() +
  theme_classic() +
  theme(legend.position="bottom") +
  coord_flip()


groupingByNumberOfINC




groupingByCitizenNum <- 
  testing_uof %>% 
  arrange(desc(OCCURRED_YEAR)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR)) %>% # was used as a placeholder
  replace_na(list(OCCURRED_YEAR = "Unreported")) %>%
  group_by(OCCURRED_YEAR, UOF_FORCE_TYPE, UOF_REASON) %>% 
  mutate(OCCURRED_YEAR == as.factor(OCCURRED_YEAR)) %>% 
  count(CITNUM) %>% 
  rename("NUM.OF.OCCURANCES.ON.CITNUM" = n) %>%  
  # filter(NUM.OF.OCCURANCES.ON.CITNUM > 50) %>% 
  ggplot(aes(x = CITNUM, y = NUM.OF.OCCURANCES.ON.CITNUM, color = OCCURRED_YEAR)) +
  geom_point() + 
  labs(title = "Number of Occurances on Single Citizen Number", 
       subtitle =  "(meaning, how many times use of force was used on one 'Citizen')",
       caption = "Measuring all of the CITNUMs together, we can assume the 'Unreported' years are grouped together") +
  scale_color_manual(values=color_palette_by_years_2014_to_unreported) + 
  # changing the color fill of the OCCURRED_YEAR
  coord_flip()


groupingByCitizenNum



testing3524 <- testing_uof %>% 
  filter(OFF_NUM == 3524)

View(testing3524)

howManyOfficersInvolved <- testing_uof %>% 
  arrange(desc(OCCURRED_YEAR)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR)) %>% # was used as a placeholder
  mutate(OFF_NUM = ifelse(OFF_NUM == 9999, "Unreported", OFF_NUM)) %>% # was used as a placeholder
  group_by(OCCURRED_YEAR) %>% 
  count(OFF_NUM) %>% 
  filter(n > 500) 
  
  
  ggplot(aes(x = OFF_NUM, y = n)) +
  geom_point(aes(color = OCCURRED_YEAR)) + 
  labs(title = "Number of Occurances on Single Citizen Number", 
       subtitle =  "(meaning, how many times use of force was used on one 'Citizen')") +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=14, angle=45),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=14, angle=45))

howManyOfficersInvolved
View(howManyOfficersInvolved)


# this returns a very weird obsertaion
groupingByOfficerNumber <-
  testing_uof %>%
  arrange(desc(OCCURRED_YEAR)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR)) %>% # was used as a placeholder
  mutate(OFF_NUM = ifelse(OFF_NUM == 9999, "Unreported", OFF_NUM)) %>% # was used as a placeholder
  replace_na(list(OCCURRED_YEAR = "Unreported")) %>%
  replace_na(list(OFF_NUM = "Unreported")) %>%
  group_by(OCCURRED_YEAR, UOF_FORCE_TYPE, UOF_REASON) %>%
  # mutate(OCCURRED_YEAR == as.factor(OCCURRED_YEAR)) %>%
  count(OFF_NUM) %>%
  rename("NUM.OF.OCCURANCES.ON.OFF_NUM" = n) %>%
  filter(NUM.OF.OCCURANCES.ON.OFF_NUM > 50) %>%
  ggplot(aes(x = OFF_NUM, y = NUM.OF.OCCURANCES.ON.OFF_NUM, color = OCCURRED_YEAR)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = paste0(NUM.OF.OCCURANCES.ON.OFF_NUM, " occurances in ", OCCURRED_YEAR, " by Officer ", OFF_NUM)), label.size = 0.2) +
  coord_flip() +
  labs(title = "Number of Occurances on Single Citizen Number",
       subtitle =  "(meaning, how many times use of force was used on one 'Citizen')") +
  scale_color_manual(values = color_palette_by_years_2014_to_unreported) +
  theme(axis.text.x = element_text(angle = 45),
        axis.text.y = element_text(angle = 45))
  

groupingByOfficerNumber


whatIs1982 <- testing_uof %>% 
  filter(OFF_NUM == 1982)

View(whatIs1982)
