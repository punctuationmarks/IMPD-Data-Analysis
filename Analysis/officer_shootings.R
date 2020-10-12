# importing officer related shootings


library(tidyverse)

officerInvolvedShootings <- read_csv("../RawData/IMPD_Officer_Involved_Shootings.csv")

View(officerInvolvedShootings)


cleaningData <- dplyr::as_tibble(officerInvolvedShootings)

cleaningData <- cleaningData %>% 
  dplyr::rename(CIT_SEX = SEX) %>% 
  dplyr::rename(CIT_RACE = RACE)


# View(cleaningData)

EDA <- cleaningData %>% ggplot2::ggplot(mapping = aes(x=CIT_AGE)) + 
  geom_freqpoly(aes(color=OFF_RACE),  bins=5) 

EDA

EDA_2 <- cleaningData %>% ggplot2::ggplot(mapping = aes(x=CIT_COND_TYPE)) + 
  geom_bar(aes(fill=CIT_WEAPON)) 

EDA_2



