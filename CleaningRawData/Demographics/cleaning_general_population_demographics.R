# Exploring US Census data on Indianapolis

library(tidyverse)


US_Census_IN <- read_csv("../RawData/QuickFacts Nov-04-2019_From_US_Census.csv")
police_demographics_2013 <- read_csv("../RawData/demographicsIMPD_as_of_2013.csv")





citizen_demographics <- US_Census_IN %>% 
  select(c(Demographics = Fact, Indianapolis_Percent_Demographic = `Indianapolis city (balance), Indiana`)) %>% 
  # Just grabbing the rows with percentages, which correlate directly to the city's population
  filter(grepl("%", Indianapolis_Percent_Demographic)) %>% 
  # dropping the first observation, since it deals with estimated population change from 2010-2018
  slice(2:26) %>% 
  # Removing the percent sign
  mutate(Indianapolis_Percent_Demographic = as.numeric(gsub("\\%", "", Indianapolis_Percent_Demographic))) %>% 
  # Changing the % to numerical percentage
  mutate(Indianapolis_Percent_Demographic = (Indianapolis_Percent_Demographic / 100)) %>%
  # These are stated in the first 4 rows of the Census data, new data will be available sometime after 2020
  # We're going to assume these percentages are unchanged for all of the data, althought the only data it is FOR SURE
  # true on is the 2010 Census
  mutate(Estimated_Population_2018 = as.integer(867125	* Indianapolis_Percent_Demographic)) %>% 
  mutate(Estimated_Population_2010 = as.integer(820436 * Indianapolis_Percent_Demographic)) %>% 
  mutate(Census_Population_2010 = as.integer(820445 * Indianapolis_Percent_Demographic)) 




police_demographics_2013 <- police_demographics_2013 %>%
  select(c(Demographics = officer_race, Census_Population_2013 = num_officers_2013))



write_csv(citizen_demographics, "cleaned_Indianapolis_citizen_population_demographics_2010_2018.csv")
write_csv(citizen_demographics, "../CleanData/Demographics/cleaned_Indianapolis_citizen_population_demographics_2010_2018.csv")
write_csv(citizen_demographics, "../ShinyProjects/TestingDeploy/Datasets/Demographics/cleaned_Indianapolis_citizen_population_demographics_2010_2018.csv")