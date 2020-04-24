library(tidyverse)
library(ggrepel)
library(leaflet)




# reading the data after read
UOF.df <-
  read_csv("../CleanData/UOF/cleaned_UOF_20191024.csv ")


# View(UOF.df)
# replacing missing lat, lon data with the downtown pd location
# -86.15646	39.76852
# also replacing place holders (which were there to keep the data type consistent)
UOF.df <- UOF.df %>%
  tidyr::replace_na(list(lon = -86.15646, lat = 39.76852)) %>%
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR)) %>% # was used as a placeholder
  replace_na(list(OCCURRED_YEAR = "Unreported")) %>%
  mutate(CIT_AGE = ifelse(CIT_AGE == 121, "Unreported", CIT_AGE)) %>% # was used as a placeholder
  replace_na(list(CIT_AGE = "Unreported"))


# View(UOF.df)




palPaired_byYear <- colorFactor(palette = 'Accent',
                                domain = testing$OCCURRED_YEAR)


# palPaired_byYear <- palette(RColorBrewer::brewer.pal(n = 6, name = "Set2"))


testing <- UOF.df %>%
  filter(OCCURRED_QUARTER == 3) %>%
  select(
    OCCURRED_QUARTER,
    OCCURRED_YEAR,
    lat,
    lon,
    UOF_FORCE_TYPE,
    UOF_REASON,
    OFF_RACE,
    CIT_RACE,
    CITCHARGE_TYPE,
    INCNUM
  )

testingGraph <- testing %>%  leaflet(options = c(
  leafletOptions(minZoom = 9, maxZoom = 18),
  leafletOptions(preferCanvas = TRUE)
)) %>%
  setView(lng = -86.15646,
          lat = 39.76852,
          zoom = 11) %>%
  addProviderTiles(
    providers$Stamen.TonerHybrid,
    options = tileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
  ) %>%
  addCircleMarkers(
    popup = paste0(testing$UOF_REASON,
                   "; ",
                   testing$CITCHARGE_TYPE),
    label = paste0(
      testing$UOF_FORCE_TYPE,
      " by ",
      stringr::str_to_lower(testing$OFF_SEX),
      " officer, in ",
      testing$OCCURRED_YEAR
    ),
    color = ~ palPaired_byYear(testing$OCCURRED_YEAR),
    opacity = 0.9
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palPaired_byYear,
    values = testing$OCCURRED_YEAR,
    opacity = 1
  )

testingGraph






# interesting, organized by how many times the same UOF was used at specific locations
locationDataWithTypeReason.df <- (
  UOF.df %>%
    group_by(
      INCNUM,
      UOF_FORCE_TYPE,
      UOF_REASON,
      DISPOSITION,
      OCCURRED_DT,
      FULL_ADD
    ) %>%
    summarise(n()) %>%
    arrange(UOF_FORCE_TYPE)
)

#
# View(locationDataWithTypeReason.df)

# glimpse(locationDataWithTypeReason.df)


# View(arrangingDataFromCleaned.df)





# glimpse(locationDataWithTypeReason.df)




#### WORKING GRAPHS ####

counting_by_Year <- UOF.df %>%
  count(OCCURRED_YEAR) %>%
  ggplot(aes(OCCURRED_YEAR, n)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = n)) +
  labs(title = "Number of occurances of UOF from 2014-2019")

# View(counting_by_Year)
# counting_by_Year


# View(UOF.df)


simplifed_force_type_and_reason_and_arrest <- UOF.df %>%
  # using regex in str_detect() to combine ufo type categories for ease of graphing and understanding
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(fogger)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(cs/oc)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-clearout oc)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-burning cs)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-cs grenade)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(flash bang)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(Pepper ball)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(pepperball)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(
    UOF_FORCE_TYPE = if_else(
      stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-bps gas)"),
      "Smoke grenade/Tear gas/Flash bang",
      UOF_FORCE_TYPE
    )
  ) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-taser)"),
    "Taser",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(handcuffing)"),
    "Handcuffing",
    UOF_FORCE_TYPE
  )) %>%  # make sure handcuffing comes first (since some are "Physical handcuffing")
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Body weight leverage)"),
    "Physical",
    UOF_FORCE_TYPE
  )) %>%  # make sure handcuffing comes first (since some are "Physical handcuffing")
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Physical)"),
    "Physical",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-leg sweep)"),
    "Physical",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Hands, fist, feet)"),
    "Physical",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Joint manipulation)"),
    "Physical",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-bean bag)"),
    "Bean Bag",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-baton)"),
    "Baton",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Less lethal-other)"),
    "Other",
    UOF_FORCE_TYPE
  )) %>%
  mutate(UOF_FORCE_TYPE = if_else(
    stringr::str_detect(UOF_FORCE_TYPE, "(Other)"),
    "Other",
    UOF_FORCE_TYPE
  )) %>%
  # grouping by age for citizen (don't need to convert type)
  mutate(CIT_AGE = if_else(CIT_AGE < 18, "< 18", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 17 &
                             CIT_AGE < 26, "18-25", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 25 &
                             CIT_AGE < 36, "26-35", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 35 &
                             CIT_AGE < 46, "36-45", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 45 &
                             CIT_AGE < 56, "46-55", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 55 &
                             CIT_AGE < 66, "56-65", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 65 &
                             CIT_AGE < 76, "66-75", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 75 &
                             CIT_AGE < 86, "76-85", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 85 &
                             CIT_AGE < 96, "86-95", CIT_AGE)) %>%
  mutate(CIT_AGE = if_else(CIT_AGE > 95, "Unreported", CIT_AGE)) %>%
  # converting officer age and grouping
  mutate(OFF_AGE = as.character(OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE < 18, "Unreported", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 17 &
                             OFF_AGE < 26, "18-25", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 25 &
                             OFF_AGE < 36, "26-35", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 35 &
                             OFF_AGE < 46, "36-45", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 45 &
                             OFF_AGE < 56, "46-55", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 55 &
                             OFF_AGE < 66, "56-65", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 65 &
                             OFF_AGE < 76, "66-75", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 75 &
                             OFF_AGE < 86, "76-85", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 85 &
                             OFF_AGE < 96, "86-95", OFF_AGE)) %>%
  mutate(OFF_AGE = if_else(OFF_AGE > 95, "Unreported", OFF_AGE)) %>%
  # GROUPING THE TYPE OF CHARGE FOR SPARKING THE UOF
  mutate(CITCHARGE_TYPE = stringr::str_squish(CITCHARGE_TYPE)) %>%
  mutate(CITCHARGE_TYPE = str_replace_all(CITCHARGE_TYPE, "[:punct:]", "")) %>% # removing punctuation for ease of replacing with regex
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Dealing in a Schedule or Controlled Substance)"),
      "Dealing Schedule/Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Dealing Marijuana)"),
      "Dealing Schedule/Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Dealing in Methamphetamine)"),
      "Dealing Schedule/Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Dealing Cocaine)"),
      "Dealing Schedule/Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of Controlled Substance)"),
      "Possession of Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of Cocaine F)"),
      "Possession of Controlled Substance F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of Methamphetamine)"),
      "Possession of Controlled Substance F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of MarijuanaHash F)"),
      "Possession of Controlled Substance F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of MarijuanaHash M)"),
      "Possession of Controlled Substance M",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Leaving the Scene of a PD Crash)"),
      "Leaving the Scene of a PD/PI Crash",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Leaving the Scene of a PI Crash)"),
      "Leaving the Scene of a PD/PI Crash",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Operating a Vehicle While Intoxicated F)"),
    "OWI F",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Operating a Vehicle While Intoxicated M)"),
    "OWI M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Operating a Vehicle with a BAC 08 to 15)"),
    "OWI M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Operating a Vehicle with a BAC 15 or Higher)"),
    "OWI M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Battery M)"),
    "Battery M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Criminal Mischief M)"),
    "Criminal Mischief M",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Cruelty to a Police Service Animal)"),
    "Animal Cruelty",
    CITCHARGE_TYPE
  )) %>% # this is an interesting observation, cruel to a cop's animal is different that animal cruelty?
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Torturing or Mutilating a Vertebrate Animal)"),
    "Animal Cruelty",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Interfering with a Firefighter)"),
      "Interfering with a Firefighter/Reporting Crime",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Interfering with Reporting a Crime)"),
      "Interfering with a Firefighter/Reporting Crime",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Resisting Law Enforcement M)"),
      "Resisting Law Enforcement M",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Resisting Law Enforcement F)"),
      "Resisting Law Enforcement F",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Public Intoxication MB)"),
    "Public Intoxication",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Pointing a Firearm F)"),
    "Possession of Firearm",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Pointing a Firearm M)"),
    "Possession of Firearm",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Possession of a Handgun F)"),
    "Possession of Firearm",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Possession of a Handgun M)"),
    "Possession of Firearm",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(TheftReceiving Stolen Property)"),
      "Theft/Receiving Stolen Property",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Visiting a Common Nuisance)"),
      "Theft/Receiving Stolen Property",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Maintaining a Common Nuisance)"),
      "Theft/Receiving Stolen Property",
      CITCHARGE_TYPE
    )
  )





# View(simplifed_force_type_and_reason_and_arrest)
write_csv(
  simplifed_force_type_and_reason_and_arrest,
  "../CleanData/UOF/simplifed_force_type_and_reason_and_arrest.csv"
)

# View(simplifed_force_type_and_reason_and_arrest)





extremely_simplified_force_type_and_reason_and_arrest <-
  simplifed_force_type_and_reason_and_arrest %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Battery)"),
    "Assault/Battery",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Assault)"),
    "Assault/Battery",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Strangulation)"),
    "Assault/Battery",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Domestic Battery)"),
    "Domestic Battery",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Ciminal)"),
    "Criminal Behavior",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Criminal)"),
    "Criminal Behavior",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of Controlled Substance)"),
      "Possession of Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Prostitution)"),
    "Prostitution",
    CITCHARGE_TYPE
  )) %>%

  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Theft)"),
    "Theft",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Robbery)"),
    "Robbery",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Violation of Protective Order)"),
      "Violation of Protective Order",
      CITCHARGE_TYPE
    )
  )  %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(OWI)"),
      "Operating While Intoxicated",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Mental Writ)"),
    "Intimidation/Mental Writ",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Intimidation)"),
    "Intimidation/Mental Writ",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Trespass)"),
    "Trespassing",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Residential Entry)"),
    "Trespassing",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Resisting Law Enforcement)"),
    "Resisting/Escape",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Immediate Detention)"),
    "Resisting/Escape",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Escape)"),
    "Resisting/Escape",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Invasion of Privacy)"),
      "Invasion of Privacy/Stalking",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Stalking)"),
      "Invasion of Privacy/Stalking",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Traffic)"),
    "Traffic Violation",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Driving)"),
    "Traffic Violation",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Joyriding)"),
    "Traffic Violation",
    CITCHARGE_TYPE
  )) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(False Reporting)"),
      "False testimony/Fraud/Forgery",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Fraud)"),
      "False testimony/Fraud/Forgery",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Forgery)"),
      "False testimony/Fraud/Forgery",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of Paraphernalia)"),
      "Possession of Paraphernalia/Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(
    CITCHARGE_TYPE = if_else(
      stringr::str_detect(CITCHARGE_TYPE, "(Possession of Controlled)"),
      "Possession of Paraphernalia/Controlled Substance",
      CITCHARGE_TYPE
    )
  ) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Robbery)"),
    "Robbery/Theft/Burglary",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Theft)"),
    "Robbery/Theft/Burglary",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Burglary)"),
    "Robbery/Theft/Burglary",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Stolen Vehicle)"),
    "Grand Theft Auto",
    CITCHARGE_TYPE
  )) %>%
  mutate(CITCHARGE_TYPE = if_else(
    stringr::str_detect(CITCHARGE_TYPE, "(Carjacking)"),
    "Grand Theft Auto",
    CITCHARGE_TYPE
  ))

#
# testing <- extremely_simplified_force_type_and_reason_and_arrest %>%
#   count(CITCHARGE_TYPE)
# View(testing)
#
write_csv(
  extremely_simplified_force_type_and_reason_and_arrest,
  "../CleanData/UOF/extremely_simplified_force_type_and_reason_and_arrest.csv"
)



UOF <- read_csv("")
# #
# # # little tip, see all the number of occurances AND the unique observations, kinda useful
# # # testing <- UOF.df %>%
# # #   count(UOF_REASON)
# # # View(testing)
#


#### GROUPING BY TIME ####
simplifed_force_type_and_reason_and_arrest_by_year <-
  simplifed_force_type_and_reason_and_arrest %>%
  group_by(UOF_FORCE_TYPE, UOF_REASON, CIT_ARRESTED, OCCURRED_YEAR) %>%
  summarise("NUM_OF_OCCURANCES" = n()) %>% # renaming the count function in line
  arrange(UOF_FORCE_TYPE)

simplifed_force_type_and_reason_and_arrest_by_week_day <-
  simplifed_force_type_and_reason_and_arrest %>%
  group_by(UOF_FORCE_TYPE, UOF_REASON, CIT_ARRESTED, OCCURRED_WEEK_DAY) %>%
  summarise("NUM_OF_OCCURANCES" = n()) %>% # renaming the count function in line
  arrange(UOF_FORCE_TYPE)

simplifed_force_type_and_reason_and_arrest_by_quarter <-
  simplifed_force_type_and_reason_and_arrest %>%
  group_by(UOF_FORCE_TYPE, UOF_REASON, CIT_ARRESTED, OCCURRED_QUARTER) %>%
  summarise("NUM_OF_OCCURANCES" = n()) %>% # renaming the count function in line
  arrange(UOF_FORCE_TYPE)





simplifed_force_type_and_reason_and_arrest_by_year_graph <-
  simplifed_force_type_and_reason_and_arrest_by_year %>%
  ggplot(aes(x = UOF_FORCE_TYPE, y = NUM_OF_OCCURANCES)) +
  geom_point() +
  labs(title = "UOF occurances per year", subtitle = "Simplified UOF type, for instance grouping all uses of tear gas and the like") +
  ylab("Number of Occurances") +
  xlab("Use of Force Type") +
  facet_wrap(. ~ OCCURRED_YEAR) +
  theme(axis.text.x = element_text(
    vjust = 0.75,
    angle = 45,
    lineheight = 1.25
  )) +
  coord_flip()

# simplifed_force_type_and_reason_and_arrest_by_year_graph
#
# ggsave(
#   "../MediaOfFindings/UOF/simplifed_force_type_and_reason_and_arrest_by_year.png",
#   simplifed_force_type_and_reason_and_arrest_by_year_graph
# )


officer_race_per_year <- UOF.df %>%
  ggplot(mapping = aes(x = OCCURRED_YEAR, fill = OFF_RACE)) +
  geom_bar(alpha = 0.85) +
  scale_fill_hue(h = c(280, 0)) +
  # scale_fill_hue()
  labs(title = "Totally UOF per year, colored by officer race",
       subtitle = "Data only relevant until July 2019") +
  xlab("Year") +
  ylab("Number of occurances of UOF") +
  theme(axis.text.y = element_text(angle = 45))
# ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
#   geom_bar(alpha = 1/5, position = "identity")
#
# # officer_race_per_year
# ggsave("../MediaOfFindings/UOF/officer_race_per_year.png",
#        officer_race_per_year)


citizen_race_per_year <- UOF.df %>%
  ggplot(mapping = aes(x = OCCURRED_YEAR, fill = CIT_RACE)) +
  geom_bar(alpha = 0.85) +
  scale_fill_hue(h = c(0, 300)) +
  # scale_fill_hue()
  labs(title = "Totally UOF per year, colored by citizen race",
       subtitle = "Data only relevant until July 2019") +
  xlab("Year") +
  ylab("Number of occurances of UOF") +
  theme(axis.text.y = element_text(angle = 45))
# ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
#   geom_bar(alpha = 1/5, position = "identity")

# # citizen_race_per_year
# ggsave("../MediaOfFindings/UOF/citizen_race_per_year.png",
#        citizen_race_per_year)


citizen_officer_sex_per_year <- UOF.df %>%
  ggplot(mapping = aes(x = OCCURRED_YEAR, fill = OFF_SEX)) +
  geom_bar(alpha = 0.75) +
  scale_fill_hue(h = c(150, 0)) +
  # scale_fill_hue()
  labs(title = "Totally UOF occurances per year,\ncolored by officer sex and separated by citizen sex",
       subtitle = "Data only relevant until July 2019") +
  xlab("Year") +
  ylab("Number of occurances of UOF on citizen sex") +
  theme(axis.text.y = element_text(angle = 45)) +
  facet_wrap(facets = . ~ CIT_SEX, nrow = 3)  +
  guides(fill = guide_legend(title = "Officer Sex"))


# citizen_officer_sex_per_year
# ggsave(
#   "../MediaOfFindings/UOF/citizen_officer_sex_per_year.png",
#   citizen_officer_sex_per_year
# )



citizen_officer_race_per_year <- UOF.df %>%
  ggplot(mapping = aes(x = OCCURRED_YEAR, fill = OFF_RACE)) +
  geom_bar(alpha = 0.85) +
  scale_fill_hue(h = c(0, 300)) +
  # scale_fill_hue()
  labs(title = "Totally UOF occurances per year,\ncolored by officer race and separated by citizen race",
       subtitle = "Data only relevant until July 2019") +
  xlab("Year") +
  ylab("Number of occurances of UOF on citizen race") +
  theme(axis.text.y = element_text(angle = 45)) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(facets = . ~ CIT_RACE, nrow = 3)  +
  guides(fill = guide_legend(title = "Officer Race"))


# citizen_officer_race_per_year
# ggsave(
#   "../MediaOfFindings/UOF/citizen_officer_race_per_year.png",
#   citizen_officer_race_per_year
# )






citizen_officer_age_per_year <- UOF.df %>%
  ggplot(mapping = aes(x = CIT_AGE, y = OFF_AGE)) +
  geom_point(alpha = 0.85) +
  # scale_fill_hue(h = c(0, 300)) +
  # scale_fill_hue()
  labs(title = "tITLE",
       subtitle = "SUBTITLE") +
  xlab("CIT AGE") +
  ylab("OFFICER AGE") +
  theme(axis.text.y = element_text(angle = 45),
        axis.ticks.x = seq(1, 100, 10)) +
  theme(axis.text.x = element_text(angle = 45))
# theme(axis.ticks.x = seq(1, 100, 10))



# citizen_officer_age_per_year
# ggsave(
#   "../MediaOfFindings/UOF/citizen_officer_race_per_year.png",
#   citizen_officer_race_per_year
# )




byOfficerSex.vs.citSex <- ggplot(UOF.df,
                                 aes(x = OFF_SEX, fill = CIT_SEX)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Complaints by Citizen Sex vs Officer Sex")

# displaying chart
# byOfficerSex.vs.citSex
# ggsave("../MediaOfFindings/UOF/byOfficerSex.vs.citSex.png",
#        byOfficerSex.vs.citSex)


byOfficerRace.vs.citRace <- ggplot(UOF.df,
                                   aes(x = OFF_RACE, fill = CIT_RACE)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Complaints by Citizen Race vs Officer Race")

# displaying chart
# byOfficerRace.vs.citRace

# ggsave("../MediaOfFindings/UOF/byOfficerRace.vs.citRace.png",
#        byOfficerRace.vs.citRace)


graphed_by_number_of_occurances_per_year <- UOF.df %>%
  ggplot(aes(x = OCCURRED_YEAR, y = count(OCCURRED_YEAR))) +
  geom_point()

# graphed_by_number_of_occurances_per_year



byCitRace.vs.officerRace <- ggplot(UOF.df,
                                   aes(x = CIT_RACE, fill = OFF_RACE)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Complaints by Officer Race vs Citizen Race")

# displaying chart
# byCitRace.vs.officerRace





### ::FACETS:: ###

fingingsVsRace <- ggplot(UOF.df,
                         aes(x = CIT_RACE, fill = OFF_RACE)) +
  theme_bw() +
  facet_wrap(~ UOF.df$OCCURRED_YEAR) +
  geom_bar() +
  coord_flip() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Race vs Officer Race") +
  theme(axis.text.x = element_text(angle = 45))

# displaying chart
# fingingsVsRace

fingingsVsSex <-
  ggplot(
    extremely_simplified_force_type_and_reason_and_arrest,
    aes(x = CIT_SEX, fill = OFF_SEX)
  ) +
  theme_bw() +
  facet_wrap(~ extremely_simplified_force_type_and_reason_and_arrest$OCCURRED_YEAR) +
  geom_bar() +
  coord_flip() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Sex vs Officer Sex") +
  theme(axis.text.x = element_text(angle = 45))


# displaying chart
fingingsVsSex




#### WORKING GRAPHS ^^^ ####
