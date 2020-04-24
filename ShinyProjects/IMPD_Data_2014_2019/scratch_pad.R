# # testing the heat map concept
# 
# library(tidyverse)
# library(leaflet)
# 
# officer_shootings_df <-
#   read_csv(
#     "../../CleaningRawData/Datasets/cleaned_officer_involved_shootings_with_lat_lon_zip_12_2019.csv"
#   )
# 
# 
# # UOF.df <- read_csv("../../CleaningRawData/Datasets/UOF_all___with_lat_lon_up_to_july_2019.csv")
# UOF_csv <- read_csv("../../CleaningRawData/Datasets/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv")
# # View(officer_shootings_df)
# 
# 
# # View(UOF_csv)
# UCR_with_year_csv <-
#   read_csv("./Datasets/UCR/simplied_crime_categories_2014_2019.csv")
# 
# complaints_csv <-
#   read_csv("./Datasets/Complaints/cleanedComplaint_data.csv")
# 
# demographics_IMPD_csv <-
#   read_csv("./Datasets/Demographics/cleaned_demographics_IMPD_as_of_2013.csv")
# 
# demographics_Indianapolis_csv <-
#   read_csv(
#     "./Datasets/Demographics/cleaned_Indianapolis_citizen_population_demographics_2010_2018.csv"
#   )
# 
# UCR.df <- UCR_with_year_csv
# UOF.df <- UOF_csv %>% select(-c(lat, lon))
# complaints.df <- complaints_csv
# demographics_IMPD_2013.df <- demographics_IMPD_csv %>%
#   select(c(Demographics = officer_race, Population = num_officers_2013))
# 
# demographics_Indianapolis.df <- demographics_Indianapolis_csv
# 
# demographics_Estimated_Population_2018 <-
#   demographics_Indianapolis.df %>% select(c(Demographics, Population = Estimated_Population_2018))
# demographics_Estimated_Population_2010 <-
#   demographics_Indianapolis.df %>% select(c(Demographics, Population = Estimated_Population_2010))
# demographics_Census_Population_2010 <-
#   demographics_Indianapolis.df %>% select(c(Demographics, Population = Census_Population_2010))
# 
# 
# 
# 
# count_zip <- UOF.df %>%
#   select(zip, OCCURRED_YEAR) %>%
#   # filter(OCCURRED_YEAR == 2015) %>%
#   count(zip) %>%
#   # tally() %>%  # tally returns a column named 'n'
#   dplyr::rename(OCCURANCES_AT_LOCATION = n)
# # %>%
# #   distinct(zip, .keep_all = TRUE)
# 
# # View(count_zip)
# 
# add_count_zip <- UOF.df %>%
#   select(zip, OCCURRED_YEAR) %>%
#   # filter(OCCURRED_YEAR == 2015) %>%
#   add_count(zip) %>%
#   # tally() %>%  # tally returns a column named 'n'
#   dplyr::rename(OCCURANCES_AT_LOCATION = n) %>%
#   distinct(zip, .keep_all = TRUE)
#   # distinct_at(vars(zip, OCCURRED_YEAR), .keep_all = TRUE)
# 
# # View(add_count_zip)
# 
# 
# race_bar_chart <- UOF.df %>%
#     filter(OCCURRED_YEAR == 2015) %>%
#     ggplot(aes(x = CIT_RACE, fill = OFF_RACE)) +
#     theme_minimal() +
#     # so this looks really cool as a facet, but not sure this is good for shiny
#     # facet_wrap( ~ UOF.df$OCCURRED_YEAR) +
#     geom_bar() +
#     labs(
#       y = "Number of reported",
#       x = "Citizen Race",
#       title = "Findings graphed by Citizen Race vs Officer Race",
#       fill = "Officer Race",
#       caption = "Data found on Indy.gov"
#     ) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1.25))
# race_bar_chart
# 
# 
# 
# ## super interested in this data, but need to wrangle it a bit more. promising
# scatter_zip <-  add_count_zip %>%
#   # filter(OCCURRED_YEAR == 2015) %>%
#   filter(zip > 46000 & zip < 50000) %>%
#   drop_na() %>%
#   # filter(OCCURANCES_AT_LOCATION > 50) %>%
#   ggplot(aes(x = zip, y = OCCURANCES_AT_LOCATION)) +
#   theme_minimal() +
#   geom_point(aes(color = factor(OCCURRED_YEAR), size = (OCCURANCES_AT_LOCATION))) +
#   # geom_label(mapping = aes(x), data = OCCURANCES_AT_LOCATION) +
#   # ggrepel::geom_label_repel(aes(
#   #   label = paste0(OCCURANCES_AT_LOCATION, " in ", zip)
#   # ), label.size = 0.05) +
#   # # geom_bar() +
#   labs(
#     y = "Number of reported UOF",
#     x = "Zip Code",
#     fill = "Number of reported UOF",
#     caption = "Data found on Indy.gov"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1.25))
# 
# # scatter_zip
# 
# 
# uof_occurances_2015 <- UOF.df %>%
#   select(zip, OCCURRED_YEAR) %>%
#   filter(OCCURRED_YEAR == 2015) %>%
#   add_count(zip) %>%
#   # tally() %>%  # tally returns a column named 'n'
#   dplyr::rename(OCCURANCES_AT_LOCATION = n) %>%
#   distinct(zip, .keep_all = TRUE)
# 
# # View(uof_occurances_2015)
# 
# # View(UOF.df)
# 
# locationByYear_zipcode <- UOF.df %>%
#       select(zip, OCCURRED_YEAR) %>%
#       filter(OCCURRED_YEAR == 2014) %>%
#       add_count(zip) %>%
#       dplyr::rename(OCCURANCES_AT_LOCATION = n) %>%
#       distinct(zip,  .keep_all = TRUE)
# 
# # View(locationByYear_zipcode)
# 
# 
# 
# 
# 
# 
# # this map is showing how many incidents of UOF at occurance
# map_UOF <- all_uof_occurances_2014_2019 %>%
#   leaflet(options = c(
#     leafletOptions(minZoom = 9, maxZoom = 18),
#     leafletOptions(preferCanvas = TRUE) # to speed up the rendering
#   )) %>%
#   setView(lng = -86.15646,
#           # loading base map before tiles or markers, for speed
#           lat = 39.76852,
#           zoom = 11) %>%
#   addMiniMap(position = "topright",
#              mapOptions = c(
#                tileOptions(updateWhenZooming = FALSE,
#                            updateWhenIdle = TRUE)
#              )) %>%
#   addProviderTiles(
#     providers$Stamen.TonerBackground,
#     options = c(
#       providerTileOptions(opacity = 0.85),
#       tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
#                   updateWhenIdle = TRUE)
#     )
#   ) %>% # map won't load new tiles when panning
#   addProviderTiles(providers$Esri.NatGeoWorldMap,
#                    options = c(
#                      providerTileOptions(opacity = 0.60),
#                      tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
#                                  updateWhenIdle = TRUE)
#                    )) %>%
#   addCircleMarkers(
#     lng = all_uof_occurances_2014_2019$lon,
#     lat = all_uof_occurances_2014_2019$lat,
#     popup = all_uof_occurances_2014_2019$OCCURANCES_AT_LOCATION,
#     label = paste0("Zip Code: ", all_uof_occurances_2014_2019$zip, " number of UOF occurances: ", all_uof_occurances_2014_2019$OCCURANCES_AT_LOCATION, " between 2014-2019"),
#     radius = all_uof_occurances_2014_2019$OCCURANCES_AT_LOCATION /50,
#     opacity = all_uof_occurances_2014_2019$OCCURANCES_AT_LOCATION /10
#   )
# map_UOF
# 
# 
# 
# 
# 
# 
# # replacing missing lat, lon data with the downtown pd location
# # -86.15646	39.76852
# cleaning_officer_shootings <- officer_shootings_df %>%
#   # tidyr::replace_na(list(longitude = -86.15646, latitude = 39.76852)) %>%
#   # unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
#   # mutate(OCCURRED_YEAR = lubridate::year(YMD_TM))  %>%
#   # group_by(INCNUM, latitude, longitude, zip ) %>%
#   # tally() %>%
#   # tally() %>%  # tally returns a column named 'n'
#   add_count(zip) %>%
#   dplyr::rename(OCCURANCES_AT_LOCATION = n)
# 
# x <- cleaning_officer_shootings
# # View(x)
# 
# 
# # this map is showing how many shootings have happened based on the zip code were there at the shootings
# map_officer_shootings <- x %>%
#   leaflet(options = c(
#     leafletOptions(minZoom = 0, maxZoom = 18),
#     leafletOptions(preferCanvas = T)
#   )) %>%
#   addTiles() %>%
#   addCircleMarkers(
#     lng = x$longitude,
#     lat = x$latitude,
#     popup = paste0("num of occurances: ", x$OCCURANCES_AT_LOCATION, " in ", x$zip, " zipcode"),
#     label = x$OCCURANCES_AT_LOCATION,
#     radius = x$OCCURANCES_AT_LOCATION * 2,
#     opacity = x$OCCURANCES_AT_LOCATION * 2
#   )
# 
# # map_officer_shootings
# 
# 
# 
# 
# 
# # UCR
# #
# # ?count
# # ?add_count
# # View(tetsing)
# UCR_with_year.df <- UCR.df %>%
#   select(YEAR, CRIME) %>%
#   group_by(YEAR, CRIME) %>%
#   filter(YEAR >= 2014) %>%
#   count(CRIME) %>%
#   rename("NUM_OF_OCCURANCES" = n)
# 
# # View(UCR_with_year.df)
# 
# UCR_with_year.df_add <- UCR.df %>%
#   select(YEAR, CRIME) %>%
#   group_by(YEAR, CRIME) %>%
#   filter(YEAR >= 2014) %>%
#   add_count(CRIME) %>%
#   rename("NUM_OF_OCCURANCES" = n)
# 
# # View(UCR_with_year.df_add)