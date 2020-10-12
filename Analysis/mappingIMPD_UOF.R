library(tidyverse)
library(leaflet) # for maps
library(mapview) # saving maps (or making new ones)



UOF.df <- read_csv("../CleanData/UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv")
# View(UOF.df)


# replacing missing lat, lon data with the downtown pd location
# -86.15646	39.76852 
UOF_to_be_graphed <- UOF.df %>%
  tidyr::replace_na(list(lon = -86.15646, lat = 39.76852)) %>% 
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>% 
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>% 
  

  
  # View(UOF_to_be_graphed)
  
  # what I want in a graph (or multiple UOF_FORCE_TYPE, UOF_REASON, OCCURRED_WEEK_DAY, OCCURRED_QUARTER, OCCURRED_YEAR)
  
  tallyOfUniqueGeoLoc_ALL_YEARS_ALL_YEARS <- UOF_to_be_graphed %>% 
  group_by(latLon) %>% 
  tally() %>%  # tally returns a column named 'n'
  rename(OCCURANCES_AT_LOCATION = n)

# View(tallyOfUniqueGeoLoc_ALL_YEARS_ALL_YEARS)




# HAVING A PALETTE FOR COLOR DIFFERENCES IN THE DATA
# If you want to set your own colors manually:
# pal <- colorFactor(
#   palette = c('red', 'blue', 'green', 'purple', 'orange'),
#   domain = df$type
# )

# If you want to use predefined palettes in the RColorBrewer package:
# Call RColorBrewer::display.brewer.all() to see all possible palettes
RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)

palPaired <- colorFactor(
  palette = 'Paired',
  domain = tallyOfUniqueGeoLoc_ALL_YEARS$Year
)

palDark2 <- colorFactor(
  palette = 'Dark2',
  domain = tallyOfUniqueGeoLoc_ALL_YEARS$Year
)



uniqueOccurancesMap <- tallyOfUniqueGeoLoc_ALL_YEARS_ALL_YEARS %>%
  leaflet(options = c(
    leafletOptions(minZoom = 0, maxZoom = 18),
    leafletOptions(preferCanvas = T)
  )) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = testing$lon,
    lat = testing$lat,
    radius = tallyOfUniqueGeoLoc_ALL_YEARS$OCCURANCES_AT_LOCATION / 20,
    opacity = tallyOfUniqueGeoLoc_ALL_YEARS$OCCURANCES_AT_LOCATION / 100,
    popup = paste0("# of UOF here between 2014-2019:", tallyOfUniqueGeoLoc_ALL_YEARS$OCCURANCES_AT_LOCATION),
    label = tallyOfUniqueGeoLoc_ALL_YEARS$OCCURANCES_AT_LOCATION, 
    color = ~redPal(tallyOfUniqueGeoLoc_ALL_YEARS$OCCURANCES_AT_LOCATION/100)
  ) 






palPaired_byCitzenRace <- colorFactor(palette = 'Paired',
                                      domain = UOF_to_be_graphed$CIT_RACE)





UOF_graph_2019_cit_race_by_color <- UOF_to_be_graphed %>%
  filter(OCCURRED_YEAR == 2019) %>%
  leaflet(options = c(
    leafletOptions(minZoom = 0, maxZoom = 12),
    leafletOptions(preferCanvas = T)
  )) %>%
  addTiles(options = tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                 updateWhenIdle = TRUE)) %>%            # map won't load new tiles when panning)) %>%
  addCircleMarkers(
    lng = UOF_to_be_graphed$lon,
    lat = UOF_to_be_graphed$lat,
    popup = paste0(UOF_to_be_graphed$UOF_REASON, "; ", UOF_to_be_graphed$CITCHARGE_TYPE),
    label = paste0(
      UOF_to_be_graphed$UOF_FORCE_TYPE,
      " by ",
      stringr::str_to_lower(UOF_to_be_graphed$OFF_RACE),
      " officer"
    ),
    color = ~ palPaired_byCitzenRace(UOF_to_be_graphed$CIT_RACE),
    opacity = 0.75
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palPaired_byCitzenRace,
    values = UOF_to_be_graphed$CIT_RACE,
    opacity = 0.8,
    title = "11215 UOF Occurances in 2019 up to July 10th , citizen race by color"
  ) 

# using mapshot -to save the leaflet file as html
# issue about saving it as png is that phantomJs needs to be installed, just kinda over installing shit
# maybe it should be used as exploration and then built into a shiny app (since that'll be the end result anyways)
mapshot(UOF_graph_2019_cit_race_by_color, url = paste0("~/Projects/IMPD/MediaOfFindings/UOF", "/UOF_graph_2019_cit_race_by_color.html"))

# display map:
# UOF_graph_2019_cit_race_by_color



palDark2_byCitzenSex <- colorFactor(
  palette = 'Set2',
  domain = UOF_to_be_graphed$CIT_SEX
)


UOF_MAP_2019_cit_sex_by_color <- UOF_to_be_graphed %>%
  filter(OCCURRED_YEAR == 2019) %>%
  leaflet(options = c(
    leafletOptions(minZoom = 0, maxZoom = 18),
    leafletOptions(preferCanvas = T)
  )) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = UOF_to_be_graphed$lon,
    lat = UOF_to_be_graphed$lat,
    popup = UOF_to_be_graphed$UOF_REASON,
    label = paste0(
      UOF_to_be_graphed$UOF_FORCE_TYPE,
      " by ",
      stringr::str_to_lower(UOF_to_be_graphed$OFF_SEX),
      " officer"
    ),
    color = ~ palDark2_byCitzenSex(UOF_to_be_graphed$CIT_SEX)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palDark2_byCitzenSex,
    values = UOF_to_be_graphed$CIT_SEX,
    opacity = 0.8,
    title = "11215 UOF Occurances in 2019 up to July 10th, citizen sex by color"
  )  
### saving with mapview library
mapshot(UOF_MAP_2019_cit_sex_by_color, url = paste0("~/Projects/IMPD/MediaOfFindings/UOF", "/UOF_MAP_2019_cit_sex_by_color.html"))

# viewing the map
# UOF_graph_2019_cit_sex_by_color







### Ideas to do, check out the seaons as well as the time of day, might be enlightening




























#### MIGHT BE A FUN IDEA ####
# adding colors based on what year the occurance happened (this does work in the summer graph with the trial dataframe, 
# but this is labor intensive and probably won't scale)

yearIcons <- iconList(
  CE.2014 = makeIcon(iconUrl="https://img.icons8.com/ultraviolet/48/000000/marker.png"),
  CE.2015 = makeIcon(iconUrl="https://img.icons8.com/material-two-tone/48/000000/marker.png"),
  CE.2016 = makeIcon(iconUrl="https://img.icons8.com/nolan/64/000000/marker.png"),
  CE.2017 = makeIcon(iconUrl="https://img.icons8.com/doodle/48/000000/marker--v5.png"),
  CE.2018 = makeIcon(iconUrl="https://img.icons8.com/ultraviolet/48/000000/marker.png"),
  CE.2019 = makeIcon(iconUrl="https://img.icons8.com/wired/64/000000/marker.png")
)

# 
# # violet
# https://img.icons8.com/ultraviolet/48/000000/marker.png
# 
# # black outline on gray
# https://img.icons8.com/material-two-tone/48/000000/marker.png
# 
# # futororistic marker
# https://img.icons8.com/nolan/64/000000/marker.png
# 
# # drawn, doodle
# https://img.icons8.com/doodle/48/000000/marker--v5.png
# 
# # lightbulb? kinda
# https://img.icons8.com/wired/64/000000/marker.png
# 
# # lightening bolt
# https://img.icons8.com/ios/50/000000/marker-storm--v2.png



