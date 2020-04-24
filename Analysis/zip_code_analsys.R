# zip code exploration,
# graphs and GIS mapping

library(tidyverse)
library(leaflet)

UOF_zip <-
  readr::read_csv(
    "../CleaningRawData/Datasets/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv"
  )

# View(UOF_zip)

x <- UOF_zip %>%
  count(zip, name = "NUM_OF_OCCURANCES_ZIP")
# dplyr::distinct(zip, .keep_all = TRUE)
# View(x)

# UOF_zip$zip
wrangled_df <- UOF_zip %>%
  add_count(zip, name = "NUM_OF_OCCURANCES_ZIP")
# dplyr::distinct(zip, .keep_all = TRUE)

# class(wrangled_df$OCCURRED_YEAR)

# year19 <- wrangled_df  %>%
  # filter(OCCURRED_YEAR == 2019) %>% 
  # filter(zip < 60000 & zip > 40000) %>% 
  # drop_na()

# View(year19)

random_hex_colors <- c(
    '#7b71b3', '#540564', '#0a41ba', '#4a83d8',
    '#b02092', '#b78642', '#923a26', '#4976a5',
    '#2e9100', '#baecee', '#68deab', '#9e6ad5',
    '#42b971', '#b10eba', '#b7d348', '#81b807',
    '#e41b40', '#541020', '#3d56b9', '#7a3d34',
    '#6333a7', '#bd5a55', '#2636c9', '#33e7e4',
    '#ed5611', '#1ad5aa', '#ad3502', '#82a2e9',
    '#184279', '#a33dab', '#8b576b', '#e2e7b7',
    '#752ce3', '#51e018', '#a7adc3', '#ae2189',
    '#6b51c9', '#b015d9', '#7978ee', '#47b3c0',
    '#a20ac6', '#854833', '#b20042', '#b72b71',
    '#2d9a03', '#ba8041', '#5284a5', '#63ed31',
    '#b43a0a', '#52c26a', '#5de930', '#bd77d5',
    '#7a69dc', '#bccd60', '#224779', '#686763',
    '#578dea', '#67ad28', '#b6bd4c', '#22e797'
)




palPaired_byZip <- colorFactor(palette = random_hex_colors,
                               domain = year19$zip,
                               ordered = TRUE)

# factpal <- colorFactor(topo.colors(60), year19$zip)

leaflet_2019 <- year19 %>%
  leaflet(options = c(
    leafletOptions(minZoom = 9, maxZoom = 18),
    leafletOptions(preferCanvas = TRUE) # to speed up the rendering
  )) %>%
  # loading base map of Indianapolis before tiles or markers, for speed
  setView(lng = -86.15646,
          lat = 39.76852,
          zoom = 11) %>%
  addMiniMap(position = "topright",
             mapOptions = c(tileOptions(
               updateWhenZooming = FALSE,
               updateWhenIdle = TRUE
             ))) %>%
  addProviderTiles(providers$OpenStreetMap,
                   # two maps, for UI
                   # map won't update tiles until zoom is done, adds speed
                   # map won't load new tiles when panning
                   options = c(
                     providerTileOptions(opacity = 0.85),
                     tileOptions(updateWhenZooming = FALSE,
                                 updateWhenIdle = TRUE)
                   )) %>%
  addCircleMarkers(
    lat = year19$latitude,
    lng = year19$longitude,
    label = paste0(year19$NUM_OF_OCCURANCES_ZIP,
                   " ocurrances in ", year19$zip),
    popup = paste0( ~ palPaired_byZip(year19$zip)),
    color = ~ palPaired_byZip(year19$zip),
    opacity = 0.25 * year19$NUM_OF_OCCURANCES_ZIP
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palPaired_byZip,
    values = year19$zip,
    opacity = 1,
    # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
    title = "testing"
  )

# leaflet_2019





# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# RColorBrewer::display.brewer.all()


# play with having a base map to have less repeating dry my dude