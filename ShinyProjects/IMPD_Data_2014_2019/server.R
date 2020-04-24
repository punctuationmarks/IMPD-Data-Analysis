library(shiny)
library(tidyverse) # mainly used for piping
library(leaflet) # maps
library(DT) # for table rendering/prettifying
library(shinycssloaders) # for having a loading page to keep the audience entertained
library(shinythemes) # for styling overall app



# fuck haven't tested this yet, 
# added the zip code map to the server, not touched the ui, but preliminary set up was done earlier

# upload the most recent cleaned data here

# UOF_csv <-
#   read_csv("Datasets/UOF/cleaned_UOF_extremely_simplified_with_lat_lon_17_12_2019.csv")
# # View(UOF_csv)
UOF_csv <- read_csv("./Datasets/UOF/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv")

# View(UOF_csv)

# View(UOF_csv)
UCR_with_year_csv <-
  read_csv("./Datasets/UCR/simplied_crime_categories_2014_2019.csv")

complaints_csv <-
  read_csv("./Datasets/Complaints/cleanedComplaint_data.csv")

demographics_IMPD_csv <-
  read_csv("./Datasets/Demographics/cleaned_demographics_IMPD_as_of_2013.csv")

demographics_Indianapolis_csv <-
  read_csv(
    "./Datasets/Demographics/cleaned_Indianapolis_citizen_population_demographics_2010_2018.csv"
  )

UCR.df <- UCR_with_year_csv
UOF.df <- UOF_csv %>% select(-c(lat, lon))

complaints.df <- complaints_csv
demographics_IMPD_2013.df <- demographics_IMPD_csv %>%
  select(c(Demographics = officer_race, Population = num_officers_2013))

demographics_Indianapolis.df <- demographics_Indianapolis_csv

demographics_Estimated_Population_2018 <-
  demographics_Indianapolis.df %>% select(c(Demographics, Population = Estimated_Population_2018))
demographics_Estimated_Population_2010 <-
  demographics_Indianapolis.df %>% select(c(Demographics, Population = Estimated_Population_2010))
demographics_Census_Population_2010 <-
  demographics_Indianapolis.df %>% select(c(Demographics, Population = Census_Population_2010))





#### ORGANIZING DATA FOR PROCESSING, BUT THIS SHOULD BE DONE ELSEWHERE, BEFORE EVEN IMPORTINHG ###
complaints.df <- (
  complaints.df %>%
    select(
      ALG_CLASS,
      FINDING,
      ALLEGATION,
      INCIDENT_TYPE,
      SERVICE_TYPE,
      OFF_YR_EMPLOY,
      OFF_RACE,
      OFF_SEX,
      OFF_AGE,
      CIT_RACE,
      CIT_SEX,
      CIT_AGE,
      OCCURRED_DATE_AND_TIME,
      OCCURRED_YEAR,
      OCCURRED_MONTH,
      everything()
    ) %>%
    arrange(ALG_CLASS, FINDING)
)



# View(tetsing)
UCR_with_year.df <- UCR.df %>%
  select(YEAR, CRIME) %>%
  group_by(YEAR, CRIME) %>%
  filter(YEAR >= 2014) %>%
  count(CRIME) %>%
  rename("NUM_OF_OCCURANCES" = n)

# View(UCR_with_year.df)






### ### ### ### ### ### ### OPTIONS FOR GRAPHS/MAPS/SPINNERS/ANYTHING EXTRA ### ### ### ### ### ### ###


# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# RColorBrewer::display.brewer.all()


# # Pass the palette function a data vector to get the corresponding colors
#

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
                               domain = UOF.df$zip,
                               ordered = TRUE)


# colorFactor()
palPaired_byCitzenRace <- colorFactor(palette = "Set1",
                                      domain = UOF.df$CIT_RACE,
                                      ordered = TRUE)
# reverse = TRUE)
# ,
# # reverse is due to the massive amount of white citizens, for ease of viewing
# reverse = TRUE)


palPaired_byCitzenSex <-
  colorFactor(palette = "Set1",
              domain = UOF.df$CIT_SEX)


palPaired_byYear <- colorFactor(palette = "Set1",
                                domain = UOF.df$OCCURRED_YEAR)

# OPTION FOR SPINNERS WHILE LOADING GRAPHS/MAPS
options(
  spinner.color.background = "#F5F5F5",
  spinner.color = "#000000",
  spinner.type = 3
)





### ### ### ### ### ### ### SERVER ### ### ### ### ### ### ###


server <- function(input, output, session) {
  ### Reactive Functions ###
  
  locationByYear_race <-
    eventReactive(input$userSelectedYearRace_leafletMap_UOF, {
      UOF.df %>%
        filter(OCCURRED_YEAR == input$userSelectedYearRace_leafletMap_UOF) %>%
        distinct(INCNUM, .keep_all = TRUE)
    }, ignoreNULL = FALSE)
  
  
  locationByYear_sex <-
    eventReactive(input$userSelectedYearSex_leafletMap, {
      UOF.df %>%
        filter(OCCURRED_YEAR == input$userSelectedYearSex_leafletMap) %>%
        distinct(INCNUM, .keep_all = TRUE)
    }, ignoreNULL = FALSE)
  
  
  # locationByYear_zipcode <-
  #   eventReactive(input$userSelectedYearZipcode_leafletMap, {
  #     UOF.df %>%
  #       filter(zip < 60000 & zip > 40000) %>% 
  #       # drop_na() %>% 
  #       filter(OCCURRED_YEAR == input$userSelectedYearZipcode_leafletMap) %>%
  #       add_count(zip, name = "NUM_OF_OCCURANCES_ZIP") %>%
  #       distinct(INCNUM, .keep_all=TRUE)
  #   }, ignoreNULL = FALSE)
  # 
  
  locationByYear_zipcode <-
    eventReactive(input$userSelectedYearZipcode_leafletMap, {
      UOF.df %>%
        # select(zip, OCCURRED_YEAR, latitude, longitude) %>%
        filter(zip < 60000 & zip > 40000) %>% 
        filter(OCCURRED_YEAR == input$userSelectedYearZipcode_leafletMap) %>%
        add_count(zip) %>%
        dplyr::rename(NUM_OF_OCCURANCES_ZIP = n)
        # distinct(zip, .keep_all = TRUE) 
    }, ignoreNULL = FALSE)
  
  
  locationByQuarter <-
    eventReactive(input$userSelectedQuarter_leafletMap, {
      UOF.df %>%
        filter(OCCURRED_QUARTER == input$userSelectedQuarter_leafletMap) %>%
        distinct(INCNUM, .keep_all = TRUE)
    }, ignoreNULL = FALSE)
  
  
  locationByTime <-
    eventReactive(input$userSelectedTimeOfDay_leafletMap, {
      UOF.df %>%
        filter(OCCURRED_HOUR == input$userSelectedTimeOfDay_leafletMap) %>%
        distinct(INCNUM, .keep_all = TRUE)
    }, ignoreNULL = FALSE)
  
  
  complaintsByYear <- reactive({
    switch(
      input$complaint_year_9999,
      "2014" = 2014,
      "2015" = 2015,
      "2016" = 2016,
      "2017" = 2017,
      "2018" = 2018,
      "2019" = 2019,
      "2014-2019" = c(2014, 2015, 2016, 2017, 2018, 2019)
      # "Unreported" = "Unreported"
    )
  })  
  
  
  ucrByYear <- reactive({
    switch(
      input$complaint_year_9999,
      "2014" = 2014,
      "2015" = 2015,
      "2016" = 2016,
      "2017" = 2017,
      "2018" = 2018,
      "2019" = 2019,
      "2014-2019" = c(2014, 2015, 2016, 2017, 2018, 2019)
      # "Unreported" = "Unreported"
    )
  })
  
  
  demographicsDatasets <- reactive({
    switch(
      input$demographic_dataset,
      "IMPD Racial Demographics 2013" = demographics_IMPD_2013.df,
      "Indianapolis 2010 Census Demographics" = demographics_Census_Population_2010,
      "Estiamted Indianapolis 2010 Demographics" = demographics_Estimated_Population_2010,
      "Estiamted Indianapolis 2018 Demographics" = demographics_Estimated_Population_2018
    )
  })
  
  
  
  ### ### ### ### ### ### ### UOF CHARTS ### ### ### ### ### ### ###
  
  output$UOF.barchart_sex <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_withSex_ggBarChart) %>%
      ggplot(aes(x = OFF_SEX, fill = CIT_SEX)) +
      theme_minimal() +
      geom_bar() +
      labs(
        y = "Number of reported",
        x = "Officer Sex",
        title = "Findings graphed by Citizen Sex vs Officer Sex",
        fill = "Citizen Sex",
        caption = "Data found on Indy.gov"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.25))
  })
  
  
  output$UOF.barchart_race <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_withRace_ggBarChart) %>%
      ggplot(aes(x = CIT_RACE, fill = OFF_RACE)) +
      theme_minimal() +
      # so this looks really cool as a facet, but not sure this is good for shiny
      # facet_wrap( ~ UOF.df$OCCURRED_YEAR) +
      geom_bar() +
      labs(
        y = "Number of reported",
        x = "Citizen Race",
        title = "Findings graphed by Citizen Race vs Officer Race",
        fill = "Officer Race",
        caption = "Data found on Indy.gov"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.25))
  })
  
  
  
  ### FACETS BY YEAR###
  
  output$ggplot_facetted_by_year <- renderPlot({
    x_data_point <-
      switch(
        input$userSelected_X_input_for_facets_ggbar,
        "Citizen Sex" = UOF.df$CIT_SEX,
        "Citizen Race" = UOF.df$CIT_RACE,
        "Citizen Age" = UOF.df$CIT_AGE
      )
    
    fill_data_point <-
      switch(
        input$userSelected_fill_input_for_facets_ggbar,
        "Officer Sex" = UOF.df$OFF_SEX,
        "Officer Race" = UOF.df$OFF_RACE,
        "Officer Age" = UOF.df$OFF_AGE,
        "Citizen Charge Type" = UOF.df$CITCHARGE_TYPE
      )
    
    
    ggplot(UOF.df, aes(x = x_data_point,
                       fill = fill_data_point)) +
      geom_bar() +
      theme_minimal() +
      facet_wrap(~ UOF.df$OCCURRED_YEAR) +
      labs(
        title = paste0(
          "Findings graphed ",
          input$userSelected_X_input_for_facets_ggbar,
          " by ",
          input$userSelected_fill_input_for_facets_ggbar
        ),
        x = input$userSelected_X_input_for_facets_ggbar,
        y = paste0("Count of Occurances"),
        fill = paste0(
          "Fill by ",
          input$userSelected_fill_input_for_facets_ggbar
        )
      )  +
      theme(axis.text.x = element_text(angle = 90,
                                       size = 9))
  })
  
  
  
  
  ### ### ### ### ### ### ### Leaflet maps ##
  ### how to combine these in a shiny way? less code, more reuseable?
  
  # output$UOF.map_ZIP <- renderLeaflet({
  #   leaflet(options = c(
  #     leafletOptions(minZoom = 9, maxZoom = 18),
  #     leafletOptions(preferCanvas = TRUE) # to speed up the rendering
  #   )) %>%
  #     # loading base map of Indianapolis before tiles or markers, for speed
  #     setView(lng = -86.15646,
  #             lat = 39.76852,
  #             zoom = 11) %>%
  #     addMiniMap(position = "topright",
  #                mapOptions = c(tileOptions(
  #                  updateWhenZooming = FALSE,
  #                  updateWhenIdle = TRUE
  #                ))) %>%
  #     addProviderTiles(
  #       providers$Stamen.TonerBackground,
  #       options = c(
  #         providerTileOptions(opacity = 0.85),
  #         tileOptions(updateWhenZooming = FALSE,
  #                     updateWhenIdle = TRUE)
  #       )
  #     ) %>%
  #     addProviderTiles(providers$Esri.NatGeoWorldMap,
  #                      options = c(
  #                        providerTileOptions(opacity = 0.60),
  #                        tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
  #                                    updateWhenIdle = TRUE)
  #                      )) %>% 
  #     addCircleMarkers(
  #       data = locationByYear_zipcode(),
  #       # lat = locationByYear_zipcode()$latitude,
  #       # lng = locationByYear_zipcode()$longitude,
  #       label = paste0(locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP,
  #                      " ocurrances in ", locationByYear_zipcode()$zip),
  #       popup = paste0(locationByYear_zipcode()$UOF_REASON, " - ", locationByYear_zipcode()$CITCHARGE_TYPE),
  #       color = ~ palPaired_byZip(locationByYear_zipcode()$zip),
  #       opacity = 0.25 * locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = palPaired_byZip,
  #       values = locationByYear_zipcode()$zip,
  #       opacity = 1,
  #       # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
  #       title = "testing"
  #     )
  # })
  # 
  
  
  
  output$UOF.map_race <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 9, maxZoom = 18),
      leafletOptions(preferCanvas = TRUE) # to speed up the rendering
    )) %>%
      # loading base map of Indianapolis before tiles or markers, for speed
      setView(lng = -86.15646,
              lat = 39.76852,
              zoom = 11) %>%
      addMiniMap(position = "topright",
                 mapOptions = c(
                   tileOptions(updateWhenZooming = FALSE,
                               updateWhenIdle = TRUE)
                 )) %>%
      addProviderTiles(
        providers$Stamen.TonerBackground,
        # two maps, for UI
        # map won't update tiles until zoom is done, adds speed
        # map won't load new tiles when panning
        options = c(
          providerTileOptions(opacity = 0.85),
          tileOptions(updateWhenZooming = FALSE,
                      updateWhenIdle = TRUE)
        )
      ) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = c(
                         providerTileOptions(opacity = 0.60),
                         tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)
                       )) %>%
      addCircleMarkers(
        data = locationByYear_race(),
        label = paste0(
          locationByYear_race()$UOF_REASON,
          "; ",
          locationByYear_race()$CITCHARGE_TYPE
        ),
        popup = paste0(
          locationByYear_race()$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_title(locationByYear_race()$OFF_RACE),
          " Officer (",
          locationByYear_race()$OFF_AGE,
          ") on ",
          stringr::str_to_title(locationByYear_race()$CIT_RACE),
          " Citizen (",
          locationByYear_race()$CIT_AGE,
          "). INCNUM: ",
          locationByYear_race()$INCNUM
        ),
        color = ~ palPaired_byCitzenRace(locationByYear_race()$CIT_RACE),
        opacity = 1
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palPaired_byCitzenRace,
        values = locationByYear_race()$CIT_RACE,
        opacity = 1,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = sprintf(
          "Color by Citizen race. %s unique use of force occurances in %s",
          count(locationByYear_race()),
          input$userSelectedYearRace_leafletMap_UOF
        )
        
      )
    
  })
  
  
  output$UOF.map_sex <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 9, maxZoom = 18),
      leafletOptions(preferCanvas = TRUE) # to speed up the rendering
    )) %>%
      setView(lng = -86.15646,
              # loading base map before tiles or markers, for speed
              lat = 39.76852,
              zoom = 11) %>%
      addMiniMap(position = "topright",
                 mapOptions = c(
                   tileOptions(updateWhenZooming = FALSE,
                               updateWhenIdle = TRUE)
                 )) %>%
      addProviderTiles(
        providers$Stamen.TonerBackground,
        options = c(
          providerTileOptions(opacity = 0.85),
          tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                      updateWhenIdle = TRUE)
        )
      ) %>% # map won't load new tiles when panning
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = c(
                         providerTileOptions(opacity = 0.60),
                         tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)
                       )) %>%
      addCircleMarkers(
        data = locationByYear_sex(),
        label = paste0(
          locationByYear_sex()$UOF_REASON,
          "; ",
          locationByYear_sex()$CITCHARGE_TYPE
        ),
        popup = paste0(
          locationByYear_sex()$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_title(locationByYear_sex()$OFF_SEX),
          " Officer (",
          locationByYear_sex()$OFF_AGE,
          ") on ",
          stringr::str_to_title(locationByYear_sex()$CIT_SEX),
          " Citizen (",
          locationByYear_sex()$CIT_AGE,
          "). INCNUM: ",
          locationByYear_sex()$INCNUM
        ),
        color = ~ palPaired_byCitzenSex(locationByYear_sex()$CIT_SEX),
        opacity = 0.85
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palPaired_byCitzenSex,
        values = locationByYear_sex()$CIT_SEX,
        opacity = 1,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = sprintf(
          "Color by Citizen sex. %s unique use of force occurances in %s.",
          count(locationByYear_race()),
          input$userSelectedYearRace_leafletMap_UOF
        )
      )
  })  
  
  # output$UOF.map_zip <- renderLeaflet({
  #   leaflet(options = c(
  #     leafletOptions(minZoom = 9, maxZoom = 18),
  #     leafletOptions(preferCanvas = TRUE) # to speed up the rendering
  #   )) %>%
  #     setView(lng = -86.15646,
  #             # loading base map before tiles or markers, for speed
  #             lat = 39.76852,
  #             zoom = 11) %>%
  #     addMiniMap(position = "topright",
  #                mapOptions = c(
  #                  tileOptions(updateWhenZooming = FALSE,
  #                              updateWhenIdle = TRUE)
  #                )) %>%
  #     addProviderTiles(
  #       providers$Stamen.TonerBackground,
  #       options = c(
  #         providerTileOptions(opacity = 0.85),
  #         tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
  #                     updateWhenIdle = TRUE)
  #       )
  #     ) %>% # map won't load new tiles when panning
  #     addProviderTiles(providers$Esri.NatGeoWorldMap,
  #                      options = c(
  #                        providerTileOptions(opacity = 0.60),
  #                        tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
  #                                    updateWhenIdle = TRUE)
  #                      )) %>%
  #     addCircleMarkers(
  #       data = locationByYear_zipcode(),
  #       popup = paste0(locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP, " UOF occurances in ", locationByYear_zipcode()$zip, " in ", locationByYear_zipcode()$OCCURRED_YEAR),
  #       label = paste0(locationByYear_zipcode()$zip),
  #       color = ~ palPaired_byZip(locationByYear_zipcode()$zip),
  #       radius = locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP / 5, # dividing here to make them fit on the map
  #       opacity = locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP /5 # allowing for each cirlce to be seen 
  #     )
  #   })
  
  
  output$UOF.map_zip <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 9, maxZoom = 18),
      leafletOptions(preferCanvas = TRUE) # to speed up the rendering
    )) %>%
      setView(lng = -86.15646,
              # loading base map before tiles or markers, for speed
              lat = 39.76852,
              zoom = 11) %>%
      addMiniMap(position = "topright",
                 mapOptions = c(
                   tileOptions(updateWhenZooming = FALSE,
                               updateWhenIdle = TRUE)
                 )) %>%
      addProviderTiles(
        providers$Stamen.TonerBackground,
        options = c(
          providerTileOptions(opacity = 0.85),
          tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                      updateWhenIdle = TRUE)
        )
      ) %>% # map won't load new tiles when panning
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = c(
                         providerTileOptions(opacity = 0.60),
                         tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)
                       )) %>%
      addCircleMarkers(
        data = locationByYear_zipcode(),
        label = paste0(locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP, " UOF occurances in ", locationByYear_zipcode()$zip, " in ", locationByYear_zipcode()$OCCURRED_YEAR),
        popup = paste0(
          stringr::str_to_title(locationByYear_zipcode()$UOF_FORCE_TYPE),
          " INCNUM: ",
          locationByYear_zipcode()$INCNUM
        ),
        color = ~ palPaired_byZip(locationByYear_zipcode()$zip),
        opacity = 0.25 * locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP)
    })
  
  
  
  output$UOF.map_quarter <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 9, maxZoom = 18),
      leafletOptions(preferCanvas = TRUE)
    )) %>%
      setView(lng = -86.15646,
              lat = 39.76852,
              zoom = 11) %>%
      addMiniMap(position = "topright",
                 mapOptions = c(
                   tileOptions(updateWhenZooming = FALSE,
                               updateWhenIdle = TRUE)
                 )) %>%
      addProviderTiles(
        providers$Stamen.TonerBackground,
        options = c(
          providerTileOptions(opacity = 0.85),
          tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                      updateWhenIdle = TRUE)
        )
      ) %>% # map won't load new tiles when panning
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = c(
                         providerTileOptions(opacity = 0.60),
                         tileOptions(updateWhenZooming = FALSE,
                                     updateWhenIdle = TRUE)
                       )) %>%
      addCircleMarkers(
        data = locationByQuarter(),
        label = paste0(
          locationByQuarter()$UOF_REASON,
          "; ",
          locationByQuarter()$CITCHARGE_TYPE
        ),
        popup = paste0(
          locationByQuarter()$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_title(locationByQuarter()$OFF_RACE),
          ", ",
          stringr::str_to_title(locationByQuarter()$OFF_SEX),
          " Officer (",
          locationByQuarter()$OFF_AGE,
          ") on ",
          str_to_title(locationByQuarter()$CIT_RACE),
          ", ",
          str_to_title(locationByQuarter()$CIT_SEX),
          " Citizen (",
          locationByQuarter()$CIT_AGE,
          "), ",
          locationByQuarter()$OCCURRED_YEAR,
          ". INCNUM: ",
          locationByQuarter()$INCNUM
        ),
        color = ~ palPaired_byYear(locationByQuarter()$OCCURRED_YEAR),
        opacity = 0.8
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palPaired_byYear,
        values = locationByQuarter()$OCCURRED_YEAR,
        opacity = 1,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = sprintf(
          "Color by Year. %s unique use of force occurances in quarter %s from 2014-2019",
          count(locationByQuarter()),
          input$userSelectedQuarter_leafletMap
        )
        
      )
  })
  
  
  output$UOF.map_time_of_day <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 9, maxZoom = 18),
      leafletOptions(preferCanvas = TRUE)
    )) %>%
      setView(lng = -86.15646,
              lat = 39.76852,
              zoom = 11) %>%
      addMiniMap(position = "topright",
                 mapOptions = c(
                   tileOptions(updateWhenZooming = FALSE,
                               updateWhenIdle = TRUE)
                 )) %>%
      addProviderTiles(
        providers$Stamen.TonerBackground,
        options = c(
          providerTileOptions(opacity = 0.85),
          tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                      updateWhenIdle = TRUE)
        )
      ) %>% # map won't load new tiles when panning
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = c(
                         providerTileOptions(opacity = 0.60),
                         tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)
                       )) %>%
      addCircleMarkers(
        data = locationByTime(),
        label = paste0(
          locationByTime()$UOF_REASON,
          "; ",
          locationByTime()$CITCHARGE_TYPE
        ),
        popup = paste0(
          locationByTime()$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_title(locationByTime()$OFF_RACE),
          ", ",
          stringr::str_to_title(locationByTime()$OFF_SEX),
          " Officer (",
          locationByTime()$OFF_AGE,
          ") on ",
          str_to_title(locationByTime()$CIT_RACE),
          ", ",
          str_to_title(locationByTime()$CIT_SEX),
          " Citizen (",
          locationByTime()$CIT_AGE,
          "), ",
          locationByTime()$OCCURRED_YEAR,
          ". INCNUM: ",
          locationByTime()$INCNUM
        ),
        color = ~ palPaired_byYear(locationByTime()$OCCURRED_YEAR),
        opacity = 0.8
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palPaired_byYear,
        values = locationByTime()$OCCURRED_YEAR,
        opacity = 1,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = sprintf(
          "Color by year. %s unique use of force occurances in hour %s from 2014-2019. ",
          count(locationByTime()),
          input$userSelectedTimeOfDay_leafletMap
        )
      )
  })
  
  
  
  
  
  ### ### ### ### ### ### ### UCR Data ##
  
  # need to adjust the y axis vheight (since it's too close to the UCR crime type (just by 0.1 or so))
  output$UCR.ggpoint <- renderPlot({
    UCR_with_year.df %>%
      select(YEAR, CRIME, NUM_OF_OCCURANCES) %>%
      filter(YEAR == input$userSelectedYear_UCR) %>%
      ggplot(aes(x = CRIME, y = NUM_OF_OCCURANCES)) +
      geom_point() +
      coord_flip() +
      ggrepel::geom_label_repel(aes(
        label = paste0(NUM_OF_OCCURANCES, ", ", stringr::str_to_sentence(CRIME))
      ), label.size = 0.15) +
      labs(
        title = paste0("IMPD UCR for ", input$userSelectedYear_UCR),
        y = "Number of Occurances",
        x = "Reported Crime",
        caption = "Data found on Indy.gov"
      ) +
      theme(axis.text.x = element_text(
        vjust = 0.75,
        angle = 45,
        lineheight = 1
      )) +
      theme(axis.title.y = element_text(vjust = 1.5))
  })
  
  
  
  ### ### ### ### ### ### ### Complaints ###
  output$Complains_Allegation.geom_point <- renderPlot({
    complaints.df %>%
      filter(OCCURRED_YEAR == complaintsByYear()) %>%
      group_by(ALG_CLASS, OCCURRED_YEAR) %>%
      tally() %>%  # tally returns a column named 'n'
      rename(`Number of Occurances` = n) %>%
      ggplot2::ggplot(aes(x = `Number of Occurances`, y = ALG_CLASS)) +
      ggplot2::geom_point(aes(color = ALG_CLASS,
                              size = `Number of Occurances`)) +
      ggplot2::theme(legend.position = "none") +
      ggrepel::geom_label_repel(aes(label = sprintf(
        "%s in %s class", `Number of Occurances`, ALG_CLASS
      ))) +
      labs(
        title = sprintf(
          "Tally of allegations (sorted by class, not unique occurance) from citizen complaints against the IMPD in %s",
          complaintsByYear()
        )
      ) +
      xlab("Number of allegations in class") +
      ylab("Allegation Class")
  })
  
  
  ### ### ### ### ### ### ### Demographics ###
  output$demographics.graph <- renderPlot({
    demographicsDatasets() %>%
      ggplot(aes(x = Population, y = Demographics)) +
      geom_point(aes(color = Demographics,
                     size = Population)) +
      ggrepel::geom_label_repel(aes(label = sprintf("%s: %s", Demographics, Population))) +
      labs(title = sprintf("%s", input$demographic_dataset)) +
      ggplot2::theme(legend.position = "none")
  })
  
  
  ### ### ### ### ### ### ### Database Tables ###
  
  output$UOF.df_data_table <- DT::renderDataTable(DT::datatable({
    UOF_csv[, input$UOF_variables, drop = FALSE]
  }))
  
  output$complaints.df_data_table <- renderDataTable(datatable({
    complaints.df[, input$complaint_variables, drop = FALSE]
  }))
  
  output$UCR.df_data_table <- renderDataTable(datatable({
    UCR.df[, input$UCR_variables, drop = FALSE]
  }))
}
