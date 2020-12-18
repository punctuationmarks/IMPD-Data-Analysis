
# All imports are in global.R ---------------------------------------------






### ### ### ### ### ### ### OPTIONS FOR GRAPHS/MAPS/SPINNERS/ANYTHING EXTRA ### ### ### ### ### ### ###








### ### ### ### ### ### ### SERVER ### ### ### ### ### ### ###

server <- function(input, output, session) {
  ### Reactive Functions ###
  
  locationByYear_race <-
    eventReactive(
      c(
        input$citizen_race,
        input$citizen_sex,
        input$officer_race,
        input$officer_sex,
        input$year_occured,
        input$quarter_occured,
        input$time_occured,
        input$occured_in_zip
      ),
      {
        UOF.df %>%
          
          # UI input conditionals
          
          # NOTE ON THE SYNTAX OF CONDITIONALS:
          # this syntax is needed to bypass filter (meaning don't filter anything is "all" is passed)
          # filter(if(input$officer_sex == "All") {TRUE} else {CIT_SEX == input$officer_sex}) #
          
          # citizen related
          filter(if (input$citizen_race == "All") {
            TRUE
          } else {
            CIT_RACE == input$citizen_race
          }) %>%
          
          filter(if (input$citizen_sex == "All") {
            TRUE
          } else {
            CIT_SEX == input$citizen_sex
          }) %>%
          # officer related
          
          filter(if (input$officer_race == "All") {
            TRUE
          } else {
            OFF_RACE == input$officer_race
          }) %>%
          
          filter(if (input$officer_sex == "All") {
            TRUE
          } else {
            OFF_SEX == input$officer_sex
          }) %>%
          
          # time related
          filter(if (input$year_occured == "All") {
            TRUE
          } else {
            OCCURRED_YEAR == input$year_occured
          }) %>%
          
          filter(if (input$quarter_occured == "All") {
            TRUE
          } else {
            OCCURRED_QUARTER == input$quarter_occured
          }) %>%
          
          filter(if (input$time_occured == "All") {
            TRUE
          } else {
            OCCURRED_HOUR == input$time_occured
          }) %>%
          
          # by location
          filter(if (input$occured_in_zip == "All") {
            TRUE
          } else {
            zip == input$occured_in_zip
          }) %>%
          
          distinct(INCNUM, .keep_all = TRUE)
      },
      ignoreNULL = FALSE
    )

  
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
  
  
  
  ### ### ### ### ### ### ### UOF Graphs ### ### ### ### ### ### ###
  
  ### bar chart
  
  
  output$UOF.barchart_sex <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$year_occured_barchart) %>%
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
      facet_wrap( ~ UOF.df$OCCURRED_YEAR) +
      labs(
        title = paste0(
          "Findings graphed ",
          input$userSelected_X_input_for_facets_ggbar,
          " by ",
          input$userSelected_fill_input_for_facets_ggbar
        ),
        x = input$userSelected_X_input_for_facets_ggbar,
        y = paste0("Count of Occurances"),
        fill = paste0("Fill by ",
                      input$userSelected_fill_input_for_facets_ggbar)
      )  +
      theme(axis.text.x = element_text(angle = 90,
                                       size = 9))
  })
  
  
  
  
  ### ### ### ### ### ### ### Leaflet maps ##
  ### how to combine these in a shiny way? less code, more reuseable?
  
  
  
  output$UOF.map <- renderLeaflet({
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
      addProviderTiles(providers$Stamen.TonerBackground,
                       # two maps, for UI
                       # map won't update tiles until zoom is done, adds speed
                       # map won't load new tiles when panning
                       options = c(
                         providerTileOptions(opacity = 0.85),
                         tileOptions(updateWhenZooming = FALSE,
                                     updateWhenIdle = TRUE)
                       )) %>%
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
          input$year_occured
        )
        
      )
    
  })
  
  # 
  # output$UOF.map_sex <- renderLeaflet({
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
  #       data = locationByYear_sex(),
  #       label = paste0(
  #         locationByYear_sex()$UOF_REASON,
  #         "; ",
  #         locationByYear_sex()$CITCHARGE_TYPE
  #       ),
  #       popup = paste0(
  #         locationByYear_sex()$UOF_FORCE_TYPE,
  #         " by ",
  #         stringr::str_to_title(locationByYear_sex()$OFF_SEX),
  #         " Officer (",
  #         locationByYear_sex()$OFF_AGE,
  #         ") on ",
  #         stringr::str_to_title(locationByYear_sex()$CIT_SEX),
  #         " Citizen (",
  #         locationByYear_sex()$CIT_AGE,
  #         "). INCNUM: ",
  #         locationByYear_sex()$INCNUM
  #       ),
  #       color = ~ palPaired_byCitzenSex(locationByYear_sex()$CIT_SEX),
  #       opacity = 0.85
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = palPaired_byCitzenSex,
  #       values = locationByYear_sex()$CIT_SEX,
  #       opacity = 1,
  #       # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
  #       title = sprintf(
  #         "Color by Citizen sex. %s unique use of force occurances in %s.",
  #         count(locationByYear_race()),
  #         input$year_occured
  #       )
  #     )
  # })  
  # 
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
  #       label = paste0(locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP, " UOF occurances in ", locationByYear_zipcode()$zip, " in ", locationByYear_zipcode()$OCCURRED_YEAR),
  #       popup = paste0(
  #         stringr::str_to_title(locationByYear_zipcode()$UOF_FORCE_TYPE),
  #         " INCNUM: ",
  #         locationByYear_zipcode()$INCNUM
  #       ),
  #       color = ~ palPaired_byZip(locationByYear_zipcode()$zip),
  #       opacity = 0.25 * locationByYear_zipcode()$NUM_OF_OCCURANCES_ZIP)
  #   })
  # 
  # 
  # 
  # output$UOF.map_quarter <- renderLeaflet({
  #   leaflet(options = c(
  #     leafletOptions(minZoom = 9, maxZoom = 18),
  #     leafletOptions(preferCanvas = TRUE)
  #   )) %>%
  #     setView(lng = -86.15646,
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
  #                        tileOptions(updateWhenZooming = FALSE,
  #                                    updateWhenIdle = TRUE)
  #                      )) %>%
  #     addCircleMarkers(
  #       data = locationByQuarter(),
  #       label = paste0(
  #         locationByQuarter()$UOF_REASON,
  #         "; ",
  #         locationByQuarter()$CITCHARGE_TYPE
  #       ),
  #       popup = paste0(
  #         locationByQuarter()$UOF_FORCE_TYPE,
  #         " by ",
  #         stringr::str_to_title(locationByQuarter()$OFF_RACE),
  #         ", ",
  #         stringr::str_to_title(locationByQuarter()$OFF_SEX),
  #         " Officer (",
  #         locationByQuarter()$OFF_AGE,
  #         ") on ",
  #         str_to_title(locationByQuarter()$CIT_RACE),
  #         ", ",
  #         str_to_title(locationByQuarter()$CIT_SEX),
  #         " Citizen (",
  #         locationByQuarter()$CIT_AGE,
  #         "), ",
  #         locationByQuarter()$OCCURRED_YEAR,
  #         ". INCNUM: ",
  #         locationByQuarter()$INCNUM
  #       ),
  #       color = ~ palPaired_byYear(locationByQuarter()$OCCURRED_YEAR),
  #       opacity = 0.8
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = palPaired_byYear,
  #       values = locationByQuarter()$OCCURRED_YEAR,
  #       opacity = 1,
  #       # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
  #       title = sprintf(
  #         "Color by Year. %s unique use of force occurances in quarter %s from 2014-2019",
  #         count(locationByQuarter()),
  #         input$quarter_occured
  #       )
  #       
  #     )
  # })
  # 
  # 
  # output$UOF.map_time_of_day <- renderLeaflet({
  #   leaflet(options = c(
  #     leafletOptions(minZoom = 9, maxZoom = 18),
  #     leafletOptions(preferCanvas = TRUE)
  #   )) %>%
  #     setView(lng = -86.15646,
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
  #       data = locationByTime(),
  #       label = paste0(
  #         locationByTime()$UOF_REASON,
  #         "; ",
  #         locationByTime()$CITCHARGE_TYPE
  #       ),
  #       popup = paste0(
  #         locationByTime()$UOF_FORCE_TYPE,
  #         " by ",
  #         stringr::str_to_title(locationByTime()$OFF_RACE),
  #         ", ",
  #         stringr::str_to_title(locationByTime()$OFF_SEX),
  #         " Officer (",
  #         locationByTime()$OFF_AGE,
  #         ") on ",
  #         str_to_title(locationByTime()$CIT_RACE),
  #         ", ",
  #         str_to_title(locationByTime()$CIT_SEX),
  #         " Citizen (",
  #         locationByTime()$CIT_AGE,
  #         "), ",
  #         locationByTime()$OCCURRED_YEAR,
  #         ". INCNUM: ",
  #         locationByTime()$INCNUM
  #       ),
  #       color = ~ palPaired_byYear(locationByTime()$OCCURRED_YEAR),
  #       opacity = 0.8
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = palPaired_byYear,
  #       values = locationByTime()$OCCURRED_YEAR,
  #       opacity = 1,
  #       # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
  #       title = sprintf(
  #         "Color by year. %s unique use of force occurances in hour %s from 2014-2019. ",
  #         count(locationByTime()),
  #         input$time_occured
  #       )
  #     )
  # })
  
  
  ### ### ### ### ### ### ### Database Tables ###
  
  output$UOF.df_data_table <- DT::renderDataTable(DT::datatable({
    UOF_csv[, input$UOF_variables, drop = FALSE]
  }))
  
  output$complaints.df_data_table <- renderDataTable(datatable({
    complaints_csv[, input$complaint_variables, drop = FALSE]
  }))
  
  output$UCR.df_data_table <- renderDataTable(datatable({
    UCR_csv[, input$UCR_variables, drop = FALSE]
  }))
}
