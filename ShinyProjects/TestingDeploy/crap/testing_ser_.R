# broken server, something about numbers being displayed in the mapping by race
# shiny of map
library(leaflet)
library(shiny)
library(tidyverse)
library(shinycssloaders) # for having a loading page to keep the audience entertained
library(ggrepel) # for some ggplot visual improvements


# This is necessary, check out the tabs for multiple graphs,
# could be something like a react SPA
# also look into reactive() for making the
# https://shiny.rstudio.com/articles/layout-guide.html
# this need be split into server and ui, should be done soon before this shit gets too large to deal with

# reading the data after read
UOF_csv <-
  read_csv("./UOF/extremely_simplified_force_type_and_reason_and_arrest.csv")


# colnames(UOF_csv)


# replacing missing lat, lon data with the downtown pd location
# -86.15646	39.76852
UOF.df <- UOF_csv %>%
  tidyr::replace_na(list(
    lon = -86.15646,
    lat = 39.76852,
    OFF_SEX = "Unreported",
    OFF_AGE = "Unreported"
  )) %>%
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  tidyr::replace_na(list(OCCURRED_YEAR = "Unreported")) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR))


#
# UCR_without_date.df <-
#   read_csv("./UCR/all_ucr_simplified_crime_categories_2014_2019_without_date.csv")
#

UCR_with_year_csv <-
  read_csv("./UCR/simplied_crime_categories_2014_2019.csv")


# View(tetsing)
UCR_with_year.df <- UCR_with_year_csv %>%
  select(YEAR, CRIME) %>%
  group_by(YEAR, CRIME) %>%
  filter(YEAR >= 2014) %>%
  count(CRIME) %>%
  rename("NUM_OF_OCCURANCES" = n)

# View(UCR_with_year.df)




#### colors and optons for maps, plots and spinners ####

#
# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# RColorBrewer::display.brewer.all()

palPaired_byCitzenRace <- colorFactor(palette = 'Set1',
                                      domain = UOF.df$CIT_RACE)


palPaired_byCitzenSex <-
  colorFactor(palette = c('#AE9C45', '#6073B1', '#052955'),
              domain = UOF.df$CIT_SEX)

options(
  spinner.color.background = "#F5F5F5",
  spinner.color = "#000000",
  spinner.type = 3
)





server <- function(input, output, session) {
  locationByYear_race <-
    eventReactive(input$userSelectedYearRace_leafletMap_UOF, {
      UOF.df %>%
        filter(OCCURRED_YEAR == input$userSelectedYearRace_leafletMap_UOF) %>%
        group_by(
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
      # distinct(unique_INCNUM())
    }, ignoreNULL = FALSE)
  
  
  locationByYear_sex <-
    eventReactive(input$userSelectedYearSex_leafletMap, {
      UOF.df %>%
        filter(OCCURRED_YEAR == input$userSelectedYearSex_leafletMap) %>%
        select(
          OCCURRED_YEAR,
          lat,
          lon,
          UOF_FORCE_TYPE,
          UOF_REASON,
          OFF_RACE,
          CIT_RACE,
          CITCHARGE_TYPE
        )
    }, ignoreNULL = FALSE)
  
  
  # REACTIVE CHECK BOX ON UNIQUE INCNUM
  
  # unique_INCNUM <- 
  # reactive({
  #   ifelse(input$userSelected_uniqueINCNUM_leafletMap_UOF == TRUE, INCUM, "")
  # })
  # 
  
  
  
  #### RENDERED OUTPUTS ####
  
  
  
  output$UOF.barchart_sex <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_withSex_ggBarChart) %>%
      ggplot(aes(x = CIT_SEX, fill = OFF_SEX)) +
      geom_bar() +
      theme_bw() +
      labs(
        y = "Number of reported",
        x = "Citizen Sex",
        title = paste0(
          "Findings graphed by Citizen Sex vs Officer Sex in ",
          input$userSelectedYear_withSex_ggBarChart
        )
      ) +
      guides(fill = guide_legend(title = "Fill by Officer Sex")) +
      theme(axis.text.x = element_text(angle = 45))
  })
  
  
  output$UOF.barchart_race <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_withRace_ggBarChart) %>%
      ggplot(aes(x = CIT_RACE, fill = OFF_RACE)) +
      geom_bar() +
      theme_bw() +
      labs(y = "Number of reported",
           x = "Citizen Race", 
           title = paste0("Findings graphed by Citizen Race vs Officer Race in ", input$userSelectedYear_withRace_ggBarChart)) +
      guides(fill = guide_legend(title = "Fill by Officer Race")) +
      theme(axis.text.x = element_text(angle = 45))
    
    
  })
  
  
  
  ## Leaflet maps ##
  
  
  output$UOF.map_race <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom =100, maxZoom = 400),
      leafletOptions(preferCanvas = TRUE) # to speed up the rendering
    )) %>%
      addTiles(options = tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)) %>%            # map won't load new tiles when panning))
      addCircleMarkers(
        data = locationByYear_race(),
        popup = paste0(UOF.df$UOF_REASON,
                       "; ",
                       UOF.df$CITCHARGE_TYPE),
        label = paste0(
          UOF.df$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_lower(UOF.df$OFF_RACE),
          " officer"
        ),
        color = ~ palPaired_byCitzenRace(UOF.df$CIT_RACE),
        opacity = 0.2
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palPaired_byCitzenRace,
        values = UOF.df$CIT_RACE,
        opacity = 1,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = paste0(
          count(locationByYear_race()),
          " UOF Occurances in ",
          input$userSelectedYearRace_leafletMap_UOF,
          ", citizen race by color", 
          "(darker the color, more occurances there)"
        )
      )
  })
  
  output$UOF.map_sex <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 0, maxZoom = 12),
      leafletOptions(preferCanvas = TRUE) # to speed up the rendering
    )) %>%
      addTiles(options = tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)) %>%            # map won't load new tiles when panning))
      addCircleMarkers(
        data = locationByYear_sex(),
        popup = paste0(UOF.df$UOF_REASON,
                       "; ",
                       UOF.df$CITCHARGE_TYPE),
        label = paste0(
          UOF.df$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_lower(UOF.df$OFF_SEX),
          " officer"
        ),
        color = ~ palPaired_byCitzenSex(UOF.df$CIT_SEX),
        opacity = 1.2
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palPaired_byCitzenSex,
        values = UOF.df$CIT_SEX,
        opacity = 1,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = paste0(
          count(locationByYear_sex()),
          " UOF Occurances in ",
          input$userSelectedYearSex_leafletMap,
          ", citizen sex by color"
        )
      )
  })
  
  
  ## UCR Data ##
  
  
  output$UCR.ggpoint <- renderPlot({
    UCR_with_year.df %>%
      select(YEAR, CRIME, NUM_OF_OCCURANCES) %>%
      filter(YEAR == input$userSelectedYear_ucr) %>%
      ggplot(aes(x = CRIME, y = NUM_OF_OCCURANCES)) +
      geom_point() +
      coord_flip() +
      ggrepel::geom_label_repel(aes(label = NUM_OF_OCCURANCES), label.size = 0.15) +
      labs(title = paste0("IMPD UCR for ", input$userSelectedYear_ucr)) +
      theme(axis.text.x = element_text(
        vjust = 0.75,
        angle = 45,
        lineheight = 1
      )) # +
    
  })
  
  
  output$UOF.facet <- renderPlot({
    UOF.df_facet %>%
      ggplot(aes(x = input$userSelected_x_facet_uof)) +
      theme_bw() +
      facet_wrap(. ~ UOF.df_facet$OCCURRED_YEAR) +
      geom_bar(aes(fill = input$userSelected_y_facet_uof)) +
      # ggrepel::geom_label_repel(aes(label = NUM_OF_OCCURANCES), label.size = 0.15) +
      labs(
        title = paste0(
          "IMPD UOF ",
          input$userSelected_x_facet_uof,
          " by ",
          input$userSelected_y_facet_uof
        )
      ) +
      theme(axis.text.x = element_text(
        vjust = 0.75,
        angle = 45,
        lineheight = 1
      ))
    
  })
  
  
  
  
  ### FACETS BY YEAR GGPLOT ###
  output$ggplot_facetted_by_year <- renderPlot({
    # palette(rainbow(12, s=08))
    
    x_data_point <-
      switch(
        input$userSelected_X_input_for_facets_ggbar,
        "Citizen Sex" = UOF.df$CIT_SEX,
        "Citizen Race" = UOF.df$CIT_RACE,
        "Citizen Age" = UOF.df$CIT_AGE
      )
    
    y_data_point <-
      switch(
        input$userSelected_fill_input_for_facets_ggbar,
        "Officer Sex" = UOF.df$OFF_SEX,
        "Officer Race" = UOF.df$OFF_RACE,
        "Officer Age" = UOF.df$OFF_AGE,
        "Citizen Charge Type" = UOF.df$CITCHARGE_TYPE
      )
    
    
    ggplot(UOF.df, aes(x = x_data_point,
                       fill = y_data_point)) +
      geom_bar() +
      theme_bw() +
      facet_wrap(~ UOF.df$OCCURRED_YEAR) +
      labs(
        title = paste0(
          "Findings graphed ",
          input$userSelected_X_input_for_facets_ggbar,
          " by ",
          input$userSelected_fill_input_for_facets_ggbar
        ),
        x = input$userSelected_X_input_for_facets_ggbar,
        y = input$userSelected_fill_input_for_facets_ggbar
      ) +
      guides(fill = guide_legend(
        title = paste0(
          "Fill by ",
          input$userSelected_fill_input_for_facets_ggbar
        )
      )) +
      theme(axis.text.x = element_text(angle = 90,
                                       size = 9))
  })
}
