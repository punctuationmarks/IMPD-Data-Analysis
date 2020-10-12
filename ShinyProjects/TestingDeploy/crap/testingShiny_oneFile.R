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
    read_csv("./UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv")
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

# replacing missing lat, lon data with the downtown pd location
# -86.15646	39.76852
UOF.df <- UOF_csv %>%
  tidyr::replace_na(list(lon = -86.15646, lat = 39.76852)) %>%
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR))



UOF.df_facet <- UOF.df %>%
  select(OCCURRED_YEAR, CIT_SEX, CIT_RACE, OFF_SEX, OFF_RACE)

# View(UOF.df_facet)
# View(UOF.df)


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





ui <- fluidPage(
  titlePanel(
    "Use of force by IMPD and Uniform Crime Report for Indianapolis 2014-2019 (current to July)"
  ),
  hr(),
  # leafletOutput("UOF.map"),
  navbarPage(
    title = "IMPD Data",
    position = "static-top",
    tabPanel(title = "Barchart by Sex",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYear_withSex_ggBarChart",
                   "Year incident occured:",
                   choices = list("2014", "2015", "2016", "2017",
                                  "2018", "2019", "Unreported")
                 ),
                 hr(),
                 helpText(
                   p(
                     "There were  63,413 incidents of use of force by the IMPD on the local population",
                     br(),
                     "This number does not mean there were 63,413 individual citizens, but 63,413 incidents.",
                     br(),
                     "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                   )
                 )
               )
               ,
               mainPanel(plotOutput("UOF.barchart_sex"))
             )),
    tabPanel(title = "Barchart by Race",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYear_withRace_ggBarChart",
                   "Year incident occured:",
                   choices = list("2014", "2015", "2016", "2017",
                                  "2018", "2019", "Unreported")
                 ),
                 hr(),
                 helpText(
                   p(
                     "There were  63,413 incidents of use of force by the IMPD on the local population",
                     br(),
                     "This number does not mean there were 63,413 individual citizens, but 63,413 incidents.",
                     br(),
                     "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                   )
                 )
               )
               ,
               mainPanel(plotOutput("UOF.barchart_race"))
             )),
    
    tabPanel(title = "Mapping by Race",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYearRace_leafletMap",
                   "Year incident occured:",
                   choices = list("2014", "2015", "2016", "2017",
                                  "2018", "2019", "Unreported")
                 ),
                 hr(),
                 helpText(
                   p(
                     "There were  63,413 incidents of use of force by the IMPD on the local population",
                     br(),
                     "This number does not mean there were 63,413 individual citizens, but 63,413 incidents.",
                     br(),
                     "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                   ),
                   p(
                     "Used ggmaps (Google maps api) to get the lat lon - this is actually requested even though I paid for the service"
                   )
                 )
               )
               ,
               mainPanel(
                 leafletOutput("UOF.map_race", width = "100%", height = 700) %>%
                   shinycssloaders::withSpinner()
               )
             )),
    tabPanel(title = "Mapping by Sex",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYearSex_leafletMap",
                   "Year incident occured:",
                   choices = list("2014", "2015", "2016", "2017",
                                  "2018", "2019", "Unreported")
                 ),
                 hr(),
                 helpText(
                   p(
                     "There were  63,413 incidents of use of force by the IMPD on the local population",
                     br(),
                     "This number does not mean there were 63,413 individual citizens, but 63,413 incidents.",
                     br(),
                     "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                   ),
                   p(
                     "Used ggmaps (Google maps api) to get the lat lon - this is actually requested even though I paid for the service"
                   )
                   
                 )
               )
               ,
               mainPanel(
                 leafletOutput("UOF.map_sex", width = "100%", height = 700) %>%
                   shinycssloaders::withSpinner()
               )
             )),
    
    tabPanel(title = "UCR by crime and year",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYear_ucr",
                   "Year incident occured:",
                   choices = list("2014", "2015", "2016", "2017",
                                  "2018", "2019")
                 ),
                 hr(),
                 helpText(
                   p(
                     "All IMPD UCR reported for 2014-2019",
                     br(),
                     "Simplified crime categories, for instance 'attempted' and 'committed' crime is combined.",
                     br(),
                     "Also 2019 data was taken up until July"
                   )
                 )
               )
               ,
               mainPanel(plotOutput("UCR.ggpoint"))
             )),
    
    
    tabPanel(title = "Testin'",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "userSelected_x_facet_uof",
                             "X: ",
                             choices = unique(as.character(
                               names(UOF.df_facet)
                             ))),
                 
                 hr(),
                 selectInput(inputId = "userSelected_y_facet_uof",
                             "Y: ",
                             choices = unique(as.character(
                               names(UOF.df_facet)
                             ))),
                 hr(),
                 
                 helpText(p("Bar charts faceted by year, with UOF data"))
               )
               ,
               mainPanel(plotOutput("UOF.facet"))
             )),
    
    tabPanel(title = "testing",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "testing_input",
                   "X:",
                   choices = list("YEAR", "QUARTER", "WEEK_DAY")
                 ),
                 hr(),
                 helpText("Testing... UCR data was taken up until July")
               )
               ,
               mainPanel(plotOutput("testing"))
             ))
  )
)














server <- function(input, output, session) {
  locationByYear_race <-
    eventReactive(input$userSelectedYearRace_leafletMap, {
      UOF.df %>%
        filter(OCCURRED_YEAR == input$userSelectedYearRace_leafletMap) %>%
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
  
  
# make it fuckin' reactive, since it is going through 200k+ too  
  # ucr_X <- reactive(input$UCR.facet, {
  #   input$userSelected_X_facetCharts
  # })
  #
  
  
  
  #### RENDERED OUTPUTS ####
  
  
  
  output$UOF.barchart_sex <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_withSex_ggBarChart) %>%
      ggplot(aes(x = OFF_SEX, fill = CIT_SEX)) +
      theme_bw() +
      geom_bar() +
      labs(y = "Number of reported",
           title = "Findings graphed by Citizen Sex vs Officer Sex") +
      theme(axis.text.x = element_text(angle = 45))
  })
  
  
  output$UOF.barchart_race <- renderPlot({
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_withRace_ggBarChart) %>%
      ggplot(aes(x = CIT_RACE, fill = OFF_RACE)) +
      theme_bw() +
      # so this looks really cool as a facet, but not sure this is good for shiny
      # facet_wrap( ~ UOF.df$OCCURRED_YEAR) +
      geom_bar() +
      labs(y = "Number of reported",
           title = "Findings graphed by Citizen Race vs Officer Race") +
      theme(axis.text.x = element_text(angle = 45))
    
  })
  
  
  
  ## Leaflet maps ##
  
  
  output$UOF.map_race <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 0, maxZoom = 12),
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
        opacity = 0.75
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
          input$userSelectedYearRace_leafletMap,
          ", citizen race by color"
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
  
  
  
  
  output$testing <- renderPlot({
    
  })
  #     UCR_with_year.df %>%
  #     SELECT(YEAR, QUARTER, WEEK_DAY, CRIME, NUM_OF_OCCURANCES) %>%
  #     filter(YEAR >= 2014) %>%
  #     ggplot(aes(x = input$))
  #
  #
  #   plot_crime_ucr_all_years <- NARROW_CRIME_ALL_UCR_ALL_YEARS %>%
  #     select(YEAR, CRIME) %>%
  #     group_by(YEAR, CRIME) %>%
  #     filter(YEAR >= 2014) %>%
  #     count(CRIME) %>%
  #     rename("NUM_OF_OCCURANCES" = n) %>%
  #     ggplot(aes(x = CRIME, y = NUM_OF_OCCURANCES)) +
  #     geom_point() +
  #     facet_wrap(. ~ YEAR) +
  #     coord_flip() +
  #     ggrepel::geom_label_repel(aes(label = NUM_OF_OCCURANCES), label.size = 0.15)+
  #     labs(title="All IMPD UCR reported for 2014-2019",
  #          subtitle = paste0("Simplified crime categories,\nfor instance attempted and committed crime is combined.
  #                        \nAlso 2019 data was taken up until July")) +
  #     theme(axis.text.x = element_text(vjust = 0.75, angle=45, lineheight = 1)) # +
  #
  #   plot_crime_ucr_all_years
  #
  #
  #
  #
  #
  # })
  
}

shinyApp(ui, server)
