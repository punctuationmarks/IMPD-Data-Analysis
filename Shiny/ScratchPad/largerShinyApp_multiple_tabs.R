# shiny of map
library(leaflet)
library(shiny)
library(tidyverse)
library(shinycssloaders) # for having a loading page to keep the audience entertained
library(ggrepel)
# This is necessary, check out the tabs for multiple graphs,
# could be something like a react SPA
# also look into reactive() for making the
# https://shiny.rstudio.com/articles/layout-guide.html


# reading the data after read
UOF.df <-
  read_csv("../../CleanData/UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv")

UCR_without_date.df <-
  read_csv(
    "../../CleanData/UCR/all_ucr_simplified_crime_categories_2014_2019_without_date.csv"
  )

UCR_with_year.df <-
  read_csv("../../CleanData/UCR/simplied_crime_categories_2014_2019.csv")

UCR_with_year.df <- UCR_with_year.df %>%
  select(DATE_, CRIME) %>%
  group_by(DATE_, CRIME) %>%
  filter(DATE_ >= 2014) %>%
  count(CRIME) %>%
  rename("NUM_OF_OCCURANCES" = n)

# View(UCR_with_year.df)

# replacing missing lat, lon data with the downtown pd location
# -86.15646	39.76852
UOF.df <- UOF.df %>%
  tidyr::replace_na(list(lon = -86.15646, lat = 39.76852)) %>%
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR))


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


rsconnect::setAccountInfo(name='punctuationmarks', token='87A08FB30979B61E596642C1F3690CBE', secret='9juk+ITllTnJY9lKNgNavORNxRoEuK2v3qrbOmb+')



ui <- fluidPage(
  titlePanel("Use of Force for use by IMPD 2014-2019 (current to July)"),
  hr(),
  # leafletOutput("UOF.map"),
  navbarPage(
    title = "IMPD Data",
    position = "static-top",
    tabPanel(title = "Mapping by Race",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYear_race",
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
               mainPanel(
                 leafletOutput("UOF.map_race", width = "100%", height = 700) %>%
                   shinycssloaders::withSpinner()
               )
             )),
    tabPanel(title = "Mapping by Sex",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYear_sex",
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
               mainPanel(
                 leafletOutput("UOF.map_sex", width = "100%", height = 700) %>%
                   shinycssloaders::withSpinner()
               )
             )),
    tabPanel("BAR CHART",
             # this needs to be looked into, thoughts would be to make a prototype and then combine
             sidebarLayout(sidebarPanel(
               # selectInput(
               #   inputId = "USER_SELECTED_YEAR",
               #   "YEAR",
               #   choices = list(2014, 2015, 2016, 2017, 2018)
               # ),
               mainPanel(plotOutput("BAR_CHART_OUTPUT") %>% withSpinner())
             ))),
    tabPanel("Component 2"),
    tabPanel("Component 3")
  )
)







server <- function(input, output, session) {
  locationByYear_race <- eventReactive(input$userSelectedYear_race, {
    UOF.df %>%
      filter(OCCURRED_YEAR == input$userSelectedYear_race) %>%
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
    eventReactive(input$userSelectedYear_sex, {
      UOF.df %>%
        filter(OCCURRED_YEAR == input$userSelectedYear_sex) %>%
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
  
  
  
  # ucrByYear <- reactive(input$USER_SELECTED_YEAR, {
  #   UCR_with_year.df$DATE_ == input$USER_SELECTED_YEAR
  # })
  #
  
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
          input$userSelectedYear_race,
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
          input$userSelectedYear_sex,
          ", citizen sex by color"
        )
      )
  })
  
  
  # output$BAR_CHART_OUTPUT <- renderPlot({
  #   UCR_with_year.df %>%
  #     filter(DATE_ == input$USER_SELECTED_YEAR) %>%
  #     ggplot(aes(x = CRIME, y = NUM_OF_OCCURANCES)) +
  #     geom_point() +
  #     coord_flip() +
  #     ggrepel::geom_label_repel(aes(label = NUM_OF_OCCURANCES), label.size = 0.15) +
  #     labs(
  #       title = "All IMPD UCR reported for 2014-2019",
  #       subtitle = paste0(
  #         "Simplified crime categories,\nfor instance attempted and committed crime is combined.
  #                        \nAlso 2019 data was taken up until July"
  #       )
  #     ) +
  #     theme(axis.text.x = element_text(
  #       vjust = 0.75,
  #       angle = 45,
  #       lineheight = 1
  #     )) 
  # })
}
shinyApp(ui, server)
