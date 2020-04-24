# shiny of map
library(leaflet)
library(shiny)
library(tidyverse)

## so the map with the race by year works, 
# could combine the two concepts (have a race by checkbox, but
# with 75k+ data points, might be unreasonable
# BUUUUUTT would be a challenge, use reactive and as many optimizers as possible
# this would eb smething that a large scale data company would
# provide, you should do better, seriously)

# This is necessary, check out the tabs for multiple graphs,
# could be something like a react SPA
# also look into reactive() for making the
# https://shiny.rstudio.com/articles/layout-guide.html


# reading the data after read
UOF.df <-
  read_csv("../../CleanData/UOF/cleanedUOF_withGeoLocation_andFormattedDate.csv")


# replacing missing lat, lon data with the downtown pd location
# -86.15646	39.76852
UOF.df <- UOF.df %>%
  tidyr::replace_na(list(lon = -86.15646, lat = 39.76852)) %>%
  unite("latLon", c(lon, lat), sep = ", ", remove = F) %>%
  mutate(OCCURRED_YEAR = lubridate::year(YMD_TM)) %>%
  mutate(OCCURRED_YEAR = ifelse(
    OCCURRED_YEAR == 2000 |
      is.na(OCCURRED_YEAR),
    "Unreported",
    OCCURRED_YEAR
  ))


# View(UOF.df)

testing <- UOF.df %>%
  filter(CIT_ARRESTED == "Yes")
# View(testing)

# filter by year (NEED TO TEST ON OPTIMIZATION)
UOF_2019.df <- UOF.df %>%
  filter(OCCURRED_YEAR == 2019)

UOF_2018.df <- UOF.df %>%
  filter(OCCURRED_YEAR == 2018)

UOF_2017.df <- UOF.df %>%
  filter(OCCURRED_YEAR == 2017)

UOF_2016.df <- UOF.df %>%
  filter(OCCURRED_YEAR == 2016)

UOF_2015.df <- UOF.df %>%
  filter(OCCURRED_YEAR == 2015)

UOF_2014.df <- UOF.df %>%
  filter(OCCURRED_YEAR == 2014)


# filter by seasons

quarterOne <- UOF.df %>%
  filter(OCCURRED_QUARTER == 1)

quarterTwo <- UOF.df %>%
  filter(OCCURRED_QUARTER == 2)

quarterThree <- UOF.df %>%
  filter(OCCURRED_QUARTER == 3)

quarterFour <- UOF.df %>%
  filter(OCCURRED_QUARTER == 4)





testing <- UOF.df %>%
  filter(CIT_RACE == c("White", "Black", "Asian"))

testing2 <- UOF.df %>%
  filter(CIT_RACE == "Asian") %>%
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


# View(testing)
# View(testing2)

ui <- fluidPage(
  titlePanel(
    "Use of Force for use by IMPD 2014-2019 (current to July), displayed by race"
  ),
  hr(),
  # leafletOutput("UOF.map.grouped_by_year"),
  
  sidebarLayout(
    sidebarPanel(
      # selectInput(
      #   inputId = "userSelectedYear",
      #   "Year incident occured:",
      #   choices = list("2014", "2015", "2016", "2017",
      #                  "2018", "2019", "Unreported")
      # ),
      selectInput(
        inputId = "USER_SELECTED_RACE",
        "USER_SELECTED_RACE",
        choices = c(
          "Asian",
          "Bi-racial",
          "Black",
          "Hispanic",
          "Native American",
          "Polynesian",
          "White",
          "Other",
          "Unreported"
        )
        #   All = c(
        #     # cool, is this might work theorically due to the array (list) in a list works for some reason
        #     # during the filter, see in testing2 (refactor this comment for future thought)
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
    # ,
    # mainPanel(leafletOutput(
    #   "UOF.map.grouped_by_year", width = "100%", height = 700
    # ))
    ,
    mainPanel(
      leafletOutput(
        "UOF.map.grouped_by_race",
        width = "100%",
        height = 700
      )
    )
  )
)

#
# palet_by_citizen_race <- colorFactor(palette = 'Paired',
#                                       domain = UOF.df$CIT_RACE)



palet_by_occurred_year <- colorFactor(
  palette = c(
    '#E658FF',
    '#887F47',
    '#9C9EB5',
    '#FAF2EA',
    '#FFF458',
    "#71FF58",
    "#FF5871"
  ),
  domain = UOF.df$OCCURRED_YEAR
)

server <- function(input, output, session) {
  # got to think of better way for reactive since it could be multiple
  REACTIVE_BY_RACE_OVER_THE_YEARS <- reactive({
    UOF.df %>%
      filter(CIT_RACE == input$USER_SELECTED_RACE) %>%
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
    
  })
  
  #
  # REACTIVE_BY_THE_YEAR <- eventReactive(input$userSelectedYear, {
  #   UOF.df %>%
  #     filter(OCCURRED_YEAR == input$userSelectedYear) %>%
  #     select(
  #       OCCURRED_YEAR,
  #       lat,
  #       lon,
  #       UOF_FORCE_TYPE,
  #       UOF_REASON,
  #       OFF_RACE,
  #       CIT_RACE,
  #       CITCHARGE_TYPE
  #     )
  # }, ignoreNULL = FALSE)
  #
  #
  # output$UOF.map.grouped_by_year <- renderLeaflet({
  #   leaflet(options = c(
  #     leafletOptions(minZoom = 0, maxZoom = 12),
  #     leafletOptions(preferCanvas = TRUE) # to speed up the rendering
  #   )) %>%
  #     addTiles(options = tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
  #                                    updateWhenIdle = TRUE)) %>%            # map won't load new tiles when panning))
  #     addCircleMarkers(
  #       data = REACTIVE_BY_THE_YEAR(),
  #       popup = paste0(UOF.df$UOF_REASON,
  #                      "; ",
  #                      UOF.df$CITCHARGE_TYPE),
  #       label = paste0(
  #         UOF.df$UOF_FORCE_TYPE,
  #         " by ",
  #         stringr::str_to_lower(UOF.df$OFF_RACE),
  #         " officer"
  #       ),
  #       color = ~ palet_by_citizen_race(UOF.df$CIT_RACE),
  #       opacity = 0.9
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       pal = palet_by_citizen_race,
  #       values = UOF.df$CIT_RACE,
  #       opacity = 0.75,
  #       # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
  #       title = paste0(
  #         count(REACTIVE_BY_THE_YEAR()),
  #         " UOF Occurances in ",
  #         input$userSelectedYear,
  #         ", citizen race by color"
  #       )
  #     )
  # })
  #
  #
  
  output$UOF.map.grouped_by_race <- renderLeaflet({
    leaflet(options = c(
      leafletOptions(minZoom = 0, maxZoom = 12),
      leafletOptions(preferCanvas = TRUE) # to speed up the rendering
    )) %>%
      addTiles(options = tileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                     updateWhenIdle = TRUE)) %>%            # map won't load new tiles when panning))
      addCircleMarkers(
        data = REACTIVE_BY_RACE_OVER_THE_YEARS(),
        popup = paste0(UOF.df$UOF_REASON,
                       "; ",
                       UOF.df$CITCHARGE_TYPE),
        label = paste0(
          UOF.df$UOF_FORCE_TYPE,
          " by ",
          stringr::str_to_lower(UOF.df$OFF_RACE),
          " officer. In ",
          UOF.df$OCCURRED_YEAR
        ),
        color = ~ palet_by_occurred_year(UOF.df$OCCURRED_YEAR),
        opacity = 0.9
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palet_by_occurred_year,
        values = UOF.df$OCCURRED_YEAR,
        opacity = 0.75,
        # note, when changing the opacity and using a color pallete for information the color pallete also will have the alpha change
        title = paste0(
          count(REACTIVE_BY_RACE_OVER_THE_YEARS()),
          " UOF Occurances to ",
          input$USER_SELECTED_RACE,
          " humans. Color by year."
        )
      )
  })
}

shinyApp(ui, server)
