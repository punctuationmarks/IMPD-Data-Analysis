# shiny of map
library(shiny)
library(tidyverse)
library(shinycssloaders) # for having a loading page to keep the audience entertained

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
  mutate(OCCURRED_YEAR = ifelse(OCCURRED_YEAR == 2000, "Unreported", OCCURRED_YEAR))


# View(UOF.df)



ui <- fluidPage(
  mainPanel(plotOutput(outputId = "BAR_CHART")
  )
)




server <- function(input, output, session) {}





shinyApp(ui, server)
