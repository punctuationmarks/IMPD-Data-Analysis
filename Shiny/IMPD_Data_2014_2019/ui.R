# install.packages(c("shiny", "tidyverse", "leaflet", "DT", "shinycssloaders", "shinythemes"))
library(shiny)
library(tidyverse) # mainly used for piping
library(leaflet) # maps
library(DT) # for table rendering/prettifying
library(shinycssloaders) # for having a loading page to keep the audience entertained
library(shinythemes) # for styling overall app
 
UOF.df <- read_csv("./Datasets/UOF/UOF_all___with_lat_lon_and_zip_up_to_dec_2019.csv")


# unique zipcodes in the data, for the input choices
zipCodes <- UOF.df %>% filter(zip < 60000 & zip > 40000) %>%  distinct(zip)



UCR.df <-
  read_csv("Datasets/UCR/simplied_crime_categories_2014_2019.csv")

complaints.df <-
  read_csv("Datasets/Complaints/cleanedComplaint_data.csv")




#### ORGANIZING DATA FOR PROCESSING, BUT THIS SHOULD BE DONE ELSEWHERE, BEFORE EVEN IMPORTING ###
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






#### colors and optons for maps, plots and spinners ####

# OPTION FOR SPINNERS WHILE LOADING GRAPHS/MAPS
options(
  spinner.color.background = "#F5F5F5",
  spinner.color = "#000000",
  spinner.type = 3
)




ui <- fluidPage(
  theme = shinytheme("flatly"),
  hr(),
  navbarPage(
    title = "IMPD UOF and UCR data 2014-2019",
    # position = "static-top",
    theme = "bootstrap.css",
    ### OUF Maps ###
    ### ### ### ### ### ### ### MAPS ### ### ### ### ### ### ###
      tabPanel(title = "Mapping UOF",
               sidebarLayout(
                 sidebarPanel(
                   h3("Related to citizen"),
                   selectInput(
                     inputId = "citizen_sex",
                     "By Citizen Sex", 
                     choices = list("All", "FEMALE", "MALE", "UNREPORTED"),
                     selected = "All"
                   ),
                   h3("Related to officer"),
                   selectInput(
                     inputId = "officer_sex",
                     "By Officer Sex",
                     choices = list("All", "FEMALE", "MALE", "UNREPORTED"),
                     selected = "All"
                   ),
                   h3("Related to time"),
                   selectInput(
                     inputId = "year_occured",
                     "Year incident occured:",
                     choices = list("All years (2014-2019)" = "All", 2014 ,  2015 ,  2016 ,  2017 ,
                                     2018 ,  2019 , "UNREPORTED"),
                     selected =  2018 
                   ),
                   selectInput(
                     inputId = "quarter_occured",
                     "Quarter incident occured:",
                     choices = list(
                       "All" = "All",
                       "QT1" = 1,
                       "QT2" = 2,
                       "QT3" = 3,
                       "QT4" = 4,
                       "UNREPORTED" = "UNREPORTED"
                     ),
                     # choices = list(1, 2, 3, 4, "UNREPORTED"),
                     selected = "All"
                   ),
                   selectInput(
                     inputId = "time_occured",
                     "Hour the incident occured (24-hour):",
                     choices = list(
                       "All day" = "All",
                       '00:00' = 0,
                       '01:00' = 1,
                       '02:00' = 2,
                       '03:00' = 3,
                       '04:00' = 4,
                       '05:00' = 5,
                       '06:00' = 6,
                       '07:00' = 7,
                       '08:00' = 8,
                       '10:00' = 10,
                       '11:00' = 11,
                       '12:00' = 12,
                       '13:00' = 13,
                       '14:00' = 14,
                       '15:00' = 15,
                       '16:00' = 16,
                       '17:00' = 17,
                       '18:00' = 18,
                       '19:00' = 19,
                       '20:00' = 20,
                       '21:00' = 21,
                       '22:00' = 22,
                       '23:00' = 23,
                       '24:00' = 24
                     ),
                     selected = "All"
                   ),
                   
                   
                   
                   # concept is to have a boolean checkbox that triggers the color changes based on zip code
                   # also have one that's based on race, sex ? couple radio buttons, that way it'll be a switch statement between color themes?
                   # checkboxInput(
                   #   inputId = "is_colored_by_zip",
                   #   "Color by zip code",
                   #   choices = list(TRUE, FALSE),
                   #   select = FALSE
                   # ),
                   
                   selectInput(
                     inputId = "occured_in_zip",
                     "By zip code",
                     choices = c("All", zipCodes),
                     selected = "All"
                   ),
                   
                   hr(),
                   helpText(
                     tags$small(
                       "This is only displaying the unqiue INCNUM's GPS location.
                        Making this explicit due to the common occurance of having multiple UOF occurances during each unqiue incidents numbers.",
                       tags$em(
                         "(e.g.INCUM 62402 has two accounts of UOF, physical and handcuffing)"
                       )
                     ),
                     br(),
                     tags$small(
                       "Note, there were  73,083 incidents of use of force by the IMPD between 2014 and 2019 on the local population. 
                       This number does not mean there were 73,083 individual citizens, but 73,083 incidents.
                       Each 'incident' means one single use of force from one officer to one citizen
                       60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     )
                   ),
                   width = 2
                 ),
                 mainPanel(
                   leafletOutput("UOF.map", width = "100%", height = 700) %>%
                     shinycssloaders::withSpinner(),
                   width = 10

               ))),

    navbarMenu(
      title = "Use of Force",
      tabPanel(title = "Barchart",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "year_occured_barchart",
                     "Year incident occured:",
                     choices = list("2014", "2015", "2016", "2017",
                                    "2018", "2019", "UNREPORTED"),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(
                     p(
                       "There were  73,083 inciednts of use of force by the IMPD on the local population",
                       br(),
                       "This number does not mean there were 73,083 individual citizens, but 73,083 incidents.",
                       br(),
                       "Each 'indcident' means one single use of force from one officer to one citizen",
                       br(),
                       "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     )
                   ),
                   width = 2
                 )
                 ,
                 mainPanel(plotOutput("UOF.barchart_sex"), width = 10)
               )),
      tabPanel(title = "Barchart by Race",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "userSelectedYear_withRace_ggBarChart",
                     "Year incident occured:",
                     choices = list("2014", "2015", "2016", "2017",
                                    "2018", "2019", "UNREPORTED"),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(
                     p(
                       "There were  73,083 incidents of use of force by the IMPD on the local population",
                       br(),
                       "This number does not mean there were 73,083 individual citizens, but 73,083 incidents.",
                       br(),
                       "Each 'indcident' means one single use of force from one officer to one citizen",
                       br(),
                       "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     )
                   ),
                   width = 2
                 )
                 ,
                 mainPanel(plotOutput("UOF.barchart_race"), width = 10)
               )),
      tabPanel(title = "Data Facetted by Year",
               sidebarLayout(
                 sidebarPanel(
                   helpText(
                     "Choose which bar chart to display, showing years 2014-2019 (up to July) for UOF trends"
                   ),
                   selectInput(
                     inputId = "userSelected_X_input_for_facets_ggbar",
                     label = "Choose an X axis",
                     choices = c("Citizen Sex", "Citizen Race", "Citizen Age"),
                     selected = "Citizen Sex"
                   ),
                   selectInput(
                     inputId = "userSelected_fill_input_for_facets_ggbar",
                     label = "Choose a fill",
                     choices = c(
                       "Officer Sex",
                       "Officer Race",
                       "Officer Age",
                       "Citizen Charge Type"
                     ),
                     selected = "Officer Sex"
                   ),
                   width = 2
                 ),
                 mainPanel(plotOutput("ggplot_facetted_by_year"), width = 10)
                 
               ))
    ),
    navbarMenu(
      title = "Database Tables",
      tabPanel(title = "UOF",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(
                     "UOF_variables",
                     "Columns in Use Of Force dataframe to show:",
                     names(UOF.df),
                     selected = names(UOF.df)
                   ),
                   width = 2
                 ),
                 
                 mainPanel(DT::dataTableOutput("UOF.df_data_table"), width = 10)
               )),
      tabPanel(title = "UCR",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(
                     "UCR_variables",
                     "Columns in Uniform Crime Report dataframe to show:",
                     names(UCR.df),
                     selected = names(UCR.df)
                   ),
                   width = 2
                 ),
                 
                 mainPanel(DT::dataTableOutput("UCR.df_data_table"), width = 10)
               )),
      tabPanel(title = "Complaints",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(
                     "complaint_variables",
                     "Columns in Complaints dataframe to show:",
                     names(complaints.df),
                     selected = names(complaints.df)
                   ),
                   width = 2
                 ),
                 
                 mainPanel(DT::dataTableOutput("complaints.df_data_table"), width = 10)
               ))
    ),
    tabPanel(
      id = "about_page",
      title = "About",
      includeMarkdown("About/AboutTheData.md")
    )
  )
)
