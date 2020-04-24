library(shiny)
library(tidyverse) # mainly used for piping
library(leaflet) # maps
library(DT) # for table rendering/prettifying
library(shinycssloaders) # for having a loading page to keep the audience entertained
library(shinythemes) # for styling overall app


# reading the data after read
UOF_csv <-
  read_csv("./Datasets/UOF/extremely_simplified_force_type_and_reason_and_arrest.csv")

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
UOF.df <- UOF_csv
complaints.df <- complaints_csv
demographics_IMPD.df <- demographics_IMPD_csv
demographics_Indianapolis.df <- demographics_Indianapolis_csv


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




#### colors and optons for maps, plots and spinners ####
# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# RColorBrewer::display.brewer.all()

palPaired_byCitzenRace <- colorFactor(palette = 'Set3',
                                      domain = UOF.df$CIT_RACE)

palPaired_byCitzenSex <-
  colorFactor(palette = "Set1",
              domain = UOF.df$CIT_SEX)

palPaired_byYear <- colorFactor(palette = 'Set3',
                                domain = UOF.df$OCCURRED_YEAR)

# OPTION FOR SPINNERS WHILE LOADING GRAPHS/MAPS
options(
  spinner.color.background = "#F5F5F5",
  spinner.color = "#000000",
  spinner.type = 3
)




ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("IMPD UOF and UCR data "),
  hr(),
  # leafletOutput("UOF.map"),
  navbarPage(
    title = "IMPD Data",
    position = "static-top",
    theme = "bootstrap.css",
    
    
    
    ### ### ### ### ### ### UOF Charts ### ### ### ### ### ### ### ### ### ### ### ###
    navbarMenu(
      title = "Use of Force",
      tabPanel(title = "Barchart by Sex",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "userSelectedYear_withSex_ggBarChart",
                     "Year incident occured:",
                     choices = list("2014", "2015", "2016", "2017",
                                    "2018", "2019", "Unreported"),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(
                     p(
                       "There were  63,413 incidents of use of force by the IMPD on the local population",
                       br(),
                       "This number does not mean there were 63,413 individual citizens, but 63,413 incidents.",
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
                                    "2018", "2019", "Unreported"),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(
                     p(
                       "There were  63,413 incidents of use of force by the IMPD on the local population",
                       br(),
                       "This number does not mean there were 63,413 individual citizens, but 63,413 incidents.",
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
    
    
    ### ### ### ### ### ### ### MAPS ### ### ### ### ### ### ###
    navbarMenu(
      title = "Mapping UOF",
      tabPanel(title = "Mapping by Race",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "userSelectedYearRace_leafletMap_UOF",
                     "Year incident occured:",
                     choices = list("2014", "2015", "2016", "2017",
                                    "2018", "2019", "Unreported"),
                     selected = "2018"
                     
                   ),
                   hr(),
                   helpText(
                     p(
                       "This is only displaying the unqiue INCNUM's GPS location.
                       This is due to the common occurance of having multiple UOF occurances during each unqiue incidents numbers.",
                       tags$em(
                         "(e.g.INCUM 62402 has two accounts of UOF, physical and handcuffing)"
                       )
                     ),
                     p(
                       "There were  63,413 incidents of use of force by the IMPD on the local population
                       This number does not mean there were 63,413 individual citizens, but 63,413 incidents.
                       Each 'indcident' means one single use of force from one officer to one citizen
                       60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     ),
                     
                     p(
                       "Used ggmaps (Google maps api) to get the lat & lon from cleaned address"
                     )
                   ),
                   width = 2
                 ),
                 mainPanel(
                   leafletOutput("UOF.map_race", width = "100%", height = 700) %>%
                     shinycssloaders::withSpinner(),
                   width = 10
                 )
               )),
      
      tabPanel(title = "Mapping by Sex",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "userSelectedYearSex_leafletMap",
                     "Year incident occured:",
                     choices = list("2014", "2015", "2016", "2017",
                                    "2018", "2019", "Unreported"),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(
                     p(
                       "This is only displaying the unqiue INCNUM's GPS location.
                       This is due to the common occurance of having multiple UOF occurances during each unqiue incidents numbers.",
                       tags$em(
                         "(e.g.INCUM 62402 has two accounts of UOF, physical and handcuffing)"
                       )
                     ),
                     p(
                       "There were  63,413 incidents of use of force by the IMPD on the local population
                       This number does not mean there were 63,413 individual citizens, but 63,413 incidents.
                       Each 'indcident' means one single use of force from one officer to one citizen
                       60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     ),
                     
                     p(
                       "Used ggmaps (Google maps api) to get the lat & lon from cleaned address"
                     )
                   ),
                   width = 2
                 ),
                 mainPanel(
                   leafletOutput("UOF.map_sex", width = "100%", height = 700) %>%
                     shinycssloaders::withSpinner(),
                   width = 10
                 )
               )),
      
      tabPanel(title = "Mapping by Quarter",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "userSelectedQuarter_leafletMap",
                     "Quarter incident occured:",
                     choices = list(
                       "QT1" = 1,
                       "QT2" = 2,
                       "QT3" = 3,
                       "QT4" = 4,
                       "Unreported" = "Unreported"
                     ),
                     # choices = list(1, 2, 3, 4, "Unreported"),
                     selected = 2
                   ),
                   hr(),
                   helpText(
                     p(
                       "This is only displaying the unqiue INCNUM's GPS location.
                       This is due to the common occurance of having multiple UOF occurances during each unqiue incidents numbers.",
                       tags$em(
                         "(e.g.INCUM 62402 has two accounts of UOF, physical and handcuffing)"
                       )
                     ),
                     p(
                       "There were  63,413 incidents of use of force by the IMPD on the local population
                       This number does not mean there were 63,413 individual citizens, but 63,413 incidents.
                       Each 'indcident' means one single use of force from one officer to one citizen
                       60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     ),
                     
                     p(
                       "Used ggmaps (Google maps api) to get the lat & lon from cleaned address"
                     )
                   ),
                   width = 2
                 ),
                 mainPanel(
                   leafletOutput("UOF.map_quarter", width = "100%", height = 700) %>%
                     shinycssloaders::withSpinner(),
                   width = 10
                 )
               )),
      
      tabPanel(title = "Mapping by time of day",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "userSelectedTimeOfDay_leafletMap",
                     "Hour the incident occured (24-hour):",
                     choices = list(
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
                     selected = 4
                   ),
                   hr(),
                   helpText(
                     p(
                       "This is only displaying the unqiue INCNUM's GPS location.
                       This is due to the common occurance of having multiple UOF occurances during each unqiue incidents numbers.",
                       tags$em(
                         "(e.g.INCUM 62402 has two accounts of UOF, physical and handcuffing)"
                       )
                     ),
                     p(
                       "There were  63,413 incidents of use of force by the IMPD on the local population
                       This number does not mean there were 63,413 individual citizens, but 63,413 incidents.
                       Each 'indcident' means one single use of force from one officer to one citizen
                       60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     ),
                     
                     p(
                       "Used ggmaps (Google maps api) to get the lat & lon from cleaned address"
                     )
                   ),
                   width = 2
                 ),
                 mainPanel(
                   leafletOutput("UOF.map_time_of_day", width = "100%", height = 700) %>%
                     shinycssloaders::withSpinner(),
                   width = 10
                 )
               ))
    ),
    
    tabPanel(title = "Uniformed Crime Report",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "userSelectedYear_UCR",
                   "Year incident occured:",
                   choices = list("2014", "2015", "2016", "2017",
                                  "2018", "2019"),
                   selected = "2018"
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
                 ),
                 width = 2
               )
               ,
               mainPanel(plotOutput("UCR.ggpoint"), width = 10)
             )),
    
    ### ### ### ### ### ### ### MAPS ### ### ### ### ### ### ###
    navbarMenu(
      title = "Complaints [Coming Soon]",
      tabPanel(title = "Graph 1",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "complaint_year_9999",
                     "Year incident occured:",
                     choices = list(
                       "2014",
                       "2015",
                       "2016",
                       "2017",
                       "2018",
                       "2019",
                       "All Years",
                       "Unreported"
                     ),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(
                     p(
                       tags$b("Overview of total data from 2014-2019:"),
                       br(),
                       "1 officer was 'Coached/Mentored',
    1 officer vacated their position,
    1 finding was 'Not Justified',
    330 findings were 'Unfounded',
    345 findings were 'Sustained',
    1028 findings were 'Not Sustained',
    1556 findings were 'Exonerated',
    and 74 investigation results were 'Unreported'."
                     )
                   ),
                   width = 2
                 )
                 ,
                 mainPanel(plotOutput("Complains_Allegation.geom_point"), width = 10)
               )),
      tabPanel(title = "Graph 2",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "Y",
                     "Year incident occured:",
                     choices = list("2014", "2015", "2016", "2017",
                                    "2018", "2019", "Unreported"),
                     selected = "2018"
                   ),
                   hr(),
                   helpText(p("INFO ABOUT COMPLAINTS")),
                   width = 2
                 )
                 ,
                 mainPanel(plotOutput("SEX"), width = 10)
               ))
    ),
    
    tabPanel(title = "Outliers [coming soon]",
             includeMarkdown("About/AboutTheOutliers.md")),
    
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
    
    tabPanel(title = "Demographics",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "demographic_dataset",
                   label = "Demographic Dataset:",
                   choices = c(
                     "IMPD Racial Demographics 2013",
                     "Indianapolis 2010 Census Demographics",
                     "Estiamted Indianapolis 2010 Demographics",
                     "Estiamted Indianapolis 2018 Demographics"
                   ),
                   selected = "IMPD 2013"
                 )
               ),
               mainPanel(plotOutput("demographics.graph"), width = 10)
             )),
    
    tabPanel(title = "About",
             includeMarkdown("About/AboutTheData.md"))
  )
)
