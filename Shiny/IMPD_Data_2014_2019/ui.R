
# All imports and globally used variables are in global.R ---------------------------------------------






ui <- fluidPage(
  theme = overallTheme, # adjust this in globals
  navbarPage(
    title = "IMPD UOF and UCR data 2014-2019",
    position = "static-top",
    
# Maps --------------------------------------------------------------------
# Use of Force ------------------------------------------------------------
    tabPanel(title = "Mapping UOF",
             # right hand layout for easier use on mobile and tablets
             sidebarLayout(
               mainPanel(
                 leafletOutput("UOF.map", width = "100%", height = 900) %>%
                   shinycssloaders::withSpinner(),
                 width = 10
               ),
               sidebarPanel(
                 h3("Display the incidents of IMPD's use of force based on the selected criteria below:"),
                 h4("Related to citizen"),
                 selectInput(
                   inputId = "citizen_race",
                   "By citizen race",
                   choices = inputCitizenRaces,
                   selected = "All"
                 ),
                 selectInput(
                   inputId = "citizen_sex",
                   "By Citizen Sex",
                   choices = inputSexes,
                   selected = "All"
                 ),
                 h4("Related to officer"),
                 selectInput(
                   inputId = "officer_race",
                   "By officer race",
                   choices = inputOfficerRaces,
                   selected = "All"
                 ),
                 selectInput(
                   inputId = "officer_sex",
                   "By Officer Sex",
                   choices = inputSexes,
                   selected = "All"
                 ),
                 h4("Related to time"),
                 selectInput(
                   inputId = "year_occured",
                   "Year incident occured:",
                   choices = inputYears,
                   selected =  2018
                 ),
                 selectInput(
                   inputId = "quarter_occured",
                   "Quarter incident occured:",
                   choices = inputQuarters,
                   selected = "All"
                 ),
                 selectInput(
                   inputId = "time_occured",
                   "Hour the incident occured (24-hour):",
                   choices = inputHours,
                   selected = "All"
                 ),
                 selectInput(
                   inputId = "occured_in_zip",
                   "By zip code",
                   choices = inputZipCodes,
                   selected = "All"
                 ),
                 # TODO:
                 # concept is to have a boolean checkbox that triggers the color changes based on zip code
                 # also have one that's based on race, sex ? couple radio buttons, that way it'll be a switch statement between color themes?
                 # checkboxInput(
                 #   inputId = "is_colored_by_zip",
                 #   "Color by zip code",
                 #   choices = list(TRUE, FALSE),
                 #   select = FALSE
                 # ),
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
             )),
    

# Graphs ------------------------------------------------------------------


# Barcharts ---------------------------------------------------------------
    navbarMenu(
      title = "Use of Force",
      tabPanel(title = "Barchart",
               sidebarLayout(
                 mainPanel(plotOutput("UOF.barchart"), width = 10),
                 sidebarPanel(
                   # this is not working obviously
                   # but the concept is sound
                   selectInput(
                     inputId = "year_occured_barchart",
                     "Year incident occured:",
                     choices = inputYears,
                     selected = inputYears[4]
                   ),
                   selectInput(
                     inputId = "bar_x_axis",
                     "X axis:",
                     choices = inputGraphAxis,
                     selected = inputGraphAxis[3]
                   ),
                   selectInput(
                     inputId = "bar_fill",
                     "Fill by:",
                     choices = inputGraphAxis,
                     selected = inputGraphAxis[4]
                   ),
                   hr(),
                   helpText(
                     p(
                       "Select a focus by what is represented by a bucket/bar and then select what to fill that bucket/bar"
                     )
                   ),
                   width = 2
                 )
                 
               )),
      tabPanel(title = "Barchart by sex",
               sidebarLayout(
                 mainPanel(plotOutput("UOF.barchart_sex"), width = 10),
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
                       "Each 'incident' means one single use of force from one officer to one citizen",
                       br(),
                       "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     )
                   ),
                   width = 2
                 )
                 
               )),
      tabPanel(title = "Barchart by Race",
               sidebarLayout(
                 mainPanel(plotOutput("UOF.barchart_race"), width = 10),
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
                       "Each 'incident' means one single use of force from one officer to one citizen",
                       br(),
                       "60,613 incidents resulted in arrest, 2,794 incidents did not result in an arrest."
                     )
                   ),
                   width = 2
                 )
               )),
      tabPanel(title = "Data Facetted by Year",
               sidebarLayout(
                 mainPanel(plotOutput("ggplot_facetted_by_year"), width = 10),
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
                 )
                 
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
