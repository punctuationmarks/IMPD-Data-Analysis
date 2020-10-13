# testing shiny with ggplot facets over year

library(shiny)
library(tidyverse)
# library(scales) # in base package, only explicit for clarity

testingFile <-
  read_csv(
    "../../CleanData/UOF/extremely_simplified_force_type_and_reason_and_arrest.csv"
  )

# View(testingFile)


findingPercentages <- testingFile %>% 
  count(OFF_SEX) %>%
  rename(NUMBER_OF_OBSERSATIONS = n) %>% 
  mutate(PERCENTAGE = ((NUMBER_OF_OBSERSATIONS/63413) * 100))


# View(findingPercentages)


BULL_SHIT <- testingFile %>% 
  group_by(OCCURRED_YEAR, 
           OFF_SEX,
           CIT_SEX) %>% 
  summarise(COUNTS = n()) %>%
  mutate(PERCENTAGES = (COUNTS/63413)* 100 )  
  
# View(BULL_SHIT)  
# 
# testing_percentages <- testingFile %>% 
#   group_by(OCCURRED_YEAR, 
#            OFF_RACE,
#            CIT_RACE) %>% 
#   summarise(COUNTS = n()) %>%
#   mutate(PERCENTAGES = round(((COUNTS/63413)* 100), 4)) %>% 
#   ggplot(aes(x = CIT_RACE, y = OFF_RACE)) +
#   geom_col(fill = "blue", color = "black") + # play with this!
#   geom_text(aes(label = PERCENTAGES, y = OFF_RACE)) +
#   # scale_y_continuous(labels=percent)
#   facet_wrap( ~ OCCURRED_YEAR) +
#   # labs(y = "Number of reported",
#   #      title = "Findings graphed by Citizen Race vs Officer Race") +
#   theme(axis.text.x = element_text(angle = 45))
# 
# # displaying chart
# testing_percentages


library(RColorBrewer)

palPaired_by_OFF_SEX <- colorFactor(palette = 'Set1',
              domain = factor(testingFile$OFF_SEX))


# View(testingFile)


TESTING_PERCENT_2 <- testingFile %>%
  group_by(OCCURRED_YEAR,
           OFF_RACE,
           CIT_RACE) %>%
  summarise(COUNTS = n()) %>%
  mutate(PERCENTAGE = COUNTS/sum(COUNTS)) %>% 
  group_by(OCCURRED_YEAR,
           OFF_RACE,
           CIT_RACE, 
           PERCENTAGE) %>%  # IS THIS PART POINTESS?
  ggplot(aes(x = CIT_RACE, y = PERCENTAGE)) +
  geom_bar(stat = "identity", 
           # fill = "pink",
             fill = palPaired_by_OFF_SEX(OFF_SEX),
           color = "black") +
  facet_wrap( ~ OCCURRED_YEAR) +
  scale_y_continuous(labels = scales::percent)
  

 
# displaying chart
# View(TESTING_PERCENT_2)
TESTING_PERCENT_2




fingingsVsRace <- testingFile %>%
  select(everything()) %>%
  filter() %>%  # so cool, you can pass filter straight though with no parameters so you can then have the select all active and filter
  # based on user input, it'd have to be a little "tedious" but you can easily write out an if/else or switch type of condition
  ggplot(aes(x = CIT_RACE)) +
  facet_wrap( ~ testingFile$OCCURRED_YEAR) +
  geom_col(aes(y = OFF_RACE)) +
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = paste0(prop.table(..count..) * 100, '%')),
            stat = 'count') +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Race vs Officer Race") +
  theme(axis.text.x = element_text(angle = 45))

# displaying chart
fingingsVsRace


TESTING_TESTING <- testingFile %>%
ggplot(aes(x= CIT_RACE,  group=OFF_RACE)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="OFF_RACE") +
  # facet_grid(~OCCURRED_YEAR) +
  scale_y_continuous(labels = scales::percent)

TESTING_TESTING


findingsRaceVsAge <- ggplot(testingFile,
                            aes(x = CIT_RACE, y = OFF_AGE)) +
  theme_bw() +
  facet_wrap( ~ testingFile$OCCURRED_YEAR) +
  geom_point() +
  coord_flip() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen SEX vs Officer SEX") +
  theme(axis.text.x = element_text(angle = 45))

# displaying chart
# findingsRaceVsAge



TESTING1 <- ggplot(testingFile,
                   aes(x = CIT_RACE, fill = OFF_AGE)) +
  theme_bw() +
  facet_wrap( ~ testingFile$OCCURRED_YEAR) +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Race vs Officer Race") +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 9))


# TESTING1









ui <- fluidPage(titlePanel("UOF Data IMPD"),
                sidebarLayout(
                  sidebarPanel(
                    helpText("Something good here"),
                    selectInput(
                      inputId = "userSelected_X_input_for_facets_ggbar",
                      label = "Choose an X axis",
                      choices = c("Citizen Sex", "Citizen Race", "Citizen Age"),
                      selected = "Citizen Sex"
                    ),
                    selectInput(
                      inputId = "userSelected_fill_input_for_facets_ggbar",
                      label = "Choose an Y axis",
                      choices = c(
                        "Officer Sex",
                        "Officer Race",
                        "Officer Age",
                        "Citizen Charge Type"
                      ),
                      selected = "Officer Sex"
                    )
                  ),
                  mainPanel(plotOutput("ggplot_facetted_by_year"), width = 12)
                  
                ))



server <- function(input, output, session) {
  output$ggplot_facetted_by_year <- renderPlot({
    # palette(rainbow(12, s=08))
    
    x_data_point <-
      switch(
        input$userSelected_X_input_for_facets_ggbar,
        "Citizen Sex" = testingFile$CIT_SEX,
        "Citizen Race" = testingFile$CIT_RACE,
        "Citizen Age" = testingFile$CIT_AGE
      )
    
    y_data_point <-
      switch(
        input$userSelected_fill_input_for_facets_ggbar,
        "Officer Sex" = testingFile$OFF_SEX,
        "Officer Race" = testingFile$OFF_RACE,
        "Officer Age" = testingFile$OFF_AGE,
        "Citizen Charge Type" = testingFile$CITCHARGE_TYPE
      )
    
    
    
    ggplot(testingFile, aes(x = x_data_point,
                            fill = y_data_point)) +
      geom_bar() +
      theme_bw() +
      facet_wrap(~ testingFile$OCCURRED_YEAR) +
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
      guides(fill=guide_legend(title="New Legend Title!!!")) +
      theme(axis.text.x = element_text(angle = 90,
                                       size = 9))
  })
}

shinyApp(ui, server)
