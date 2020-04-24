# more info on the datset
# https://xmaps.indy.gov/arcgis/rest/services/OpenData/OpenData_NonSpatial/MapServer/14
# dataset found here:
# http://data.indy.gov



library(tidyverse)
library(ggrepel) # for plotting numbers on points in the graph

# has not been finished yet, still working on the cleaning data
complaints.df <- read_csv("../CleanData/Complaints/cleanedComplaint_data.csv")

# 
# # just grabbing the text from the allegations
# # to be used for a word cloud
# allegationsText <- complaints.df$ALLEGATION
# write.table(allegationsText, "allegationsOfComplaints.txt", row.names = F)

# colnames(complaints.df)

# organizing some of the data
# dropping address and excessive dates
  organizedComplants.df <- (
    complaints.df %>%
     select(
      ALG_CLASS,
      ALLEGATION,
      FINDING,
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
      OCCURRED_MONTH, everything()
    ) %>% 
    arrange(ALG_CLASS, FINDING)
  )




View(organizedComplants.df)







# setting up factors for graphing categorical data
# factors can be thought of as categorical data OR anything we won't be doing any mathemtical funcitons on 
# this might not be necessary with the current thought process of graphing

# organizedComplants.df$CIT_SEX <- as.factor(organizedComplants.df$CIT_SEX)
# organizedComplants.df$OFF_SEX <-
#   as.factor(organizedComplants.df$OFF_SEX)
# organizedComplants.df$CIT_RACE <-
#   as.factor(organizedComplants.df$CIT_RACE)
# organizedComplants.df$OFF_RACE <-
#   as.factor(organizedComplants.df$OFF_RACE)
# organizedComplants.df$OFF_AGE <-
#   as.factor(organizedComplants.df$OFF_AGE)
# organizedComplants.df$OFF_YR_EMPLOY <-
#   as.factor(organizedComplants.df$OFF_YR_EMPLOY)
# organizedComplants.df$CIT_AGE <-
#   as.factor(organizedComplants.df$CIT_AGE)
# organizedComplants.df$FINDING <-
#   as.factor(organizedComplants.df$FINDING)
# organizedComplants.df$ALG_CLASS <-
#   as.factor(organizedComplants.df$ALG_CLASS)
# organizedComplants.df$ALLEGATION <-
#   as.factor(organizedComplants.df$ALLEGATION)






# glimpse(tallyOfResultOfFindings)
# colnames(tallyOfResultOfFindings)




graph_tallyOfResultOfFindings <- organizedComplants.df %>%
  group_by(FINDING, OCCURRED_YEAR) %>%
  # filter(OCCURRED_YEAR != is.na(OCCURRED_YEAR)) %>% 
  filter(OCCURRED_YEAR == 2018) %>% 
  tally() %>%  # tally returns a column named 'n'
  rename(NUM_OF_FINDINGS = n) %>% 
  # dplyr::filter(NUM_OF_FINDINGS > 74) %>%  # if you wanted to drop the outliers
  # dplyr::filter(NUM_OF_FINDINGS != "Unreported") %>%
  dplyr::arrange(FINDING) %>% 
  # filter(OCCURRED_YEAR == 2018) %>% 
  ggplot2::ggplot(aes(x = NUM_OF_FINDINGS, y = FINDING)) +
  ggplot2::geom_point(aes(
    color = FINDING,
    size = NUM_OF_FINDINGS
  )) +
  ggrepel::geom_label_repel(aes(label = NUM_OF_FINDINGS, label.size = 0.15)) +
  labs(title = "Tally of Findings from citizen complaints against the IMPD 2014-2019",
    subtitle = "Brief analysis of data: 1 officer was 'Coached/Mentored', 
    1 officer vacated their position, 
    1 finding was 'Not Justified',
    330 findings were 'Unfounded',
    345 findings were 'Sustained',
    1028 findings were 'Not Sustained',
    1556 findings were 'Exonerated',
    and 74 investigation results were 'Unreported'.") +
  xlab("Number of internal investigation findings") +
  ylab("The results of IMPD internal investigations on the police officer after allegation") 
  # ggsave("../MediaOfFindings/Complaints/tallyOfAllFindings.png")

  # coord_flip()

graph_tallyOfResultOfFindings
# copying data to be manipultaed further


# 
# # displaying chart of ALL allegations based on Class of Allegation
# # this is due to the varity of actual allegations being too large, since they are civilian initiated
# 



findingsOfAllAllegations.vs.Findings <- organizedComplants.df %>% 
  ggplot2::ggplot(aes(x = ALG_CLASS, fill = FINDING)) + 
  labs(title = "Finding of Allegations FROM 2014-2019") +
  ggplot2::geom_bar() +
  ggplot2::coord_flip()
  # ggrepel::geom_text_repel()
  # ggrepel::geom_label_repel(aes(label = FINDING))
  
  # ggsave("../MediaOfFindings/Complaints/findingsOfAllAllegations_vs_Findings.png")

findingsOfAllAllegations.vs.Findings

# was attempting to have something with a numerical labeled bar chart
# datanovia.com/en/blog/how-to-create-a-ggplot-stacked-bar-chart-2/
# but is not working, too many variables?
# test <- organizedComplants.df %>% 
#   ggplot(aes(x = ALG_CLASS, y = FINDING)) +
#   geom_col(aes(fill = OCCURRED_YEAR), width = 0.7) 
#   # geom_text(aes(y = count(FINDING,), label = FINDING, group = as.integer(OCCURRED_YEAR)), color="white")


# used to narrow down the number of allegation to graph

organizing_top_allegations <- organizedComplants.df %>% 
  group_by(ALG_CLASS) %>% 
  tally(sort=TRUE) %>%  # tally returns a column named 'n'
  rename(NUM_OF_ALG_CLASS = n) %>% 
  filter(NUM_OF_ALG_CLASS > 30) 







#### tHI IS
topAllegationClasses.GRAPH <- organizedComplants.df %>%
  group_by(ALG_CLASS) %>%
  tally(sort = TRUE) %>%  # tally returns a column named 'n'
  rename(NUM_OF_ALG_CLASS = n) %>%
  filter(NUM_OF_ALG_CLASS > 30)  %>%
  ggplot2::ggplot(aes(x = NUM_OF_ALG_CLASS, y = ALG_CLASS)) + # Aesthetics being explicitly defined
  geom_point(aes(color = ALG_CLASS)) +
  ggrepel::geom_label_repel(aes(label = NUM_OF_ALG_CLASS),
                            label.r = 0.20,
                            label.size = 0.15) +
  labs(title = "Top Allegation Classes",
       subtitle = "Complaint allegations from 'citizens' on individual IMPD officers ",
       caption = "Data from Indy.gov", 
       color = "Allegation Class") +
  xlab("Number of Allegations in each Class") +
  ylab("Allegation Class") +
  theme(axis.text.y = element_text(angle = 45))  # tilting the ALG_CLASSe words to be easier to read
# theme_bw()
# theme_classic()
(topAllegationClasses.GRAPH + theme_bw())





### fucking with the color, waht I want is larger dots for larger occurances of allegation
### but it isn't working atm
topAllegationClasses.GRAPH <- top15AllegationClasses %>%
  ggplot2::ggplot(aes(x = NUM_OF_ALG_CLASS, y = ALG_CLASS )) + # Aesthetics being explicitly defined
  geom_point(aes(color=factor(ALG_CLASS),
                 shape = factor(NUM_OF_ALG_CLASS),
                 fill = factor(NUM_OF_ALG_CLASS))) +
  labs(title = "Top Allegation Classes") +
  xlab("Number of Allegations in each Class") +
  ylab("Allegation Class") +
  theme(axis.text.y = element_text(angle = 25)) # tilting the ALG_CLASSe words to be easier to read


topAllegationClasses.GRAPH











# View(organizedComplants.df)
# 
# ### BROKEN ###
# allegationsVsFindings <- organizedComplants.df %>% 
#   select(FINDING, ALG_CLASS) %>%
#   ggplot(aes(x = unique(ALG_CLASS), y =  ALG_CLASS)) + 
#   geom_bar(aes(fill = ALG_CLASS), stat="identity") +
#   facet_wrap(~FINDING, scales="fixed")



# allegationsByYear.2019 <- organizedComplants.df %>% 
#   filter(OCCURRED_YEAR == 2019) %>% 
#   group_by(ALLEGATION) %>% 
#   tally(name="NUM_OF_ALLEGATIONS", sort=TRUE)  
# 
# View(allegationsByYear.2019)













# saving the graphs in png
# ggsave("findings_of_allegations_IMPD.png",
#        plot = findingsOfAllegations)
# 

findingsOfAllegations2019 <- organizedComplants.df %>%
  filter(OCCURRED_YEAR == 2019)


# displaying percentages
# prop.table(table(organizedComplants.df$FINDING))


byCitizenSex <- ggplot2::ggplot(organizedComplants.df,
                       aes(x = CIT_SEX, fill=OFF_SEX)) + 
  ggplot2::geom_bar() 
  # ggplot2::coord_flip() # flipping to keep it in line with allegations graph


# displaying chart
byCitizenSex


byCitizenRaceWithOffRace <- ggplot2::ggplot(organizedComplants.df,
                        aes(x = CIT_RACE, fill=OFF_RACE)) + 
  labs(title = "All UOF from 2014-2019, citizen race ~ officer race") +
  ggplot2::geom_bar() + 
  
  ggplot2::coord_flip() # flipping to keep it in line with allegations graph


# displaying chart
byCitizenRaceWithOffRace


byOfficerSex <- ggplot(organizedComplants.df,
                       aes(x = OFF_SEX)) +
  geom_bar()

# displaying chart
# byOfficerSex



byOfficerSex.vs.citSex <- ggplot(organizedComplants.df,
                   aes(x = OFF_SEX, fill = CIT_SEX)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Complaints by Citizen Sex vs Officer Sex")

# displaying chart
byOfficerSex.vs.citSex


byOfficerRace.vs.citRace <- ggplot(organizedComplants.df,
                     aes(x = OFF_RACE, fill = CIT_RACE)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Complaints by Citizen Race vs Officer Race")

# displaying chart
byOfficerRace.vs.citRace



byCitRace.vs.officerRace <- ggplot(organizedComplants.df,
                                   aes(x = CIT_RACE, fill = OFF_RACE)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Complaints by Officer Race vs Citizen Race")

# displaying chart
byCitRace.vs.officerRace






#### Race and Sex with Findings of complaint ####

fingingsVsRace <- ggplot(organizedComplants.df,
                         aes(x = CIT_RACE, fill = OFF_RACE)) +
  theme_bw() +
  facet_wrap(~ FINDING) +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Race vs Officer Race") +
  theme(axis.text.x = element_text(angle = 45))

# displaying chart
fingingsVsRace

fingingsVsSex <- ggplot(organizedComplants.df,
                        aes(x = OFF_SEX, fill = CIT_SEX)) +
  theme_bw() +
  facet_wrap(~ FINDING) +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Sex vs Officer Sex") +
  theme(axis.text.x = element_text(angle = 45))


# displaying chart
fingingsVsSex







#### Race and Sex with Allegation Class ####


temp_df <- organizedComplants.df

# used to narrow down the number of allegation to graph
temp_df$ALG_CLASS <- as.factor(temp_df$ALG_CLASS)

tallyOfAllegationClasses <-
  row.names(as.data.frame(summary(temp_df$ALG_CLASS, max = 10)))

temp_df$ALG_CLASS <- as.character(temp_df$ALG_CLASS)

# View(temp_df)

temp_df$TOP_ALG_CLASSES <- ifelse(
  temp_df$ALG_CLASS %in% tallyOfAllegationClasses,
  temp_df$ALG_CLASS,
  "Other"
)


temp_df$TOP_ALG_CLASSES <- as.factor(temp_df$TOP_ALG_CLASSES)


testingPlot <- temp_df %>%
  ggplot(aes(x = factor(TOP_ALG_CLASSES, levels = names(
    sort(table(TOP_ALG_CLASSES))
  ))))
  geom_bar(aes(fill=OFF_RACE)) +
  coord_flip() +
  labs(title = "Top 10 Allegation Classes graphed by Officer Race from 2014-2019")


# testingPlot



testingPlot <- temp_df %>%
  ggplot(aes(x = factor(TOP_ALG_CLASSES, levels = names(sort(
    table(TOP_ALG_CLASSES)
  ))))) +
  geom_bar(aes(fill = OFF_SEX)) +
  labs(title = "Top 10 Allegation Classes graphed by Officer Sex",
       subtitle = "From 2014-2019. * 'Other' is the combination of all other Allegation classes") +
  ylab("Number of complaints") + # remember, the graph is flipped, so x and y are backwards :/
  xlab("Top 10 allegations") +
  coord_flip()



testingPlot




allegationClassVsRace <- ggplot(organizedComplants.df,
                         aes(x = CIT_RACE, fill = OFF_RACE)) +
  theme_bw() +
  facet_wrap(~ ALG_CLASS[1:10,]) +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Race vs Officer Race") +
  theme(axis.text.x = element_text(angle = 45))

# displaying chart
allegationClassVsRace







allegationClassVsSex <- ggplot(organizedComplants.df,
                        aes(x = OFF_SEX, fill = CIT_SEX)) +
  theme_bw() +
  facet_wrap(~ ALG_CLASS) +
  geom_bar() +
  labs(y = "Number of reported",
       title = "Findings graphed by Citizen Sex vs Officer Sex") +
  theme(axis.text.x = element_text(angle = 45))


# displaying chart
allegationClassVsSex




#### Tile graph of the Findings vs the Allegation Class ####

allegationsVsFindings <- organizedComplants.df %>% 
  count(FINDING, ALG_CLASS) %>% 
  ggplot(aes(x = FINDING, y = ALG_CLASS)) +
  geom_tile(aes(fill = n)) + 
  labs(title="allegationsVsFindings", subtitle = "Darker the color, the fewer of that outcome. The brighter, the more common") +
  theme(axis.text.x = element_text(vjust = 0.5, angle=45)) + 
  scale_x_discrete(name ="Findings from interal investigations of accusations") +
  scale_y_discrete(name="Accusation Class")
  

allegationsVsFindings



# displaying the ages


# citizenSexVsFindings
citizenSexVsFindings <- ggplot(organizedComplants.df, aes(x = CIT_SEX, fill = FINDING)) +
  geom_bar() + 
  facet_wrap(~ OFF_SEX, ncol = 1)

citizenSexVsFindings















# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 







#### WAFFLE GRAPH of ALLEGATIONS ####
#  the issue with this graph is that it needs exactly 100 tiles, or must be changed accordingly


## Organizing the allegations
# 
# 
# allegations <- organizedComplaint.df %>% 
#   dplyr::distinct(organizingComplants.df$ALLEGATION)
# 
# # displaying the number of unique allegations of complaints
# nrow(allegations)
# 
# allegationsObservations <- organizedComplaint.df$ALLEGATION
# 
# nrows <- 138
# 
# allegations.df <- tidyr::expand_grid(y = 1:nrows, x = 1:nrows)
# 
# category_table <-
#   round(table(allegationsObservations) * ((nrows * nrows) / length(allegationsObservations)))
# 
# View(category_table)
# 
# 
# # making the factor easier with the names being the layers 
# allegations.df$category <- factor(rep(names(category_table), category_table))
# 
# View(allegations.df)
# 
# 
# 
# ## Graphing the waffle
# 
# allegationWaffleChart <-
#   allegations.df %>% ggplot(aes(
#     x = x,
#     y = y,
#     fill = category
#   )) +
#   geom_tile(color = "black", size = 0.5) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
#   scale_fill_brewer(palette = "Set3") +
#   labs(title = "Waffle",
#        subtitle = "Citizen Complaint Allegations against IMPD",
#        caption = "CAPTION!") +
#   theme(
#     panel.border = element_rect(size = 2),
#     plot.title = element_text(size = rel(1.2)),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     legend.title = element_blank(),
#     legend.position = "right"
#   )
# 
# 

# allegationWaffleChart


#### WAFFLE GRAPH of ALLEGATIONS ####







