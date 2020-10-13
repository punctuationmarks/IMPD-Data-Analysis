# more info on the datset
# https://xmaps.indy.gov/arcgis/rest/services/OpenData/OpenData_NonSpatial/MapServer/14
# dataset found here:
# http://data.indy.gov


#
# interesting insights while cleaning the data:
#   there is a clear difference between user hand-typed data and
#   a pre-filed-choice for entering data (e.g. possibly a drop down box
#   for certain variables)
# For instance, I had to clean up the capitalization of STREET, CITY, UDTEXT24C
#
# But did not need to clean up the capitalization of UOF_REASON, UOF_TYPE, CITCHARGE_TYPE
#
# Also there were some that seemed so common there was a pre-filed-choice for things like
# CIT_CONDITION, OFF_CONDITION, OFF_RACE, and CIT_RACE. These seemed to only need to be made uniform
# once there was custom entered data
#



# This is mostly using base R packages, only using dplyr for some quick processing at the end
# (and some exploration since the tidyverse is quite handy)
# NOTE, using mostly the tidyverse this would be much quicker and less code, just sayin'
# (for instance using stringr::str_squish to remove white space would speed it up,
# but this is nice practice and layout of how to generally clean data)

library(tidyverse)



# strings are not factors by default, encoding is utf-8 by default
# read_csv() is comma diliminated, and read_csv2() is semi-colon deliminated
# ALSO, 10X faster than base read csv
# to use the base read.csv() just specify the encoding (UTF-8 is usually best, and be sure to turn strings_as_factors to FALSE
# unless you want them as factors)
UOF_csv <-
  readr::read_csv("../RawData/IMPD_Use_Of_Force.csv")



# viewing raw dataset
# View(UOF_csv)

# grabbing quick info
# glimpse(UOF_csv)


# making a copy of the raw data
wrangledUOF.df <- UOF_csv

# just verifying the copy works
# glimpse(wrangledUOF.df)
# View(wrangledUOF.df)





# Fixing mispellings of the word "Indianapolis"
# #
# # # This section could be refactored by changing all of the city names to capital first letters, with the use of
# # # tidyverse's str_to_title() or some other way of having all the same capitalization,
# # # but there is something interesting in seeing all (minus the three capitalization differences in
# # # variable withoutCorrectSpelling) the different ways that the officer spelled "Indianapolis".
# # # Also interesting that it doesn't seem to have a dropdown box for entering the city name
# # # (on the original reporting program given by the IMPD to the PO),
# # # but there is one for the UOF_REASON, UOF_TYPE, and CITCHARGE_TYPE since those didn't have
# # # a single spelling error in either variable's observations.
# #


# Assuming all of the missing city data takes place in indianapolis (or at least Marion county)
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == " ")] = "Indianapolis"
wrangledUOF.df$CITY[which(is.na(wrangledUOF.df$CITY))] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indiana")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indiana")] = "Indianapolis"


# # Used to narrow down all of the misplellings of the city name "Indianapolis
# withoutCorrectSpelling <- wrangledUOF.df %>% filter(!grepl("Indianapolis|indianapolis|INDIANAPOLIS", CITY))
# View(withoutCorrectSpelling)

wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indpls")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indpls")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDPLS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Ind")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "ind")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Iindianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapoli")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Idianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indanapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indiaapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapoplis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indian")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indian")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indinapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indainapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indiaanapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapoils")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianapoplis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianaolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapols")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Inidianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indiananapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianpolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapois")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Inddianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianaplis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDIANAPLIS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indinaapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianaplis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indinapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Injdianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianaplois")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "iNDIANAPOLIS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indiapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indiananpolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Inianapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDY")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianaoplis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapolii")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapolils")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapoliks")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "IndianapollS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDIANApolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapoluis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianapoolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianpolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDIANPOLIS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDOLS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indiapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianaolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianapoli")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapolid")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDIANAPOLID")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "IndianapolIS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDIANAPOLIIS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indiapolis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianapois")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "Indianapoins")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianaoplis")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "INDANAPOLIS")] = "Indianapolis"
wrangledUOF.df$CITY[which(wrangledUOF.df$CITY == "indianapolsi")] = "Indianapolis"



# View(wrangledUOF.df)




wrangledUOF.df$OCCURRED_DT[which(is.na(wrangledUOF.df$OCCURRED_DT))] = "2000-01-01"
wrangledUOF.df$OCCURRED_TM[which(wrangledUOF.df$OCCURRED_TM == "")] = "Unreported"
wrangledUOF.df$OCCURRED_TM[which(is.na(wrangledUOF.df$OCCURRED_TM))] = "Unreported"
wrangledUOF.df$OCCURRED_TM[which(wrangledUOF.df$OCCURRED_TM == "00:00:00:000")] = "Unreported"


# finding all of the outlier/incorrect/less accurate streets
#
# interSectionStreets <- wrangledUOF.df %>%
#   dplyr::arrange(STREET, STREET_N, STREET_T, STREET_G) %>%
#   dplyr::filter(grepl('/|and|And|@', STREET))


# View(interSectionStreets)




# highWays <- wrangledUOF.df %>%  dplyr::filter(grepl('i70|i65|i465|I70|I65|I465|i-70|i-65|i-465|I-70|I-65|I-465|interstate|Interstate', STREET))

# View(highWays)



# Cleaning obvious outliers on street names
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th  / Rear Parking Lot")] = "10th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th  / Rear Parking Lot	")] = "10th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th Street")] = "10th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th St")] = "10th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == 10)] = "10th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10")] = "10th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "30th Street")] = "30th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "1437  Iron Trail E")] = "Iron Trail"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "W 38th")] = "38th"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "2405 Madison Ave")] = "Madison"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "8505 Frongate")] = "Frongate"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "8524 Adlington")] = "Adlington"


# Skeleton of the variables for renaming address observations
# # NOTE: Because we are searching these by the street name AND changing the street name
# # we need to change the street name as the last change, since if we don't the type, geo, or number might not link up
# wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "")] = ""
# wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "")] = ""
# wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "")] = ""
# wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "")] = ""


# separating some street data

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "2100 N Montcalm")] = "2100"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "2100 N Montcalm")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "2100 N Montcalm")] = "St"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "2100 N Montcalm")] = "Montcalm"

wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th And Dearborn ")] = "BUGGY_SOME_REASON"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th And Dearborn")] = "BUGGY_SOME_REASON"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "BUGGY_SOME_REASON")] = "3200"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "BUGGY_SOME_REASON")] = "E"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "BUGGY_SOME_REASON")] = "St"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "BUGGY_SOME_REASON")] = "10th"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "10th Street/LaSalle Street")] = "3300"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "10th Street/LaSalle Street")] = "St"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "10th Street/LaSalle Street")] = "E"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "10th Street/LaSalle Street")] = "10th"


wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Jenny Lane / Pleasant Run Pkwy N Dr")] = "4700"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Jenny Lane / Pleasant Run Pkwy N Dr")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Jenny Lane / Pleasant Run Pkwy N Dr")] = "Dr"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Jenny Lane / Pleasant Run Pkwy N Dr")] = "Pleasant Run Pkwy"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "jenny lane / pleasant run pkwy n dr")] = "4700"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "jenny lane / pleasant run pkwy n dr")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "jenny lane / pleasant run pkwy n dr")] = "Dr"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "jenny lane / pleasant run pkwy n dr")] = "Pleasant Run Pkwy"


wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Patton Dr/Suburban Dr")] = "3546"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Patton Dr/Suburban Dr")] = "Dr"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Patton Dr/Suburban Dr")] = "Patton"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Hill valley/South Meridian")] = "8298"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Hill valley/South Meridian")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Hill valley/South Meridian")] = "St"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Hill valley/South Meridian")] = "Meridian"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Rockville Road/Mickley Ave")] = "28-2"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Rockville Road/Mickley Ave")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Rockville Road/Mickley Ave")] = "Ave"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Rockville Road/Mickley Ave")] = "Mickley"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Kentucky Ave/Minnesota Wy")] = "1790"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Kentucky Ave/Minnesota Wy")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Kentucky Ave/Minnesota Wy")] = "Ave"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Kentucky Ave/Minnesota Wy")] = "Kentucky"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "W Kentucky Av/S Harding St")] = "1790"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "W Kentucky Av/S Harding St")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "W Kentucky Av/S Harding St")] = "Ave"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "W Kentucky Av/S Harding St")] = "Kentucky"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Jackson st/South Meridian")] = "230"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Jackson st/South Meridian")] = "S"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Jackson st/South Meridian")] = "St"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Jackson st/South Meridian")] = "Meridian"


wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Lafayette Rd/Georgetown Rd")] = "4320"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Lafayette Rd/Georgetown Rd")] = ""
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Lafayette Rd/Georgetown Rd")] = "Rd"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Lafayette Rd/Georgetown Rd")] = "Layfayette"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "34th And Massachusetts")] = "5991"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "34th And Massachusetts")] = ""
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "34th And Massachusetts")] = "Ave"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "34th And Massachusetts")] = "Massachusetts"

wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "E. 75th St. And Johnson Rd.")] = "6801"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "E. 75th St. And Johnson Rd.")] = "E"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "E. 75th St. And Johnson Rd.")] = "St"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "E. 75th St. And Johnson Rd.")] = "75th"


wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "W. 11th Street And I-65")] = "441"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "W. 11th Street And I-65")] = "W"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "W. 11th Street And I-65")] = "St"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "W. 11th Street And I-65")] = "11th"

wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Fall Creek Pkwy N.")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Fall Creek Pkwy N.")] = "Pkwy"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Fall Creek Pkwy N.")] = "Fall Creek"


# just need to fix the cardinal direction here
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Waterfront Pkwy W")] = "W"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Waterfront Pkwy W")] = "Waterfront Pkwy"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "US 31")] = "US-31"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "US 31 South" |
                                wrangledUOF.df$STREET == "US 31 S")] = "S"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "US 31 South" |
                              wrangledUOF.df$STREET == "US 31 S")] = "US-31"

# View(wrangledUOF.df)





# Google maps will actually swtich this to a 1126 N Sherman Dr if I leave it as Ave here
wrangledUOF.df$STREET_T[which(
  wrangledUOF.df$STREET == "sherman" &
    # so notice how this is lowercase? this is why you should do things like case sensitivity and remove white space at the get go
    # but this is for training and learning experience, but keep these things in mind
    wrangledUOF.df$STREET_N == 1126 &
    wrangledUOF.df$STREET_G == "S"
)] = "Dr"


# Google maps won't be able to find this one unless it's 3701
wrangledUOF.df$STREET_N[which(
  wrangledUOF.df$STREET == "Post" | wrangledUOF.df$STREET == "post" &
    wrangledUOF.df$STREET_N == 3700
)] = 3701




wrangledUOF.df$STREET_T[which(
  wrangledUOF.df$STREET == "Pennsylvania" &
    wrangledUOF.df$STREET_N == 2900 &
    wrangledUOF.df$STREET_G == "S"
)] = "St"




## !! IMPORTANT !! ##
# Any addresses that are not reported will be defaulted to
# the downtown district building of IMPD (39 Jackson Place)
# AND all highways will be the Department of Transportation (100 N Senate Ave)

wrangledUOF.df$STREET_N[which(is.na(wrangledUOF.df$STREET_N) &
                                is.na(wrangledUOF.df$STREET))] = "39"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "")] = "Jackson"
wrangledUOF.df$STREET[which(is.na(wrangledUOF.df$STREET))] = "Jackson"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "")] = "Place"
wrangledUOF.df$STREET_T[which(is.na(wrangledUOF.df$STREET))] = "Place"


# Grouping all of the UOF happenings on the highway and setting them to the DOT
# This is for costs and time, since finding the actual GPS location (lat/lon)
# of the incidents would be too high at this time (but with more funding, it is possible)
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I465 8.6 Mm")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I465 Northbound")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I70")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I65")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I465")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I70/I465")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I70/465")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I69/Sr37")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "I69 (N/B)")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Mile Marker 84	")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I70")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "Interstate 74")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I69/Sr37")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "Interstate 65 S")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I465 Northbound")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "Interstate 74 Wb")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "Interstate 69")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I-465")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I465 8.6 Mm")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I-70 Wb @ 83.5 Mm	")] = "Department of Transportation"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET_N == "I70")] = "Department of Transportation"



# Changing the placeholder to be the actual address of the INDOT
wrangledUOF.df$STREET_N[which(wrangledUOF.df$STREET == "Department of Transportation")] = "100"
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET == "Department of Transportation")] = "N"
wrangledUOF.df$STREET_T[which(wrangledUOF.df$STREET == "Department of Transportation")] = "Ave"
wrangledUOF.df$STREET[which(wrangledUOF.df$STREET == "Department of Transportation")] = "Senate"





# LEAVING STREET TYPE BLANK FOR GMAPS
wrangledUOF.df$STREET_G[which(is.na(wrangledUOF.df$STREET_G))] = " "
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET_G == "")] = " "
wrangledUOF.df$STREET_G[which(wrangledUOF.df$STREET_G == " ")] = " "



# dealing with NA and blank strings
wrangledUOF.df$UOF_FORCE_TYPE[which(is.na(wrangledUOF.df$UOF_FORCE_TYPE))] = "Unreported"
wrangledUOF.df$UOF_FORCE_TYPE[which(wrangledUOF.df$UOF_FORCE_TYPE == "")] = "Unreported"
wrangledUOF.df$UOF_FORCE_TYPE[which(wrangledUOF.df$UOF_FORCE_TYPE == " ")] = "Unreported"


wrangledUOF.df$UOF_REASON[which(is.na(wrangledUOF.df$UOF_REASON))] = "Unreported"
wrangledUOF.df$UOF_REASON[which(wrangledUOF.df$UOF_REASON == "")] = "Unreported"

wrangledUOF.df$SERVICE_TYPE[which(is.na(wrangledUOF.df$SERVICE_TYPE))] = "Unreported"
wrangledUOF.df$SERVICE_TYPE[which(wrangledUOF.df$SERVICE_TYPE == "")] = "Unreported"

wrangledUOF.df$UDTEXT24A[which(is.na(wrangledUOF.df$UDTEXT24A))] = "Unreported"
wrangledUOF.df$UDTEXT24B[which(is.na(wrangledUOF.df$UDTEXT24B))] = "Unreported"
wrangledUOF.df$UDTEXT24C[which(is.na(wrangledUOF.df$UDTEXT24C))] = "Unreported"
wrangledUOF.df$UDTEXT24D[which(is.na(wrangledUOF.df$UDTEXT24D))] = "Unreported"


wrangledUOF.df$UDTEXT24C[which(wrangledUOF.df$UDTEXT24A == " ")] = "Unreported"
wrangledUOF.df$UDTEXT24B[which(wrangledUOF.df$UDTEXT24B == " ")] = "Unreported"
wrangledUOF.df$UDTEXT24C[which(wrangledUOF.df$UDTEXT24C == " ")] = "Unreported"
wrangledUOF.df$UDTEXT24D[which(wrangledUOF.df$UDTEXT24D == " ")] = "Unreported"

wrangledUOF.df$UDTEXT24A[which(wrangledUOF.df$UDTEXT24A == "")] = "Unreported"
wrangledUOF.df$UDTEXT24B[which(wrangledUOF.df$UDTEXT24B == "")] = "Unreported"
wrangledUOF.df$UDTEXT24C[which(wrangledUOF.df$UDTEXT24C == "")] = "Unreported"
wrangledUOF.df$UDTEXT24D[which(wrangledUOF.df$UDTEXT24D == "")] = "Unreported"

wrangledUOF.df$DISPOSITION[which(wrangledUOF.df$DISPOSITION == "")] = "Unreported"



wrangledUOF.df$CIT_ARRESTED[which(wrangledUOF.df$CIT_ARRESTED == "")] = "Unreported"
wrangledUOF.df$CITCHARGE_TYPE[which(wrangledUOF.df$CITCHARGE_TYPE == "")] = "Unreported"
wrangledUOF.df$CITCHARGE_TYPE[which(is.na(wrangledUOF.df$CITCHARGE_TYPE))] = "Unreported"
wrangledUOF.df$CIT_INJURED[which(wrangledUOF.df$CIT_INJURED == " ")] = "Unreported"
wrangledUOF.df$CIT_INJURED[which(is.na(wrangledUOF.df$CIT_INJURED))] = "Unreported"
wrangledUOF.df$CIT_HOSPITAL[which(wrangledUOF.df$CIT_HOSPITAL == "")] = "Unreported"
wrangledUOF.df$RACE[which(wrangledUOF.df$RACE == "")] = "Unreported"
wrangledUOF.df$SEX[which(wrangledUOF.df$SEX == "")] = "Unreported"
wrangledUOF.df$CIT_COND_TYPE[which(wrangledUOF.df$CIT_COND_TYPE == " ")] = "Unreported"
wrangledUOF.df$CIT_COND_TYPE[which(wrangledUOF.df$CIT_COND_TYPE == "")] = "Unreported"
wrangledUOF.df$CIT_COND_TYPE[which(is.na(wrangledUOF.df$CIT_COND_TYPE))] = "Unreported"
wrangledUOF.df$CIT_WEAPON_TYPE[which(wrangledUOF.df$CIT_WEAPON_TYPE == "")] = "Unreported"
wrangledUOF.df$CIT_WEAPON_TYPE[which(is.na(wrangledUOF.df$CIT_WEAPON_TYPE))] = "Unreported"

# # we'll just say this is 121 to ensure keeping the observation and have the number be a ridiculous number
wrangledUOF.df$CIT_AGE[which(is.na(wrangledUOF.df$CIT_AGE))] = 121


# # we'll just say this is 9999 to ensure keeping the observation and have the number be a ridiculous number
wrangledUOF.df$OFFNUM[which(is.na(wrangledUOF.df$OFFNUM))] = 9999
wrangledUOF.df$OFF_RACE[which(is.na(wrangledUOF.df$OFF_RACE))] = "Unreported"
wrangledUOF.df$OFF_RACE[which(wrangledUOF.df$OFF_RACE == " ")] = "Unreported"
wrangledUOF.df$OFF_SEX[which(is.na(wrangledUOF.df$OFF_SEX))] = "Unreported"
wrangledUOF.df$OFF_SEX[which(wrangledUOF.df$OFF_SEX == " ")] = "Unreported"
wrangledUOF.df$OFF_COND_TYPE[which(is.na(wrangledUOF.df$OFF_COND_TYPE))] = "Unreported"
wrangledUOF.df$OFF_COND_TYPE[which(wrangledUOF.df$OFF_COND_TYPE == " ")] = "Unreported"
wrangledUOF.df$OFF_COND_TYPE[which(wrangledUOF.df$OFF_COND_TYPE == "")] = "Unreported"
wrangledUOF.df$DISPOSITION[which(is.na(wrangledUOF.df$DISPOSITION))] = "Unreported"
wrangledUOF.df$DISPOSITION[which(wrangledUOF.df$DISPOSITION == " ")] = "Unreported"

wrangledUOF.df$OFF_INJURED[which(is.na(wrangledUOF.df$OFF_INJURED))] = "Unreported"
wrangledUOF.df$OFF_INJURED[which(wrangledUOF.df$OFF_INJURED == "")] = "Unreported"
wrangledUOF.df$OFF_INJURED[which(wrangledUOF.df$OFF_INJURED == " ")] = "Unreported"
wrangledUOF.df$OFF_HOSPITAL[which(wrangledUOF.df$OFF_HOSPITAL == "")] = "Unreported"
wrangledUOF.df$OFF_HOSPITAL[which(wrangledUOF.df$OFF_HOSPITAL == " ")] = "Unreported"


# Chaning any illogical numeric data to Unreported so it won't be graphed
# # we'll just say this is 121 to ensure keeping the observation and have the number be a ridiculous number
wrangledUOF.df$OFF_AGE[which(wrangledUOF.df$OFF_AGE < 18)] = 121
wrangledUOF.df$OFF_YR_EMPLOY[which(wrangledUOF.df$OFF_YR_EMPLOY < 0)] = 121



# View(wrangledComplaints.df)

# View(wrangledUOF.df)

#
# cleaning up the visual reading of the observations
## NOTE, this step should have been done first, right off the bat. The point of this tutorial
# is to show you how to use most of the base functions and then to ween you into using the Tidyverse,
# notice how easy it is to change full variable's observations (eg dplyr with STATE) and how to change
# string case with stringr

wrangledUOF.df <- wrangledUOF.df %>%
  dplyr::mutate(STATE = "IN") %>%
  mutate(UDTEXT24A = stringr::str_to_sentence(UDTEXT24A)) %>%
  mutate(UDTEXT24B = str_to_sentence(UDTEXT24B)) %>%
  # leaving UDTEXT24C all capitalized, due to complications/"time suck" on converting
  # the data to lower case (due to NW, DT, SE, ect)
  mutate(UDTEXT24C = stringr::str_to_upper(UDTEXT24C)) %>%
  mutate(UDTEXT24D = str_to_upper(UDTEXT24D)) %>%
  mutate(UOF_FORCE_TYPE = str_to_sentence(UOF_FORCE_TYPE)) %>%
  mutate(OFF_COND_TYPE = str_to_sentence(OFF_COND_TYPE)) %>%
  mutate(CIT_COND_TYPE = str_to_sentence(CIT_COND_TYPE)) %>%
  mutate(STREET = str_to_title(STREET)) %>%
  mutate(STREET_T = str_to_title(STREET_T)) %>%
  mutate(CITY = str_to_title(CITY))

# View(wrangledUOF.df)

# # an easier way of seeing how many issues in street names there are (option way would be to make them factors)
# numberByStreetName <- wrangledUOF.df %>% group_by(STREET) %>% summarise(n())
# View(numberByStreetName)








#
# # saving new clean database, one here and one in the directory CleanData/
# # using tidyverse since it's much faster
# readr::write_csv(wrangledUOF.df, "cleanedUOF_data.csv")
# write_csv(wrangledUOF.df, "../CleanData/cleanedUOF_data.csv")
#
#
# checkingWriteReadFunction <- read_csv("cleanedUOF_data.csv")
# View(checkingWriteReadFunction)
