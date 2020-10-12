# impd importing all data 

# more info on the datset
# https://xmaps.indy.gov/arcgis/rest/services/OpenData/OpenData_NonSpatial/MapServer/14
# dataset found here:
# http://data.indy.gov

complaints <- read.csv("IMPD_COMPLAINTS.csv",
                       stringsAsFactors = F,
                       encoding = "UTF-8")

useOfForce <- read.csv("IMPD_Use_Of_Force.csv",
                       stringsAsFactors = F,
                       encoding = "UTF-8")
ucr <- read.csv("IMPD_UCR_2018_Data.csv",
                stringsAsFactors = F,
                encoding = "UTF-8")
officerInvoledShooting <- read.csv("IMPD_Officer_Involved_Shootings.csv",
                                   stringsAsFactors = F,
                                   encoding = "UTF-8")


# viewing all of the data (in this current window, 
# un-explicit file ext, spreadsheet style)
# View(complaints)
# View(useOfForce)
# View(ucr)
# View(officerInvoledShooting)
