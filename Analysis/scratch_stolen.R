library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(ggalt)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)

# Prepare the zip poly data for US
mydata <- readOGR(dsn = ".", layer = "cb_2016_us_zcta510_500k")

# Texas zip code data
zip <- read_csv("zip_code_database.csv")
tx <- filter(zip, state == "TX")


# Get polygon data for TX only
mypoly <- subset(mydata, ZCTA5CE10 %in% tx$zip)

# Create a new group with the first three digit.
# Drop unnecessary factor levels.
# Add a fake numeric variable, which is used for coloring polygons later.

mypoly$group <- substr(mypoly$ZCTA5CE10, 1,3)
mypoly$ZCTA5CE10 <- droplevels(mypoly$ZCTA5CE10)

set.seed(111)
mypoly$value <- sample.int(n = 10000, size = nrow(mypoly), replace = TRUE)

# Merge polygons using the group variable
# Create a data frame for ggplot.
mypoly.union <- unionSpatialPolygons(mypoly, mypoly$group)

mymap <- fortify(mypoly.union)

# Check how polygons are like

plot(mypoly)
plot(mypoly.union, add = T, border = "red", lwd = 1)


# Convert SpatialPolygons to data frame and aggregate the fake values
mypoly.df <- as(mypoly, "data.frame") %>%
  group_by(group) %>%
  summarise(value = sum(value))



# Find a center point for each zip code area
centers <- data.frame(gCentroid(spgeom = mypoly.union, byid = TRUE))
centers$zip <- rownames(centers)


# Finally, drawing a graphic
ggplot() +
  geom_cartogram(data = mymap, aes(x = long, y = lat, map_id = id), map = mymap) +
  geom_cartogram(data = mypoly.df, aes(fill = value, map_id = group), map = mymap) +
  geom_text_repel(data = centers, aes(label = zip, x = x, y = y), size = 3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()
