# Make up some random levels. (TODO: Better example)
year19$Zip <- factor(sample.int(5L, nrow(year19), TRUE))

factpal <- colorFactor(topo.colors(512), year19$Zip)

leaflet(year19) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~factpal(Zip))