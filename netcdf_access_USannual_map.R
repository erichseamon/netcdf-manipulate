climate_annual <- function(year, climvar){

library(leaflet)
library(rgdal)

setwd("/dmine/data/GRIDMET_USannual/")

data <- readShapePoly(paste("county_", climvar, "_", year, ".shp", sep=""),
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

if (climvar == "pr"){
  pal <- colorNumeric(brewer.pal(11, "BrBG"),  na.color = "#ffffff", domain = eval(parse(text=paste("data$", climvar, sep=""))))
 } else if (climvar == "tmmx") {
   
data$tmmx <- data$tmmx - 273   
pal <- colorNumeric(rev(brewer.pal(11, "BrBG")),  na.color = "#ffffff", domain = eval(parse(text=paste("data$", climvar, sep=""))))
 } else {
   
pal <- colorNumeric(rev(brewer.pal(11, "BrBG")),  na.color = "#ffffff", domain = eval(parse(text=paste("data$", climvar, sep=""))))
   
 }


exte <- as.vector(extent(data))

label <- paste(sep = "<br/>", data$NAME, round(eval(parse(text=paste("data$", climvar, sep=""))), 3))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("data$", "pet", sep=""))))

map <- leaflet(data = data) %>% addProviderTiles("Stamen.TonerLite")  %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("data$", climvar, sep="")))),  popup = markers$label, weight = 1)  %>%   
addLegend(pal = pal, values = ~na.omit(eval(parse(text=paste("data$", climvar, sep="")))),  labels = c("1", "2"), opacity = .5,
position = "bottomright")

map

}

