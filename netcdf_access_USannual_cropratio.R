cropacres_annual <- function(year, crops, damagecause){

  
  library(leaflet)
  library(rgdal)
  
damage <- read.csv("/dmine/code/git/dmine-clustering/damage.csv")
damage <- damage[,2:13]

colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
damage$State <- state.name[match(damage$State,state.abb)]

damage_loss1 <- subset(damage, Commodity == crops)
damage_loss1 <- subset(damage_loss1, Year == year)

damage_loss2 <- subset(damage_loss1, Damagecause == damagecause)
damage_loss <- subset(damage_loss2, Year == year)

damage_crop1 <- aggregate(damage_loss$Acres, by=list(damage_loss$State, damage_loss$County), FUN = "sum")
colnames(damage_crop1) <- c("State", "County", "Acres")
damage_crop_all <- aggregate(damage_loss1$Acres, by=list(damage_loss1$State, damage_loss1$County), FUN = "sum")
colnames(damage_crop_all) <- c("State", "County", "Acres")

damage_crop_final <- merge(damage_crop1, damage_crop_all, by=c("State", "County"))

colnames(damage_crop_final) <- c("State", "County", "crop_acres", "total_acres")
damage_crop_final$acre_ratio <- damage_crop_final$crop_acres / damage_crop_final$total_acres

counties <- readShapePoly('/dmine/data/counties/UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

colnames(counties@data)[1] = "County"
colnames(counties@data)[2] = "State"

counties_ratio <- merge(counties, damage_crop_final, by=c("State", "County"))

pal <- colorNumeric(rev(brewer.pal(11, "Spectral")),  na.color = "#ffffff", domain = eval(parse(text=paste("counties_ratio$acre_ratio", sep=""))))


exte <- as.vector(extent(counties_ratio))

label <- paste(sep = "<br/>", counties_ratio$County, round(eval(parse(text=paste("counties_ratio$acre_ratio", sep=""))), 3))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("counties_ratio$acre_ratio", sep=""))))

map <- leaflet(data = counties_ratio) %>% addProviderTiles("Stamen.TonerLite")  %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("counties_ratio$acre_ratio", sep="")))),  popup = markers$label, weight = 1)  %>%   
  addLegend(pal = pal, values = ~na.omit(eval(parse(text=paste("counties_ratio$acre_ratio", sep="")))), title = paste(crops, "/", damagecause, " Acreage Ratio ", year, sep=""),  labels = c("1", "2"), opacity = .5,
            position = "bottomright")

map

}















  