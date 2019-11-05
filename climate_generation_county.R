climate_generation_county <- function(climvar, year) {

  #set up state and county data frame
  
  data <- readShapePoly(paste("county_", "pet", "_", "2015", ".shp", sep=""),
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  
  statecounty <- cbind(data.frame(data@data[,2]), data.frame(data@data[,3]))
  colnames(statecounty) <- c("State", "County")
  
  
  mergeddata2 <- matrix(NA, ncol = 6)
  colnames(mergeddata2) <- c("State", "County", "pet", "pr", "tmmx", "year")
  
  
  
for (i in 2001:2015) {  
  jj = 0
  mergeddata <- matrix(NA, nrow = 3109, ncol = 4)
  for (j in c("pet", "pr", "tmmx")) {
  
  jj = jj + 1

  setwd("/dmine/data/GRIDMET_USannual/")

  data <- readShapePoly(paste("county_", j, "_", i, ".shp", sep=""),
                      proj4string=CRS
                      ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


  mergeddata[,jj] <- data@data[,6]

  }
  
  mergeddata[,4] <- i
  mergeddata <- cbind(statecounty, mergeddata)
  colnames(mergeddata) <- c("County", "State", "pet", "pr", "tmmx", "year")
  
  mergeddata2 <- rbind(mergeddata, mergeddata2)
  
}
  
  mergeddata3 <- mergeddata2[-46636,]
  
  countyaverage_tmmx <- aggregate(mergeddata3$tmmx, by=list(mergeddata3$State, mergeddata3$County), FUN = "mean")
  colnames(countyaverage_tmmx) <- c("State", "County", "tmmx_average")
  
  countyaverage_pet <- aggregate(mergeddata3$pet, by=list(mergeddata3$State, mergeddata3$County), FUN = "mean")
  colnames(countyaverage_pet) <- c("State", "County", "pet_average")
  
  countyaverage_pr <- aggregate(mergeddata3$pr, by=list(mergeddata3$State, mergeddata3$County), FUN = "mean")
  colnames(countyaverage_pr) <- c("State", "County", "pr_average")
  
  
  anomaly1 <- merge(mergeddata3, countyaverage_tmmx, by=c("County", "State"))  
  anomaly2 <- merge(anomaly1, countyaverage_pet, by=c("County", "State"))  
  anomaly3 <- merge(anomaly2, countyaverage_pr, by=c("County", "State"))  
  
  anomaly3$pet_anomaly <- anomaly3$pet - anomaly3$pet_average
  anomaly3$pr_anomaly <- anomaly3$pr - anomaly3$pr_average
  anomaly3$tmmx_anomaly <- anomaly3$tmmx - anomaly3$tmmx_average
  
  
  
  for (i in 2001:2015) {
    
    annual_anomaly <- subset(anomaly3, year == i)
    setwd("/dmine/data/counties/")
    annual_anomaly$County <- trimws(annual_anomaly$County[])
    annual_anomaly$State <- trimws(annual_anomaly$State[])
    
    
    counties <- readShapePoly('/dmine/data/counties/UScounties_conus.shp',
                              proj4string=CRS
                              ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    colnames(counties@data)[1] = "County"
    colnames(counties@data)[2] = "State"
    
    merged_anomaly <- merge(counties, annual_anomaly, by=c("County", "State"))
    writeOGR(obj=merged_anomaly, dsn="/dmine/data/GRIDMET_USannual", layer=paste("US_anomaly_", i, sep=""), driver="ESRI Shapefile") # this is in geographical projection
    
    
  }
  
  
}

