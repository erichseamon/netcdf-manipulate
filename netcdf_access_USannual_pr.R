#------------------------------------------------------------------------#
# TITLE:        netcdf_access_PDSI_days.R
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         Feb 1, 2019
#
# STAGE:        netcdf access
#
# COMMENTS:     This script opens and displays netcdf data.  
#
#--Setting the working directory an d clearing the workspace-----------#


#netcdf_access(climatevar_short, climatevar, year )
#netcdf_access

#number of days under pdsi across th five years

#netcdf_access_PDSI <- function(year) {
  
for (h in 2001:2015) {
  
  #library("ncdf")
  library("zoo")
  library("raster")
  library("sp")
  library("rgeos")
  library("rgdal")
  library("proj4")
  library("RNetCDF")
  library("ncdf4")
  library("RColorBrewer")
  library("raster")
  #library("rasterVis")
  library("latticeExtra")
  library("maptools")
  library("parallel")
  library("Evapotranspiration")
  library("plyr")
  library("data.table")
  library("sirad")
  library("rgdal")
  library("stringr")
  library("leaflet")
  
  setwd("/dmine/data/counties/")
  
  counties <- readShapePoly('UScounties_conus.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
  #counties <- counties[grep("Washington", counties@data$STATE_NAME),]
  
  
  
  #--loop list for county by fip
  countyfiploop <- counties@data$FIPS
  
  #--data frame of county fip list
  countyfiplist <- data.frame(counties@data$FIPS)
  
  #--data frame of county names
  countynames <- data.frame(counties@data$NAME)
  statenames <- data.frame(counties@data$STATE_NAME)
  
  #combo of county names and fip for this list
  countylist <- cbind(statenames, countynames, countyfiplist)
  colnames(countylist) <- c("STATE_NAME", "NAME", "FIPS")
  
  #--number of rows in county list
  countylistrows <- nrow(countylist)
  
  climatevar_short <- "pr"
  climatevar <- "precipitation_amount"
  
  nc <- nc_open(paste("/dmine/data/netcdf/pr_", h, "_average.nc", sep=""))
  
  ##--
  
  # extract variable name, size and dimension
  v <- nc$var[[1]]
  size <- v$varsize
  dims <- v$ndims
  nt <- size[dims]              # length of time dimension
  lat <- nc$dim$lat$vals   # latitude position
  lon <- nc$dim$lon$vals  # longitude position
  
  # read sst variable
  r<-list()
  #for (i in 1:nt) {
    start <- rep(1,dims)     # begin with start=(1,1,...,1)
    start[dims] <- 1             # change to start=(1,1,...,i) to read    timestep i
    count <- size                # begin with count=(nx,ny,...,nt), reads entire var
    count[dims] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
    
    dt<-ncvar_get(nc, varid = 'precipitation_amount', start = start, count = count)
    
    # convert to raster
    r<-raster(dt)
    #r<- rotate(r)
    extent(r) <- c(25.0667, 49.4000, -124.7667, -67.0583)
    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


  r3 <- t(flip(r, direction='x' ))
  
  #get mean by month, grouping every three layers of pdsi
  
  #r4 <- mean(r3)
  
  options(warn=-1)
  
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=2)
  layer <- c(1:1)
  for(ii in layer) {
    jj = 0
    for (l in countyfiploop) {
      jj = jj + 1
      subset_county <- counties[counties@data$FIPS == l,]
      i2 <- paste("X", ii, sep="")
      i3 <- ii + 1
      e <- extract(r3, subset_county)
      newmatrix[jj,2] <- mean(e[[1]])
      newmatrix[jj,1] <- l
      
    }  
  }
  
  options(warn=0)
  
  colnames(newmatrix) <- c("FIPS", "pr")
  county_pr <- merge(counties, newmatrix, by=c("FIPS"))
  
  county_pr$pr <- as.numeric(as.character((county_pr$pr)))
  
  
  
  writeOGR(obj=county_pr, dsn="/dmine/data/GRIDMET_USannual", layer=paste("county_pr_", h, sep=""), driver="ESRI Shapefile") # this is in geographical projection
  
  
  
  
  pal <- colorNumeric(brewer.pal(11, "BrBG"),  na.color = "#ffffff", domain = eval(parse(text=paste("county_pr$", "pr", sep=""))))
  
  exte <- as.vector(extent(counties))
  
  label <- paste(sep = "<br/>", county_pr$NAME, round(eval(parse(text=paste("county_pr$", "pr", sep=""))), 0))
  markers <- data.frame(label)
  labs <- as.list(eval(parse(text=paste("county_pr$", "pr", sep=""))))

#map <- leaflet(data = county_pr) %>% addProviderTiles("Stamen.TonerLite")  %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("county_pr$", "pr", sep="")))),  popup = markers$label, weight = 1)  
  #addLegend(pal = pal, values = ~na.omit(eval(parse(text=paste("county_pr$", "pr", sep="")))),  labels = c("1", "2"), opacity = .5,
            #position = "bottomright")

#map

}

