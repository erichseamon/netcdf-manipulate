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

for (h in 2007:2012) {
  
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
  
  setwd("/nethome/erichs/counties/")
  
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
  #counties <- counties[grep("Washington", counties@data$STATE_NAME),]
  
  
  
  #subsets to CONUS
  counties <- subset(counties, STATE_NAME != "Alaska")
  counties <- subset(counties, STATE_NAME != "Hawaii")
  
  #counties <- subset(counties, STATE_NAME == "Idaho")
  #counties <- subset(counties, NAME == "Latah")
  
  
  
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
  
  climatevar_short <- "pdsi"
  climatevar <- "palmer_drought_severity_index"
  nc <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/pet/monthly/", "pet_gridMET", ".nc?lon[0:1:1385],lat[0:1:584],", "eto", "[0:1:0][0:1:0][0:1:0],time[0:1:0]", sep=""))
  nc2 <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/pr/monthly/", "pr_gridMET", ".nc?lon[0:1:1385],lat[0:1:584],", "pr", "[0:1:0][0:1:0][0:1:0],time[0:1:0]", sep=""))
  
  
  #   nc$var$eto$dim[[1]]$name
  #  nc$var$eto$dim[[2]]$name
  #  nc$var$eto$dim[[3]]$name
  
  ##--
  
  # extract variable name, size and dimension
  v <- nc$var[[1]]
  size <- v$varsize
  dims <- v$ndims
  time <- 1
  nt <- size[time]              # length of time dimension
  lat <- nc$dim$lat$vals   # latitude position
  lon <- nc$dim$lon$vals  # longitude position
  
  # read sst variable
  r<-list()
  for (i in 1:nt) {
    start <- rep(1,dims)     # begin with start=(1,1,...,1)
    start[1] <- i             # change to start=(1,1,...,i) to read    timestep i
    count <- size                # begin with count=(nx,ny,...,nt), reads entire var
    count[1] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
    
    dt<-ncvar_get(nc, varid = 'eto', start = start, count = count)
    
    
    # convert to raster
    r[i]<-raster(dt)
    r[i] <- rotate(r[[i]])
    extent(r[[i]]) <- c(-124.7667, -67.0583, 25.0667, 49.4000)
    crs(r[[i]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  }
  
  # extract variable name, size and dimension
  v <- nc2$var[[1]]
  size <- v$varsize
  dims <- v$ndims
  time <- 1
  nt <- size[time]              # length of time dimension
  lat <- nc2$dim$lat$vals   # latitude position
  lon <- nc2$dim$lon$vals  # longitude position
  
  # read sst variable
  r2<-list()
  for (i in 1:nt) {
    start <- rep(1,dims)     # begin with start=(1,1,...,1)
    start[1] <- i             # change to start=(1,1,...,i) to read    timestep i
    count <- size                # begin with count=(nx,ny,...,nt), reads entire var
    count[1] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
    
    dt<-ncvar_get(nc2, varid = 'pr', start = start, count = count)
    
    
    # convert to raster
    r2[i]<-raster(dt)
    r2[i] <- rotate(r2[[i]])
    extent(r2[[i]]) <- c(-124.7667, -67.0583, 25.0667, 49.4000)
    crs(r2[[i]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  }
  
  r_pet <- brick(r)
  
  r_pr <- brick(r2)
  
  r_bal <- r_pr - r_pet
  
  funSPEI <- function(x, scale=1, na.rm=TRUE,...) as.numeric((SPEI::spei(x, scale=scale, na.rm=na.rm, ...))$fitted)
  
  rstSPEI <- raster::calc(r_bal, fun = funSPEI)
  
  sel <- subset(rstSPEI, 325:396) #2007-2012 by month
  
  sel2007 <- subset(rstSPEI, 325:336) 
  sel2008 <- subset(rstSPEI, 336:347)
  sel2009 <- subset(rstSPEI, 348:359)
  sel2010 <- subset(rstSPEI, 360:371)
  sel2011 <- subset(rstSPEI, 372:383)
  sel2012 <- subset(rstSPEI, 384:396)
  
  
  #spei moderate drougtht - -1.00 and below

  
  #get mean by month, grouping every three layers of pdsi
  
  groupn=function(n,m){rep(1:m,rep(n/m,m))}
  group3 = groupn(36,12)
  f = function(v){tapply(v,group3,mean)}
  out = calc(r3, f)
  
  #spei2007
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=13)
  layer <- c(1:12)
  for(ii in layer) {
    jj = 0
    for (l in countyfiploop) {
      jj = jj + 1
      subset_county <- counties[counties@data$FIPS == l,]
      i2 <- paste("layer.", ii + 324, sep="")
      i3 <- ii + 1
      e <- extract(sel2007[[i2]], subset_county) 
      newmatrix[jj,i3] <- mean(e[[1]])
      newmatrix[jj,1] <- l
      
    }  
  }
  
  nm <- data.frame(NA, nrow=newmatrix, ncol=13)
  nm$countyFIPS <- as.numeric(as.character(newmatrix[,1]))
  nm$jan <- as.numeric(as.character(newmatrix[,2]))
  nm$feb <- as.numeric(as.character(newmatrix[,3]))
  nm$mar <- as.numeric(as.character(newmatrix[,4]))
  nm$apr <- as.numeric(as.character(newmatrix[,5]))
  nm$may <- as.numeric(as.character(newmatrix[,6]))
  nm$jun <- as.numeric(as.character(newmatrix[,7]))
  nm$jul <- as.numeric(as.character(newmatrix[,8]))
  nm$aug <- as.numeric(as.character(newmatrix[,9]))
  nm$sep <- as.numeric(as.character(newmatrix[,10]))
  nm$oct <- as.numeric(as.character(newmatrix[,11]))
  nm$nov <- as.numeric(as.character(newmatrix[,12]))
  nm$dec <- as.numeric(as.character(newmatrix[,13]))
  
  nm2 <- data.frame(nm$countyFIPS, nm$jan, nm$feb, nm$mar, nm$apr, nm$may, nm$jun, nm$jul, nm$aug, nm$sep, nm$oct, nm$nov, nm$dec)
  colnames(nm2) <- c("FIPS","spei_jan", "spei_feb", "spei_mar", "spei_apr", "spei_may", "spei_jun", "spei_jul", "spei_aug", "spei_sep", "spei_oct", "spei_nov", "spei_dec" )
  nm2$FIPS <- str_pad(nm2$FIPS, 5, pad = "0")
  
  newmatrix2 <- matrix(NA, nrow=countylistrows, ncol=2)
  
  for (k in 1:nrow(nm2)) {
    
    newmatrix2[k,1] <- sum(nm2[k,2:13] < -1)
    newmatrix2[k,2] <- nm2[k,1]
    
    
  }
  
  newmatrix2[,1] <- as.numeric(newmatrix2[,1])
  newmatrix2[,2] <- as.numeric(newmatrix2[,2])
  colnames(newmatrix2) <- c(paste(h, "_days", sep=""), "FIPS")
  
  
  assign(paste("spei_moderate_drought_", h, sep=""), data.frame(newmatrix2))
  
}
