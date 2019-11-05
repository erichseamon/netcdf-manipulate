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
  
for (h in 2007) {
  
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
  
  climatevar_short <- "pr"
  climatevar <- "precipitation_amount"
  
  #nc <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", climatevar_short, "_1979_CurrentYear_CONUS.nc?lon[0:1:1385],lat[0:1:584],", climatevar, "[0:1:0][0:1:0][0:1:0],day[0:1:0]", sep=""))
  
  nc <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/pr/", climatevar_short, "_", h, ".nc?lon[0:1:1385],lat[0:1:584],", climatevar, "[0:1:0][0:1:0][0:1:0],day[0:1:0]", sep=""))
  
  #maxtemp <- nc_open('http://reacchpna.org/thredds/dodsC/agg_met_tmmx_1979_2014_WUSA.nc?lon[0:1:1385],lat[0:1:584],daily_maximum_temperature[0:1:0][0:1:0][0:1:0],day[0:1:10]')# Open a netcdf file 
  
  nc <- nc_open("/nethome/erichs/pr_2015_average.nc")
  
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
    r<- rotate(r)
    extent(r) <- c(25.0667, 49.4000, -124.7667, -67.0583)
    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  }
  
  r2 <- brick(r)
  
  r3 <- t(flip(r, direction='x' ))
  
  #get mean by month, grouping every three layers of pdsi
  
  groupn=function(n,m){rep(1:m,rep(n/m,m))}
  group3 = groupn(1,1)
  f = function(v){tapply(v,group3,sum)}
  out = calc(r3, f)
  
  #r4 <- mean(r3)
  
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
  colnames(nm2) <- c("FIPS","pdsi_jan", "pdsi_feb", "pdsi_mar", "pdsi_apr", "pdsi_may", "pdsi_jun", "pdsi_jul", "pdsi_aug", "pdsi_sep", "pdsi_oct", "pdsi_nov", "pdsi_dec" )
  nm2$FIPS <- str_pad(nm2$FIPS, 5, pad = "0")
  
  newmatrix2 <- matrix(NA, nrow=countylistrows, ncol=2)
  
  for (k in 1:nrow(nm2)) {
    
    newmatrix2[k,1] <- sum(nm2[k,2:13] < -2) #less than -2.00 = moderate drought
    newmatrix2[k,2] <- nm2[k,1]
    
    
  }
  
  newmatrix2[,1] <- as.numeric(newmatrix2[,1])
  newmatrix2[,2] <- as.numeric(newmatrix2[,2])
  colnames(newmatrix2) <- c(paste(h, "_days", sep=""), "FIPS")
  
  
  assign(paste("pdsi_moderate_drought_", h, sep=""), data.frame(newmatrix2))
  
}

pdsi_moderate_drought_2007_2012 <- cbind(pdsi_moderate_drought_2007[1], pdsi_moderate_drought_2008[1], pdsi_moderate_drought_2009[1], pdsi_moderate_drought_2010[1], pdsi_moderate_drought_2011[1], pdsi_moderate_drought_2012)
colnames(pdsi_moderate_drought_2007_2012) <- c("2007", "2008", "2009", "2010", "2011", "2012", "FIPS")

pdsioutput <- as.data.frame(pdsi_moderate_drought_2007_2012)

pdsioutput$`2007` <- as.numeric(pdsioutput[,1])
pdsioutput$`2008` <- as.numeric(pdsioutput[,2])
pdsioutput$`2009` <- as.numeric(pdsioutput[,3])
pdsioutput$`2010` <- as.numeric(pdsioutput[,4])
pdsioutput$`2011` <- as.numeric(pdsioutput[,5])
pdsioutput$`2012` <- as.numeric(pdsioutput[,6])


pdsioutput$TOTALS <-apply(pdsioutput[,1:6],1,sum)



write.csv(pdsioutput, file = "/dmine/data/soilses/data/PDSI_totals_2007_2012.csv")
