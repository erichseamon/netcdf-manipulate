#------------------------------------------------------------------------#
# TITLE:        netcdf_access.R
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         August 12, 2015
#
# STAGE:        netcdf access
#
# COMMENTS:     This script opens and displays netcdf data.  
#
#--Setting the working directory an d clearing the workspace-----------#

#library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
library("raster")
library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
library("Evapotranspiration")
library("plyr")
library("data.table")
library("sirad")
library("rgdal")


#netcdf_access(climatevar, state )

state <- "Washington"
scen_state = paste(state,sep="", collapse="|")
setwd("/dmine/data/counties")
counties <- readOGR(dsn = "/dmine/data/counties/", layer = "UScounties")
counties <- counties[grep(scen_state, counties@data$STATE_NAME),]



climatevar_short <- "tmmn"
climatevar <- "daily_minimum_temperature"
http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/tmmn/tmmn_2017.nc
mintemp <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", climatevar_short, "_1979_CurrentYear_CONUS.nc?lon[0:1:1385],lat[0:1:584],", climatevar, "[0:1:0][0:1:0][0:1:0],day[0:1:0]", sep=""))

#maxtemp <- nc_open('http://reacchpna.org/thredds/dodsC/agg_met_tmmx_1979_2014_WUSA.nc?lon[0:1:1385],lat[0:1:584],daily_maximum_temperature[0:1:0][0:1:0][0:1:0],day[0:1:10]')# Open a netcdf file 

v3 <- mintemp$var[[1]]
lonsize <- v3$varsize[1]
latsize <- v3$varsize[2] 
endcount <- v3$varsize[3] 

#get nearest index values from loaded netcdf, from lat lon
wherenearest <- function(myPoint, allPoints){
  d <- abs(allPoints-myPoint[1])
  index <- which.min(d)
  return( index )}

#starting lat and lon
lon <- wherenearest(-125, mintemp$dim$lon$vals)
lat <- wherenearest(52, mintemp$dim$lat$vals)
mintemp[,3] = as.POSIXct(strptime(mintemp[,3],format = "%b %d %Y %I:%M%p",
                                  tz = "PDT"))


mintemp_var <- ncvar_get(mintemp, climatevar, start=c(1,1,500),count=c(-1,-1,1))


mintemp_var <- ncvar_get(mintemp, climatevar, start=c(1,1,1), count=c(1,1,-1)) # extract just the variable form the nc file

mintempraster <- (raster(mintemp_var)) # turn the file into a raster

xtent <- extent(25.1562, 52.8438, -124.5938, -67.0312) # set the exent of the raster
extent(mintempraster) <- xtent
crs(mintempraster) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" # set the projection of the raster

mintempraster <- setExtent(mintempraster, xtent, keepres=TRUE)


plot(mintempraster) 

hist(mintempraster_transposed)

