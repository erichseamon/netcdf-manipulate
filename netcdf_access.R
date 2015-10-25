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

library("ncdf")
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

maxtemp <- nc_open('http://reacchpna.org/thredds/dodsC/agg_met_tmmx_1979_2015_WUSA.nc?lon[0:1:1385],lat[0:1:584],daily_maximum_temperature[0:1:0][0:1:0][0:1:0],day[0:1:10]')

#maxtemp <- nc_open('http://reacchpna.org/thredds/dodsC/agg_met_tmmx_1979_2014_WUSA.nc?lon[0:1:1385],lat[0:1:584],daily_maximum_temperature[0:1:0][0:1:0][0:1:0],day[0:1:10]')# Open a netcdf file 

maxtemp_var <- ncvar_get(maxtemp, 'daily_maximum_temperature', start=c(1,1,1), count=c(-1,-1,1)) # extract just the variable form the nc file

maxtempraster <- (raster(maxtemp_var)) # turn the file into a raster

xtent <- extent(25.1562, 52.8438, -124.5938, -67.0312) # set the exent of the raster
extent(maxtempraster) <- xtent
crs(maxtempraster) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" # set the projection of the raster

maxtempraster <- setExtent(maxtempraster, xtent, keepres=TRUE)

maxtempraster_transposed <- t(maxtempraster) # transpose the raster 

plot(maxtempraster_transposed) 

hist(maxtempraster_transposed)