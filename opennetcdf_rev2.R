##Constructing dataframe from netcdf file in R

#Open ncdf4(only works on 32-bit)
library(ncdf4)

#Download netcdf file
url_grid<-"http://thredds.northwestknowledge.net:8080/thredds/fileServer/MET/pr/pr_2016.nc"
download.file(url_grid, "pdur_2012.nc", method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE)

setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/")
#Open file and check variables
ncin <- brick("pr_mar_2007.nc")
plot(ncin)

#Get variables from netcdf file
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)
lon.a<-as.vector(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat)
lat.a<-as.vector(lat)

print(c(nlon,nlat))

t<-ncvar_get(ncin,"day")
time<-as.vector(t)

altitude<-ncvar_get(ncin, "precipit")
dim(altitude)

#Select part of the variable of interest, here altitude
m <- 1
altitude.slice <- precipitation_amount[, , m]
altitude.vec<-as.vector(altitude.slice)

#Combine into data frame
altitude.data<-data.frame(cbind(time,lon.a,lat.a,altitude))
names(altitude.data)<-c("time","lon","lat","altitude")
head(altitude.data)

#Altitude data contains missing values; delete those rows
altitude.data<-altitude.data[!is.na(altitude.data$altitude),]
head(altitude.data)

#Save as CSV
#write.csv(altitude.data,file="jarkusKB134_1110.csv",row.names=FALSE)




library(ncdf);
temp.nc <- open.ncdf("tmmx_feb_2007.nc");
temp <- get.var.ncdf(temp.nc,"air_temperature");

temp.nc$dim$lon$vals -> lon
temp.nc$dim$lat$vals -> lat

#lat <- rev(lat)
#temp <- temp[nrow(temp):1,]

#temp[temp==-32767] <- NA
temp <- t(temp)

image(lon,lat,temp)
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, add = TRUE)


library(raster) ## requires ncdf package for this file  
d <- raster("tmmx_feb_2007.nc", varname = "air_temperature")

--
  
  
library(ncdf)
f <- open.ncdf("tmmx_feb_2007.nc")
A <- get.var.ncdf(f,"air_temperature")  
f$dim$lon$vals -> Longitude
f$dim$lat$vals -> Latitude

image(Longitude, Latitude, A)
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, add = TRUE)










b <- brick("tmmx_feb_2007.nc", lvar=3, level=1, xmx=LAT2, xmn=LAT1)
b <- t(b)

#--
library(maptools)
monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

newmat <- matrix(,ncol = 15, nrow = 12 )
newx <- 1
newj <- 1
for (j in climz) {
 for (i in monthz) {
 setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/")
 ncfile = paste(climz, "_", i, "_", "2007", ".nc", sep="")
 rasterout <- brick(ncfile)

 setwd("/dmine/data/counties/")

 counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

 #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
 counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
 counties <- counties[grep("Latah", counties@data$NAME),]

 rasterout <- crop(rasterout, counties)

 vect <- cellStats(rasterout, mean)
 vect2 <- mean(vect)
 newmat[newx] <- vect2
 newx <- newx + 1
 }
 newj < newj + 1
}
  #---


library(chron)
library(lattice)
library(RColorBrewer)

# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth=as.integer(unlist(tdstr)[2])
tday=as.integer(unlist(tdstr)[3])
tyear=as.integer(unlist(tdstr)[1])
chron(t,origin=c(tmonth, tday, tyear))



