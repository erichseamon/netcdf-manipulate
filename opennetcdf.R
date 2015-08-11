##Constructing dataframe from netcdf file in R

#Open ncdf4(only works on 32-bit)
library(ncdf4)

#Download netcdf file
url_grid<-"http://reacchpna.org/reacchspace/obj1/netcdf/MET/pdur/pdur_2012.nc"
download.file(url_grid, "pdur_2012.nc", method = "auto",quiet = FALSE, mode="wb", cacheOK = TRUE)

#Open file and check variables
ncin<-nc_open("pdur_2012.nc")
print(ncin)

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

t<-ncvar_get(ncin,"time")
time<-as.vector(t)

altitude<-ncvar_get(ncin,"z")
dim(altitude)

#Select part of the variable of interest, here altitude
m <- 1
altitude.slice <- altitude[, , m]
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