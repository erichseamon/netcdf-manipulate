## netCDF

library(chron)
library(RColorBrewer)
library(lattice)

# check and reset working directory if necessary
getwd()
workdir <- "X:/Dropbox/ES Research/ES Classwork/FOR504/data/"
setwd(workdir)

# read a netCDF file using the ncdf package 
# the file is assumed to be a CF-compliant "classic" file

library(ncdf)

# CRU CL 2.0 data on 0.5 degree grid
ncname <- "_agg_met_2007_WUSA"  
ncfname <- paste(ncname,".nc", sep="")
dname <- "day"  # tmp means temperature (not temporary)

# read the netCDF file using the ncdf package

# open a netCDF file
ncin <- open.ncdf(ncfname) 

# print some basic informaition
print(ncin)

# a more verbose description
# str(ncin)

# get longitudes and latitudes
lat <- get.var.ncdf(ncin,"lat")
nlat <- dim(lat)
head(lat)
lon <- get.var.ncdf(ncin,"lon", verbose=F)
nlon <- dim(lon)
head(lon)

print(c(nlon,nlat)) # confirms the dimensions of the data

# get time 
t <- get.var.ncdf(ncin,"day")
tunits <- att.get.ncdf(ncin,"day","units")
t
nt <- dim(t)
nt
tunits$value

# now get the data and attributes
tmp.array <- get.var.ncdf(ncin,dname)
dlname <- att.get.ncdf(ncin,dname,"long_name")
dunits <- att.get.ncdf(ncin,dname,"units")
fillvalue <- att.get.ncdf(ncin,dname,"_FillValue")
dim(tmp.array)

# get global attributes
title <- att.get.ncdf(ncin,0,"title")
institution <- att.get.ncdf(ncin,0,"institution")
datasource <- att.get.ncdf(ncin,0,"source")
references <- att.get.ncdf(ncin,0,"references")
history <- att.get.ncdf(ncin,0,"history")
Conventions <- att.get.ncdf(ncin,0,"Conventions")

# done with the netCDF file, so close it
close.ncdf(ncin)

# read a netCDF file using the ncdf4 package

library(ncdf4)

# CRU CL 2.0 data on 0.5 degree grid
ncname <- "_agg_met_2007_WUSA"  
ncfname <- paste(ncname,".nc", sep="")
dname <- "day"  # tmp means temperature (not temporary)

# read the netCDF file using the ncdf package

# open a netCDF file
ncin <- nc_open(ncfname)

# print some basic informaition
# (like the output of ncdump)
print(ncin)

# a more verbose description
# str(ncin)

# get longitudes and latitudes

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
lon <- ncvar_get(ncin,"lon",verbose=F)
nlon <- dim(lon)
head(lon)


print(c(nlat,nlon))

# get time 
t <- ncvar_get(ncin,"day")
tunits <- ncatt_get(ncin,"day","units")
t
nt <- dim(t)
nt
tunits$value

# now get the data and attributes
tmp.array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp.array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# done with the netCDF file, so close it
nc_close(ncin)


## do various things to a netCDF file -- CRU tmp data

#show "real" times
#split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth=as.integer(unlist(tdstr)[2])
tday=as.integer(unlist(tdstr)[3])
tyear=as.integer(unlist(tdstr)[1])
chron(t,origin=c(tmonth, tday, tyear))

# replace netCDF fillvalues with R NAs
head(tmp.array)
tmp.array[tmp.array==fillvalue$value] <- NA
head(tmp.array)


# get a time slice of the data, and write a .csv file
m <- 1
tmp.slice <- tmp.array[,,m]
dim(tmp.slice)

# plot the slice
# quick-and-dirty image() plot
image(lon,lat,tmp.slice, col=rev(brewer.pal(10,"RdBu")))

# better levelplot
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp.slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# create a data frame from that slice
lonlat <- expand.grid(lon,lat)
dim(lonlat)
tmp.vec <- as.vector(tmp.slice)
length(tmp.vec)
tmp.df01 <- data.frame(cbind(lonlat,tmp.vec))
dim(tmp.df01)
names(tmp.df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp.df01), 20)

# write the data frame out as a .csv file, dropping NAs
csvfile <- "cru_tmp_1.csv"
write.table(na.omit(tmp.df01),csvfile, row.names=FALSE, sep=",")


# convert the whole array set to a second data frame, and calculate MTWA, MTCO,
# and the annual mean

# convert the nlon by nlat by nt array into a nlon*lat by nt matrix
tmp.vec.long <- as.vector(tmp.array)
length(tmp.vec.long)
tmp.mat <- matrix(tmp.vec.long, nrow=nlon*nlat, ncol=nt)
dim(tmp.mat)
head(na.omit(tmp.mat))

# create a data frame from the matrix
lonlat <- expand.grid(lon,lat)
dim(lonlat)
tmp.df02 <- data.frame(cbind(lonlat,tmp.mat))
dim(tmp.df02)
names(tmp.df02) <- c("lon","lat","tmpJan","tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                     "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp.df02, 20))


# get annual mean, mtwa and mtco and add to the second data frame

# get mtwa
tmp.df02$mtwa <- apply(tmp.df02[3:14],1,max)
length(tmp.df02$mtwa)

# get mtco
tmp.df02$mtco <- apply(tmp.df02[3:14],1,min)
length(tmp.df02$mtco)

# get row means
tmp.df02$mat <- apply(tmp.df02[3:14],1,mean)
length(tmp.df02$mat)


# list the first part of the data frame
head(tmp.df02)
dim(tmp.df02)

# and again, without missing values
head(na.omit(tmp.df02))
dim(na.omit(tmp.df02))

# write the second data fraome out as a .csv file, dropping NAs
csvfile <- "cru_tmp_2.csv"
write.table(na.omit(tmp.df02),csvfile, row.names=FALSE, sep=",")


# create a third data frame, with only non-missing values
tmp.df03 <- na.omit(tmp.df02)
head(tmp.df03)
dim(tmp.df03)

# alternarive listing of the first five lines
tmp.df03[1:5,1:17]


# create some arrays to write out

# reshape tmp.df02 data frame columns to arrays

# copy lon, lat and time from the initial netCDF data set
lon2 <- lon
lat2 <- lat
t2 <- t
tunits2 <- tunits
nlon2 <- nlon; nlat2 <- nlat; nt2 <- nt

# convert tmp.df02 back into an array
# in practice this array has to be structured correctly, 
# lons varying most rapidly, then lats, (then z's) then times
# this example shows how to extract the matrix from the data frame
tmp.mat2 <- as.matrix(tmp.df02[3:(3+nt-1)])
dim(tmp.mat2)
tmp.array2 <- array(tmp.mat2, dim=c(nlon2,nlat2,nt))
dim(tmp.array2)

# convert mtwa, mtco and mat to arrays
mtwa.array2 <- array(tmp.df02$mtwa, dim=c(nlon2,nlat2))
dim(mtwa.array2)
mtco.array2 <- array(tmp.df02$mtco, dim=c(nlon2,nlat2))
dim(mtco.array2)
mat.array2 <- array(tmp.df02$mat, dim=c(nlon2,nlat2))
dim(mat.array2)

# some plots to check creation of arrays
levelplot(tmp.array2[,,1] ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
levelplot(mtwa.array2 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
levelplot(mtco.array2 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
levelplot(mat.array2 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# create another set of arrays, from the tmp.df03  data frame
# create arrays, then loop over the rows of the data frame

# copy lon, lat and time from initial netCDF data set
lon3 <- lon
lat3 <- lat
t3 <- t
tunits3 <- tunits
nlon3 <- nlon; nlat3 <- nlat; nt3 <- nt

# make an nlon * nlat * nt array, and fill with the original fill value
tmp.array3 <- array(fillvalue$value, dim=c(nlon3,nlat3,nt3))
# nlon * nlat arrays for mtwa, mtco and mat
mtwa.array3 <- array(fillvalue$value, dim=c(nlon3,nlat3))
mtco.array3 <- array(fillvalue$value, dim=c(nlon3,nlat3))
mat.array3 <- array(fillvalue$value, dim=c(nlon3,nlat3))

# get number of observations (rows) in tmp.df03
nobs <- dim(tmp.df03)[1]
nobs

# three ways to convert the (short) data frame into arrays:

# loop over the rows in the data frame 
# most explicit, but takes a VERY LONG TIME
for(i in 1:nobs) {
  
  # figure out location in arrays of data in each row of the data frame
  j <- which.min(abs(lon3-tmp.df03$lon[i]))
  k <- which.min(abs(lat3-tmp.df03$lat[i]))
  
  # copy data from the data frame to array
  tmp.array3[j,k,1:nt] <- as.matrix(tmp.df03[i,3:(nt+3-1)])
  mtwa.array3[j,k] <- tmp.df03$mtwa[i]
  mtco.array3[j,k] <- tmp.df03$mtco[i]
  mat.array3[j,k] <- tmp.df03$mat[i]
}

# loop-avoidance approaches 
# get vectors of the grid-cell indices for each row in the data frame
j2 <- match(tmp.df03$lon,lon3)
k2 <- match(tmp.df03$lat,lat3)
head(cbind(j2,k2))

# partial loop avoidance for tmp.array3
temp.array <- array(fillvalue$value, dim=c(nlon3,nlat3))
for (l in 1:nt) {
  temp.array[cbind(j2,k2)] <- as.matrix(tmp.df03[1:nobs,l+2]) 
  tmp.array3[,,l] <- temp.array
}

# can copy 2-d arrays directly
mtwa.array3[cbind(j2,k2)] <- as.matrix(tmp.df03$mtwa)
mtco.array3[cbind(j2,k2)] <- array(tmp.df03$mtco) 
mat.array3[cbind(j2,k2)] <- array(tmp.df03$mat) 


# loop avoidance for all of the variables
l <- rep(1:nt3,each=nobs)
tmp.array3[cbind(j2,k2,l)] <- as.matrix(tmp.df03[1:nobs,3:(nt+2)])
mtwa.array3[cbind(j2,k2)] <- as.matrix(tmp.df03$mtwa) 
mtco.array3[cbind(j2,k2)] <- array(tmp.df03$mtco) 
mat.array3[cbind(j2,k2)] <- array(tmp.df03$mat) 


# some plots to check creation of arrays
m <- 7
levelplot(tmp.array3[,,m] ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
levelplot(mtwa.array3 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
levelplot(mtco.array3 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
levelplot(mat.array3 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))



# create and write the netCDF file
# ncdf version

# define dimensions
londim <- dim.def.ncdf("lon","degrees_east",as.double(lon3)) 
latdim <- dim.def.ncdf("lat","degrees_north",as.double(lat3)) 
timedim <- dim.def.ncdf("time",tunits3$value,as.double(t3))

# define variables
dlname <- "2m air temperature"
tmp.def <- var.def.ncdf("tmp","deg_C",list(londim,latdim,timedim),fillvalue$value,dlname,prec="single")
dlname <- "mean_temperture_warmest_month"
mtwa.def <- var.def.ncdf("mtwa","deg_C",list(londim,latdim),fillvalue$value,dlname,prec="single")
dlname <- "mean_temperature_coldest_month"
mtco.def <- var.def.ncdf("mtco","deg_C",list(londim,latdim),fillvalue$value,dlname,prec="single")
dlname <- "mean_annual_temperature"
mat.def <- var.def.ncdf("mat","deg_C",list(londim,latdim),fillvalue$value,dlname,prec="single")

# create netCDF file and put arrays
ncfname <- "cru10min30_new.nc"
ncout <- create.ncdf(ncfname,list(tmp.def,mtco.def,mtwa.def,mat.def))

# put variables
put.var.ncdf(ncout,tmp.def,tmp.array3)
put.var.ncdf(ncout,mtwa.def,mtwa.array3)
put.var.ncdf(ncout,mtco.def,mtco.array3)
put.var.ncdf(ncout,mat.def,mat.array3)

# put additional attributes into dimension and data variables
att.put.ncdf(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
att.put.ncdf(ncout,"lat","axis","Y")
att.put.ncdf(ncout,"time","axis","T")

# add global attributes
att.put.ncdf(ncout,0,"title",title$value)
att.put.ncdf(ncout,0,"institution",institution$value)
att.put.ncdf(ncout,0,"source",datasource$value)
att.put.ncdf(ncout,0,"references",references$value)
history <- "Bartlein 08 Apr 2013"
att.put.ncdf(ncout,0,"history",history)
att.put.ncdf(ncout,0,"Conventions",Conventions$value)

# close the file, writing data to disk
close.ncdf(ncout)


# create and write the netCDF file
# ncdf4 version

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon3)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat3)) 
timedim <- ncdim_def("time",tunits3$value,as.double(t3))

# define variables
dlname <- "2m air temperature"
tmp.def <- ncvar_def("tmp","deg_C",list(londim,latdim,timedim),fillvalue$value,dlname,prec="single")
dlname <- "mean_temperture_warmest_month"
mtwa.def <- ncvar_def("mtwa","deg_C",list(londim,latdim),fillvalue$value,dlname,prec="single")
dlname <- "mean_temperature_coldest_month"
mtco.def <- ncvar_def("mtco","deg_C",list(londim,latdim),fillvalue$value,dlname,prec="single")
dlname <- "mean_annual_temperature"
mat.def <- ncvar_def("mat","deg_C",list(londim,latdim),fillvalue$value,dlname,prec="single")

# create netCDF file and put arrays
ncfname <- "cru10min30_new4.nc"
ncout <- nc_create(ncfname,list(tmp.def,mtco.def,mtwa.def,mat.def),force_v4=T)

# put variables
ncvar_put(ncout,tmp.def,tmp.array3)
ncvar_put(ncout,mtwa.def,mtwa.array3)
ncvar_put(ncout,mtco.def,mtco.array3)
ncvar_put(ncout,mat.def,mat.array3)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

# add global attributes
ncatt_put(ncout,0,"title",title$value)
ncatt_put(ncout,0,"institution",institution$value)
ncatt_put(ncout,0,"source",datasource$value)
ncatt_put(ncout,0,"references",references$value)
history <- "Bartlein 08 Apr 2013"
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"Conventions",Conventions$value)

# close the file, writing data to disk
nc_close(ncout)