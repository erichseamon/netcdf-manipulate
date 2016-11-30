

#--
library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
counties <- counties[grep("Latah", counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

#newj <- 1
#for (j in climz) {
  newx <- 1
 for (i in monthz) {
 setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/")
 ncfile = paste("pr", "_", i, "_", "2007", ".nc", sep="")
 rasterout <- brick(ncfile)
 rasterout <- crop(rasterout, counties)
 vect <- cellStats(rasterout, mean)
 vect2 <- mean(vect)
 newmat[newx] <- vect2
 newx <- newx + 1
# }
 #newj <- newj + 1
}
colnames(newmat) <- "pr"
rownames(newmat) <- monthz
newmat <- data.frame(newmat)

#---

i <- paste("2007", "_monthly_usda_gridmet_post2001_", "Idaho", sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/summaries/", sep="")

setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- data.table(x)

DT2 <- subset(DT, county == "Latah")
DT2 <- subset(DT2, commodity == "WHEAT")

newmat2 <- matrix(, ncol = 1, nrow = 12)
#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1

#--loss
 newii <- 1
 for (ii in monthzz) {
   nez <- subset(DT2, monthcode == ii)
   newmat2[ii, newjj] <- sum(nez$loss)
   newii <- newii + 1
 }

newmat3 <- matrix(, ncol = 1, nrow = 12)
 
#--acres

  newii <- 1
  for (ii in monthzz) {
    nez <- subset(DT2, monthcode == ii)
    newmat3[ii, newjj] <- sum(nez[,31])
    newii <- newii + 1
  }

newmat4 <- cbind(newmat2, newmat3)
colnames(newmat4) <- c("loss", "acres")

newmat5 <- cbind(newmat, newmat4)

newmat5[is.na(newmat5)] <- 0

#library(dygraphs)
#library(xts)
#test <- cbind(newmat5$loss)
#dygraph(data.frame(test))

#newmat6 <- newmat5
#rownames(newmat6) -> data.frame(newmat7)
#test <- cbind(newmat7, newmat5$pr)
#as.xts(newmat5$pr, newmat7)

xxx <- min(newmat$pr)
xxxx <- max(newmat$pr)
interval <- (xxxx-xxx)/5



par(mar=c(4,4,4,5))
par(mfrow = c(2, 1))                 
barplot(newmat5$loss, names.arg = rownames(newmat5), las=2, col="blue", main = paste("Idaho", " crop loss $ \n", "Feb", " ", "2001", "\n", "Wheat", sep=""))
plot(newmat5$pr, axes=FALSE, xlab = "months", ylab = "pr", main = paste("Idaho", " precipitation \n", "Feb", " ", "2001", "\n", sep=""))
axis(side=1, at=c(1:12))
axis(side=2, at=seq(xxx, xxxx, by = interval))
lines(newmat5$pr, las=2, col="blue")






    
  