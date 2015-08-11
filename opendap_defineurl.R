
rm(list = ls()) #--clears all lists
setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data/")   #----set the working directory

url_grid <-
  "https://www.reacchpna.org/reacchspace/obj1/netcdf/MET/tmmx/tmmx_2007.nc" # note: netcdf4 does not work on windows R

url_time <-
  "https://www.reacchpna.org/reacchspace/obj1/netcdf/MET/tmmx/tmin_2007.nc"

download.file(url_grid, "vaklodingenKB116_4544.nc", method = "auto",
              quiet = FALSE, mode="wb", cacheOK = TRUE)

download.file(url_time, "id410-DELFZBTHVN.nc", method = "auto",
              quiet = FALSE, mode="wb", cacheOK = TRUE)
