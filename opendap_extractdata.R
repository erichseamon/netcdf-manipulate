grid.nc <- open.ncdf("vaklodingenKB116_4544.nc")

# look what's in there...
grid.nc

# Get grid data
G.x <- get.var.ncdf(grid.nc,'x')
G.y <- get.var.ncdf(grid.nc,'y')

# get only first timestep
G.z <- get.var.ncdf(grid.nc,'z')[,,1]

# to get a black background, and set the scale of depth values to start from 0.
G.z[G.z == -9999] <- 0

# image.plot needs sorted x- and y-values;
# as y-values are descending, the order is reversed here...
G.y <- rev(G.y)
G.z <- G.z[,length(G.y):1]

time.nc <- open.ncdf("id410-DELFZBTHVN.nc")
# look what's in there...
time.nc

T.t <- get.var.ncdf(time.nc,'time')
T.eta <-
  get.var.ncdf(time.nc,'concentration_of_suspended_matter_in_sea_water')