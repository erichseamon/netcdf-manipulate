


qss <- function (files,...)
{
  ln <- nl <- bands.vec <- c()
  for(p in 1:length(files)){
    b <- brick(files[p],...)
    ln <- c(ln, b@layernames)
    nl <- c(nl, nlayers(b))
    bands.vec <- c(bands.vec, 1:nlayers(b))
  }
  r <- raster(b,1)
  r@data@haveminmax = FALSE
  s <- stack(r)
  files.vec <- rep(files,times=nl)
  nbands.vec <- rep(nl,times=nl)
  s@layers <- sapply(1:sum(nl), function(i) {
    r@file@name <- files.vec[i]
    r@file@nbands <- nbands.vec[i]
    r@data@band <-  bands.vec[i]
    r@layernames <- ln[i]
    r
  })
  s@layernames <- ln
  s
}