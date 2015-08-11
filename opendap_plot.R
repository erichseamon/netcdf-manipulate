# R-package fields provides nice image facilities and color schemes
par (mfrow = c(1,2))
library(fields)
image.plot(G.x,G.y,as.matrix(G.z),
           col = c(tim.colors(),"black"),
           xlab = "x [m]", ylab = "y [m]")
plot(as.Date(T.t, origin="1970-01-01"), T.eta, type = "l", ylab = "spm
[mg/l]")