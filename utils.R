######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : utility functions

save.image("utils.R.RData") 
load("utils.R.RData")     

############ Resistance transformation ----
res.tran_JW <- function(x, k = 0.5, ymax = 1, shape) {
  # x    — vector of resource values
  # k    — half-saturation constant (value of x where f(x) = ymax/2) for S; unitless for A (1/k)
  # ymax — asymptote / maximum output (default 1)
  # shape — "S" for saturating or "A" for accelerating or "U" for
  # Normalise x to [0, 1] — handles any units and any starting value
  
  x_norm <- (x - unlist(global(x,fun="min",na.rm=T))) / (unlist(global(x,fun="max",na.rm=T)) - unlist(global(x,fun="min",na.rm=T)))
  if (shape == "S") {
    # Monod — concave, saturating
    # k: fraction of x-range where f(x) = ymax/2
    return(ymax * x_norm / (k + x_norm))
    
  } else if (shape == "A") {
    # Power function — purely convex, accelerating throughout
    # exponent n = 1/k: smaller k → steeper acceleration
    # k=0.5 → n=2 (quadratic), k=0.3 → n=3.3, k=0.2 → n=5
    n <- 1 / k
    return(ymax * x_norm^n)
    
  } else if (shape == "U") {
    # k = minimum value (at centre of x range)
    # ymax = maximum value (at both edges)
    return(k + (ymax - k) * (2 * (x_norm - 0.5))^2)
    
  } else {
    stop(paste0("Unknown shape '", shape, "'. Use 'S' or 'A'."))
  }
}



###### Test
xlol <- seq(0, 100, 1)
xlol
# Saturating
lol <- res.tran_JW(x=xlol, k=25, ymax=10,  shape="S")
plot(xlol,lol,xlab="Tree cover density (%)", ylab="Resistance",
     type="l",col="red", lwd="2", ylim=c(0,8))
hist(lol)
range(lol)

# Accelerating
lol2 <- res.tran_JW(x=xlol, k=0.25, ymax=10, shape="A")
plot((xlol - min(xlol))/(max(xlol) - min(xlol)),lol2)

# U-shaped
lol3 <- res.tran_JW(x=xlol, k=5, ymax=10, shape="U")
plot((xlol - min(xlol))/(max(xlol) - min(xlol)),lol3)

#Applying functions to rasters#
#Imperviousness
IMP.function <- function(x) {res.tran_JW(x = x, k = 0.10, ymax = 10, shape = "S")} 
plot(IMP.function)
IMP.res <- app(IMP_10km_mask,IMP.function)
plot(IMP.res)
writeRaster(IMP.res, "IMP.res.tif", overwrite=TRUE)# Saving raster

#Forest density
FOR.function <- function(x) {res.tran_JW(x = x, k = 25, ymax = 10, shape = "S")} #k=25 and ymax=10, this gives as a maximum of 8
plot(FOR.function)
FOR.res <- app(FOR_10km_mask,FOR.function)
plot(FOR.res)
writeRaster(FOR.res, "FOR.res.tif", overwrite=TRUE)# Saving raster
range(FOR.)

#Grasslands


#Croplands
#Times values by 5
CROP.res<-CROP_10km_mask*5
plot(CROP.res)

#Aspect
ASP.function <- function(x) {res.tran_JW(x = x, k = 0.5, ymax = 10, shape = "S")}
plot(ASP.function)
ASP.res <- (Aspect_southness_10km_mask*(-1)+1)*4.5+1 #Have to turn raster values from -1 - 1 to 1 - 10
plot(ASP.res)
writeRaster(ASP.res, "ASP.res.tif", overwrite=TRUE)# Saving raster

#Average summer maximum
SMax.function<- function(x) {res.tran_JW(x=x, k=1, ymax=8, shape="A")}
plot(SMax.function)
SMax.res <- app(summer_tempmax_10km_mask,SMax.function)
plot(SMax.res)
writeRaster(SMax.res, "SMax.res.tif", overwrite=TRUE)# Saving raster
range(summer_tempmax_10km_mask)
plot(summer_tempmax_10km_mask)
SMax.res
(IMP_10km_mask-min(IMP_10km_mask))/(max(IMP_10km_mask)-min(IMP_10km_mask))
x<-IMP_10km_mask
x_norm <- (x - unlist(global(x,fun="min",na.rm=T))) / (unlist(global(x,fun="max",na.rm=T)) - unlist(global(x,fun="min",na.rm=T)))
hist(x_norm)
plot(x_norm)
10*x_norm/(0.1+x_norm)


#PAs
#Not sure how to yet, for binary rasters