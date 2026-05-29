######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Load environmental data

###### Environmental and administrative data for the country (+mdata)
############ High resolution land cover rasters
GRA <- rast(paste0(ENVIPATH,"RastersLuxHighestResolution/LUX_GRA_10m.grd"))


                                    ### Adding new variables ###

#Crop type
CROP <- rast("C:/crop_type")
#Checking extent
crs(CROP)
crs(CROP) <- "EPSG:3035"
ext(CROP)
CROP_lux <- project(CROP, "EPSG:2169", method = "near") #Projecting to LUREF
ext(CROP_lux)
plot(CROP_lux)

# Imperviousness density
IMPREV <- rast("C:/imprev")
#Checking extent
crs(IMPREV)
crs(IMPREV) <- "EPSG:3035"
ext(IMPREV)
IMPREV_lux <- project(IMPREV, "EPSG:2169", method = "near") #Projecting to LUREF
ext(IMPREV_lux)
crs(IMPREV_lux)
plot(IMPREV_lux)

# Tree cover density
FOR <- rast("C:/treecoverdens")
#Checking extent
crs(FOR)
crs(FOR) <- "EPSG:3035"
ext(FOR)
FOR_lux <- project(FOR, "EPSG:2169", method = "near") #Projecting to LUREF
ext(FOR_lux)
crs(FOR_lux)
plot(FOR_lux)

#Forests polygon
FOR2 <- vect("C:/All_forests_diss.shp")
plot(FOR2)
ext(FOR2)
crs(FOR2)
FOR2_r <- rasterize(FOR2, #Rasterizing Forest polygon
  GRA, 
  cover=TRUE)
plot(FOR2_r)
summary(FOR2_r)                          

                                  #### Adding Luxembourg map - LUREF ###

Border <- st_read("C:/Lux_borders_prj.shp") #No buffer
plot(Border)
ext(Border)
crs(Border)


Border_5 <- vect("C:/Lux_borders_5km.shp") #5km buffer
plot(Border_5)
ext(Border_5)
crs(Border_5)

Border_10 <- vect("C:/Lux_borders_10km.shp") #10km buffer
plot(Border_10)
ext(Border_10)
crs(Border_10)

#Overlying Borders with Cropland
plot(CROP_lux)
plot(Border, add = TRUE)
CROP_masked <- mask(CROP_lux, Border) #Crop and mask Crop layer to Luxembourg borders
plot(CROP_masked)
?mask
CROP_masked
sum(is.na(vect(CROP_masked)))
sum(values(CROP_masked) == 0, na.rm = TRUE)

#Mask of Cropland at 10km buffer
Border_10km<-st_buffer(Border,dist = 10000)
?st_buffer
plot(Border_10km)
CROP_10km_mask <- mask(CROP_lux, Border_10km) #Crop and mask Crop layer to Luxembourg borders
plot(CROP_10km_mask)
sum(!is.na(values(CROP_10km_mask)))

#st_buffer######################
##### TESTING
######################
GRA <- aggregate(GRA, fact=10)

GRA <- rast("C:/grass_luerf")
#Checking extent
crs(GRA)
crs(GRA) <- "EPSG:2169"
ext(GRA)
plot(GRA)
GRA_10km_mask <- mask(GRA, Border_10km) #Crop and mask Crop layer to Luxembourg borders
plot(GRA_10km_mask)

#Rasters not perfectly aligned - need to align them
CROP_lux
GRA
IMPREV_lux
FOR_lux

#Crop layer binarization and resampling
CROP_lux[CROP_lux!=0,]<-1
CROP_lux<-as.numeric(CROP_lux)
plot(CROP_lux)
CROP_resamp<-resample(CROP_lux, GRA, method="mean", threads=TRUE, by_util=FALSE)
range(CROP_resamp) 
plot(CROP_lux)
plot(CROP_resamp)
hist(CROP_resamp)
hist(values(CROP_resamp))
CROP_10km_mask <- mask(CROP_resamp, Border_10km) #Crop and mask Crop layer to Luxembourg borders
plot(CROP_10km_mask)

#Impreviousness layer resampling
IMP_resamp<-resample(IMPREV_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
range(IMP_resamp) 
plot(IMP_resamp)
plot(CROP_resamp)
hist(IMP_resamp)
hist(values(CROP_resamp))

IMP_10km_mask <- mask(IMP_resamp, Border_10km) #Crop and mask IMP layer to Luxembourg borders
plot(IMP_10km_mask)
IMP_10km_mask
CROP_10km_mask

#Forest density layer resampling
FOR_resamp<-resample(FOR_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
range(IMP_resamp) 
plot(IMP_resamp)
plot(CROP_resamp)
hist(IMP_resamp)
hist(values(CROP_resamp))

FOR_10km_mask <- mask(FOR_resamp, Border_10km) #Crop and mask IMP layer to Luxembourg borders
plot(FOR_10km_mask)

###SOIL####
#Organic carbon (%) 2012-2014 10m
Soil <- rast("C:/soil/soil.tif")
#Checking extent
crs(Soil)
Soil_lux <- project(Soil, "EPSG:2169", method = "near") #Projecting to LUREF
plot(Soil_lux)
Soil_organic_resamp<-resample(Soil_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(Soil_organic_resamp)

#Ph 2009-2014 10m
Soil_ph <- rast("C:/soil/ph.tif")
Soil_ph_lux <- project(Soil_ph, "EPSG:2169", method = "near") #Projecting to LUREF
plot(Soil_lux)
Soil_ph_resamp<-resample(Soil_ph_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(Soil_ph_resamp)

#Moisture (m3/m3) at 100m from 2023, measured at 5-10cm depth
Soil_moisture<-rast("C:/soil/moisture.tif")
Soil_moisture_lux <- project(Soil_moisture, "EPSG:2169", method = "near") #Projecting to LUREF
plot(Soil_moisture_lux)
Soil_moisture_resamp<-resample(Soil_moisture_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(Soil_moisture_resamp)

###Resampled variables###
GRA_10km_mask
IMP_10km_mask
CROP_10km_mask
FOR_10km_mask
Elev_10km_mask #?
Aspect_southness_10km_mask #Aspect values are circular, so I converted them into a measure of "southness"
Spring_temp_10km_mask #?
spring_tempmin_10km_mask #?
Summer_temp_10km_mask #?
summer_tempmax_10km_mask
Spring_precip_10km_mask
Summer_precip_10km_mask #?
SR_spring_10km_mask
Slope_10km_mask
Soil_organic_resamp
Soil_ph_resamp
Soil_moisture_resamp

#Saving rasters and calling
writeRaster(IMP_10km_mask, "IMP_10km_mask.tif", overwrite=TRUE)
writeRaster(GRA_10km_mask, "GRA_10km_mask.tif", overwrite=TRUE)
writeRaster(CROP_10km_mask, "CROP_10km_mask.tif", overwrite=TRUE)
writeRaster(FOR_10km_mask, "FOR_10km_mask.tif", overwrite=TRUE)
writeRaster(Elev_10km_mask, "Elev_10km_mask.tif", overwrite=TRUE)
writeRaster(Aspect_southness_10km_mask, "Aspect_southness_10km_mask.tif", overwrite=TRUE)
writeRaster(summer_tempmax_10km_mask, "summer_tempmax_10km_mask.tif", overwrite=TRUE)
writeRaster(Spring_precip_10km_mask, "Spring_precip_10km_mask.tif", overwrite=TRUE)
writeRaster(SR_spring_10km_mask, "SR_spring_10km_mask.tif", overwrite=TRUE)
writeRaster(Slope_10km_mask, "Slope_10km_mask.tif", overwrite=TRUE)
writeRaster(Soil_organic_resamp, "Soil_organic_resamp.tif", overwrite=TRUE)
writeRaster(Soil_ph_resamp, "Soil_ph_resamp.tif", overwrite=TRUE)
writeRaster(Soil_moisture_resamp, "Soil_moisture_resamp.tif", overwrite=TRUE)
IMP_10km_mask <- rast("IMP_10km_mask.tif") # Use this code to call the rasters

save.image("2_EnvironmentalData.R.RData")
load.image("2_EnvironmentalData.R.RData")

############ Protected areas
source("ProtectedAreas.R")
PRO_n2000 <- prot.areas[prot.areas$type%in%c("Habitats Dir.", "Birds Dir."),]
PRO_zpin <- prot.areas[prot.areas$type=="National",]
############ Biotopes
source("Biotopes.R")
BTP <- dry
