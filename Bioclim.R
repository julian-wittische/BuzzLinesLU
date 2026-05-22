                                          ###Biolclim Variables####
#Adding tiles of Land surface temperature 25m (LST)
#Spring mean
LST_march <- rast("C:/Temperature/march.tif")
LST_april <- rast("C:/Temperature/april.tif")
LST_may <- rast("C:/Temperature/may.tif")
LST_aprils_lux <- project(LST_april, "EPSG:2169", method = "near") #Projecting to LUREF
LST_may_lux <- project(LST_may, "EPSG:2169", method = "near")
LST_march_lux <- project(LST_march, "EPSG:2169", method = "near")
spring_stack <- c(LST_may_lux, LST_aprils_lux, LST_march_lux)  #Calculating mean temperature
spring_mean <- mean(spring_stack) #Overlying Borders with Mean temperature
spring_mean_masked <- mask(spring_mean, Border_10km)
Spring_temp_10km_mask<-resample(spring_mean_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE) #Mean temp. layer resampling
plot(Spring_temp_10km_mask)

#Summer mean
LST_june <- rast("C:/Temperature/june.tif")
LST_july <- rast("C:/Temperature/july.tif")
LST_august <- rast("C:/Temperature/august.tif")
LST_june_lux <- project(LST_june, "EPSG:2169", method = "near")
LST_july_lux <- project(LST_july, "EPSG:2169", method = "near")
LST_august_lux <- project(LST_august, "EPSG:2169", method = "near")
summer_stack <- c(LST_june_lux, LST_july_lux, LST_august_lux)
summer_mean <- mean(summer_stack)
summer_mean_masked <- mask(summer_mean, Border_10km)
Summer_temp_10km_mask<-resample(summer_mean_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(Summer_temp_10km_mask)


#10km buffer
Border_10km<-st_buffer(Border,dist = 10000)

#Precipitation 25m
#Spring mean
Precip_march <- rast("C:/Precipitation/march.tif")
Precip_april <- rast("C:/Precipitation/april.tif")
Precip_may <- rast("C:/Precipitation/may.tif")
Precip_march_lux <- project(Precip_march, "EPSG:2169", method = "near")
Precip_april_lux <- project(Precip_april, "EPSG:2169", method = "near")
Precip_may_lux <- project(Precip_may, "EPSG:2169", method = "near")
spring_precip_stack <- c(Precip_may_lux, Precip_april_lux, Precip_march_lux)
spring_precip_mean <- mean(spring_precip_stack)
Pring_precip_masked <- mask(spring_precip_mean, Border_10km)
Spring_precip_10km_mask<-resample(Pring_precip_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(Spring_precip_10km_mask)

#Summer mean
Precip_june <- rast("C:/Precipitation/june.tif")
Precip_july <- rast("C:/Precipitation/july.tif")
Precip_august <- rast("C:/Precipitation/august.tif")
Precip_june_lux <- project(Precip_june, "EPSG:2169", method = "near")
Precip_july_lux <- project(Precip_july, "EPSG:2169", method = "near")
Precip_august_lux <- project(Precip_august, "EPSG:2169", method = "near")
Summer_precip_stack <- c(Precip_june_lux, Precip_july_lux, Precip_august_lux)
Summer_precip_mean <- mean(Summer_precip_stack)
Summer_precip_masked <- mask(Summer_precip_mean, Border_10km)
Summer_precip_10km_mask<-resample(Summer_precip_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(Summer_precip_10km_mask)

#Spring min
Tempmin_march <- rast("C:/Temp_min/march.tif")
Tempmin_april <- rast("C:/Temp_min/april.tif")
Tempmin_may <- rast("C:/Temp_min/may.tif")
Tempmin_march_lux <- project(Tempmin_march, "EPSG:2169", method = "near")
Tempmin_april_lux <- project(Tempmin_april, "EPSG:2169", method = "near")
Tempmin_may_lux <- project(Tempmin_may, "EPSG:2169", method = "near")
spring_tempmin_stack <- c(Tempmin_march_lux, Tempmin_april_lux, Tempmin_may_lux)
spring_tempmin_mean <- mean(spring_tempmin_stack)
spring_tempmin_masked <- mask(spring_tempmin_mean, Border_10km)
spring_tempmin_10km_mask<-resample(spring_tempmin_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(spring_tempmin_10km_mask)

#Summer max
Tempmax_june <- rast("C:/Temp_max/june.tif")
Tempmax_july <- rast("C:/Temp_max/july.tif")
Tempmax_august <- rast("C:/Temp_max/august.tif")
Tempmax_june_lux <- project(Tempmax_june, "EPSG:2169", method = "near")
Tempmax_july_lux <- project(Tempmax_july, "EPSG:2169", method = "near")
Tempmax_august_lux <- project(Tempmax_august, "EPSG:2169", method = "near")
summer_tempmax_stack <- c(Tempmax_june_lux, Tempmax_july_lux, Tempmax_august_lux)
summer_tempmax_mean <- mean(summer_tempmax_stack)
summer_tempmax_masked <- mask(summer_tempmax_mean, Border_10km)
summer_tempmax_10km_mask<-resample(summer_tempmax_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(summer_tempmax_10km_mask)

#Mean solar radiation (kJ m-2 day-1) 25m
#Spring 
Spring_sol_march <- rast("C:/Solar_radiation/march.tif")
Spring_sol_april <- rast("C:/Solar_radiation/april.tif")
Spring_sol_may <- rast("C:/Solar_radiation/may.tif")
SR_march_lux <- project(Spring_sol_march, "EPSG:2169", method = "near")
SR_april_lux <- project(Spring_sol_april, "EPSG:2169", method = "near")
SR_may_lux <- project(Spring_sol_may, "EPSG:2169", method = "near")
SR_spring_stack <- c(SR_march_lux, SR_april_lux, SR_may_lux)
SP_spring_mean <- mean(SR_spring_stack)
SR_spring_masked <- mask(SP_spring_mean, Border_10km)
SR_spring_10km_mask<-resample(SR_spring_masked, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
plot(SR_spring_10km_mask)

###TOPOGRAPHY - Elevation (25m)###
Elev <- rast("C:/Topography/elevation.tif")
Elev_lux <- project(Elev, "EPSG:2169", method = "near") #Projecting to LUREF
Elev_resamp<-resample(Elev_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
Border_10km<-st_buffer(Border,dist = 10000)
Elev_10km_mask <- mask(Elev_resamp, Border_10km) #Crop and mask Elevation layer to Luxembourg borders
plot(Elev_10km_mask)

###TOPOGRAPHY - Aspect (25m)###
Aspect <- rast("C:/aspect")
crs(Aspect)<- "EPSG:2169"
Aspect_lux <- project(Elev, "EPSG:2169", method = "near") #Projecting to LUREF
Aspect_resamp<-resample(Aspect, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
Border_10km<-st_buffer(Border,dist = 10000)
Aspect_10km_mask <- mask(Aspect_resamp, Border_10km) #Crop and mask Elevation layer to Luxembourg borders
plot(Aspect_10km_mask)
Aspect_southness_10km_mask <- cos(Aspect_10km_mask - pi) #Aspect values are circular, so I converted them into a measure of "southness"
plot(southness)
southness

###TOPOGRAPHY - Slope (25m) (Degrees)###
Slope <- rast("C:/Slope")
crs(Aspect)<- "EPSG:2169"
Slope_lux <- project(Slope, "EPSG:2169", method = "near")
Slope_resamp<-resample(Slope_lux, GRA, method="bilinear", threads=TRUE, by_util=FALSE)
Border_10km<-st_buffer(Border,dist = 10000)
Slope_10km_mask <- mask(Slope_resamp, Border_10km)
plot(Slope_10km_mask)
