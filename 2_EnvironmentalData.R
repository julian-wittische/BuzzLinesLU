######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Load environmental data

###### Environmental and administrative data for the country (+mdata)
############ High resolution land cover rasters
GRA <- rast(paste0(ENVIPATH,"RastersLuxHighestResolution/LUX_GRA_10m.grd"))

######################
##### TESTING
######################
GRA <- aggregate(GRA, fact=10)
############ Protected areas
source("ProtectedAreas.R")
PRO_n2000 <- prot.areas[prot.areas$type%in%c("Habitats Dir.", "Birds Dir."),]
PRO_zpin <- prot.areas[prot.areas$type=="National",]
############ Biotopes
source("Biotopes.R")
BTP <- dry
