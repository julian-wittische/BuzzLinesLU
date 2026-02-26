######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Rasterize vector data (relevant biotopes, protected areas)

############ Fractional cover and rule

###### RULE 1: proportional
PRO_n2000_r <- rasterize(
  vect(PRO_n2000), # polygon layer to rasterize
  GRA, # base raster layer 
  cover=TRUE
)

PRO_zpin_r <- rasterize(
  vect(PRO_zpin), # polygon layer to rasterize
  GRA, # base raster layer 
  cover=TRUE
)

BTP_r <- rasterize(
    vect(BTP), # polygon layer to rasterize
    GRA, # base raster layer 
    cover=TRUE
)
  
