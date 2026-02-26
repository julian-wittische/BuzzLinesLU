######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Load biotope data and select most interesting subset

# Where is the open cadastre file?
zip_path <- file.path(ENVIPATH,"obk-situation-2024")
zip_path

# Choose surfaces without orchards (I think its F)
opencad <- read_sf(dsn=zip_path, layer="OBK_F")

### Only select dry habitats
# Subset by Axel codes
index <- which(opencad$E_Btyp1_co %in% c("BK01", "BK02", "BK03", "BK07",
                                         "4030",
                                         "5110",
                                         "5130",
                                         "6110",
                                         "6210",
                                         "8150",
                                         "8160",
                                         "8210",
                                         "8220",
                                         "8230"))

# Use index
dry <- opencad[index,]

