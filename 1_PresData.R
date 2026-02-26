######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Load environmental data


############ Loading and checking presence data ----
###### Raw
dry_raw <- read.csv(paste0("./PROJDATA/dryland.csv")) # OLD WE MUST CHANGE IT

### Checkpoint
dim(dry_raw)
str(dry_raw)
View(dry_raw)

### Are we happy with this species list?
sort(table(dry_raw$preferred), decreasing=TRUE)

### Date check
table(as.Date(dry_raw$date_end))

###### Keep essentials
dry_e <- dry_raw[,c("preferred", "date_end", "Lat", "Long")]

###### Make sf spatial points object
dry_sf <- st_as_sf(dry_e, coords=c("Long", "Lat"), crs="EPSG:4326")
dry_sf <- st_transform(dry_sf, crs="EPSG:2169")
### Checkpoint
ggplot() + geom_sf(data=dry_sf)
