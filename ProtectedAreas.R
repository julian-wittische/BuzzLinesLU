######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Spring 2026
# Data: MNHNL
# Script objective : Load, correctc combine protected area data from Luxembourg

############ Loading protected area data ----
####### Encoding option for sf
enc.opt <- "ENCODING=latin1"
enc.opt <- "ENCODING=UTF8"

enc.opt <- "ENCODING=Windows1242"
enc.opt.z <- "ENCODING=latin1"

###### Load Natura 2000 data
ludh <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"ludh-20231006.zip"), options = enc.opt)
ludo <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"ludo-20231006.zip"), options = enc.opt)

ludh <- ludh[,2:3]
ludo <- ludo[,2:3]

ludh$type <- "Habitats Dir."
ludo$type <- "Birds Dir."

### Double check potential full overlap - same name "Dudelange Haard"

# Slight difference
st_area(ludo[ludo$SITENAME=="Dudelange Haard",])
st_area(ludh[ludh$SITENAME=="Dudelange Haard",])

# Sliver in the southwesternmost part of the protected area
ggplot() + 
  geom_sf(data=ludh[ludh$SITENAME=="Dudelange Haard",], fill=alpha("green", 0.99)) +
  geom_sf(data=ludo[ludo$SITENAME=="Dudelange Haard",], fill=alpha("black", 0.99))

# Solution: rename with HD and BD
ludo[ludo$SITENAME=="Dudelange Haard",]$SITENAME <- "Dudelange Haard (BD)"
ludh[ludh$SITENAME=="Dudelange Haard",]$SITENAME <- "Dudelange Haard (HD)"

###### Load ZPIN data

zpin <- st_read(dsn=paste0("/vsizip/", ENVIPATH,"zpin-declarees.zip"),options = enc.opt.z)
zpin <- st_zm(zpin)
# ERROR: at least 1 geometry not valid
zpin <- st_make_valid(zpin)
# ERROR: A\r\n and 1 instead of A
# Correction
zpin[107,"SOUSZONE"] <- "A"
zpin[120, "SOUSZONE"] <- "A"

# Problem Laangmuer
ggplot() + geom_sf(data=zpin[zpin$NOM=="Laangmuer",])

# zpin$NATCODE <- gsub(" ", "", zpin$NATCODE, fixed = TRUE)
# which(zpin$NATCODE=="")
zpin <- zpin[order(zpin$PRIE_ID),]
zpin_temp <- zpin %>%   group_by(PRIE_ID) %>%   summarise(SITECODE= first(PRIE_ID), SITENAME=first(NOM), geometry = st_union(geometry))

# Checkpoint
ggplot() + geom_sf(data=zpin_temp[zpin_temp$SITENAME=="Laangmuer",])

zpin <- zpin_temp
zpin <- zpin[,c("SITECODE","SITENAME")]
zpin$SITECODE <- as.character(zpin$SITECODE)

zpin$type <- "National"

###### Combine
prot.areas <- rbind(ludh, ludo, zpin)